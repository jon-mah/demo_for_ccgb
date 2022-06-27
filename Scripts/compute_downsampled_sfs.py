"""
Summarizes the average pi values per sample across species and populations.

JCM 20220516
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy
import bz2
import pandas as pd
from utils import parse_midas_data, diversity_utils, clade_utils
import numpy
import gzip
import dadi
import scipy.stats.distributions
import scipy.integrate
import scipy.optimize


class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class ComputeDownSampledSFS():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def computeDownSampledSFSParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Computes a downsampled SFS for a given species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'species', type=str,
            help=('String describing the species being analyzed.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def load_substitution_rate_map(self, species):
    # This definition is called whenever another script downstream uses the output of this data.
        data_directory = os.path.expanduser("/u/project/ngarud/Garud_lab/metagenomic_fastq_files/HMP1-2/data")
        substitution_rate_directory = '%s/substitution_rates/' % data_directory
        intermediate_filename_template = '%s%s.txt.gz'

        intermediate_filename = intermediate_filename_template % (substitution_rate_directory, species)

        substitution_rate_map = {}

        if not os.path.isfile(intermediate_filename):
            return substitution_rate_map

        file = gzip.open(intermediate_filename,"r")
        file.readline() # header
        for line in file:
            items = line.split(",")
            if items[0].strip()!=species:
                continue

            record_strs = [", ".join(['Species', 'Sample1', 'Sample2', 'Type', 'Num_muts', 'Num_revs', 'Num_mut_opportunities', 'Num_rev_opportunities'])]

            sample_1 = items[1].strip()
            sample_2 = items[2].strip()
            type = items[3].strip()
            num_muts = float(items[4])
            num_revs = float(items[5])
            num_mut_opportunities = float(items[6])
            num_rev_opportunities = float(items[7])

            num_changes = num_muts+num_revs
            num_opportunities = num_mut_opportunities+num_rev_opportunities

            sample_pair = (sample_1, sample_2)

            if type not in substitution_rate_map:
                substitution_rate_map[type] = {}

            substitution_rate_map[type][sample_pair] = (num_muts, num_revs, num_mut_opportunities, num_rev_opportunities)

        return substitution_rate_map

    def calculate_mutrev_matrices_from_substitution_rate_map(self, substitution_rate_map, type, allowed_samples=[]):
        # Rewritten to preserve order of allowed samples
        # If allowed samples contains things that are not in DB, it returns zero opportunities

        total_sample_set = set([])
        for sample_1, sample_2 in substitution_rate_map[type].keys():
            total_sample_set.add(sample_1)
            total_sample_set.add(sample_2)

        if len(allowed_samples)==0:
            allowed_samples = list(sorted(total_sample_set))

        # allows us to go from sample name to idx in allowed samples (to preserve order)
        sample_idx_map = {allowed_samples[i]:i for i in xrange(0,len(allowed_samples))}

        mut_difference_matrix = numpy.zeros((len(allowed_samples), len(allowed_samples)))*1.0
        rev_difference_matrix = numpy.zeros_like(mut_difference_matrix)

        mut_opportunity_matrix = numpy.zeros_like(mut_difference_matrix)
        rev_opportunity_matrix = numpy.zeros_like(mut_difference_matrix)

        for sample_pair in substitution_rate_map[type].keys():

            sample_i = sample_pair[0]
            sample_j = sample_pair[1]

            if not ((sample_i in sample_idx_map) and (sample_j in sample_idx_map)):
                continue

            i = sample_idx_map[sample_i]
            j = sample_idx_map[sample_j]

            num_muts, num_revs, num_mut_opportunities, num_rev_opportunities = substitution_rate_map[type][sample_pair]

            mut_difference_matrix[i,j] = num_muts
            rev_difference_matrix[i,j] = num_revs

            mut_opportunity_matrix[i,j] = num_mut_opportunities
            rev_opportunity_matrix[i,j] = num_rev_opportunities

        return allowed_samples, mut_difference_matrix, rev_difference_matrix, mut_opportunity_matrix, rev_opportunity_matrix

    def calculate_matrices_from_substitution_rate_map(self, substitution_rate_map, type, allowed_samples=[]):
        # once the map is loaded, then we can compute rate matrices in this definition (so, it relies on the previous def)

        samples, mut_difference_matrix, rev_difference_matrix, mut_opportunity_matrix, rev_opportunity_matrix = self.calculate_mutrev_matrices_from_substitution_rate_map( substitution_rate_map, type, allowed_samples)

        difference_matrix = mut_difference_matrix+rev_difference_matrix
        opportunity_matrix = mut_opportunity_matrix+rev_opportunity_matrix

        return samples, difference_matrix, opportunity_matrix

    def snm(self, notused, ns, pts):
        """Return a standard neutral model.

        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi.

        # Construct Spectrum object.
        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs

    def two_epoch(self, params, ns, pts):
        """Define a two-epoch demography, i.e., an instantaneous size change.

        params = (nu, T)
            nu: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nu, T = params  # Define given parameters.
        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi.

        phi = dadi.Integration.one_pop(phi, xx, T, nu)  # Integrate.

        # Construct Spectrum object.
        fs = dadi.Spectrum.from_phi(phi, ns, (xx,))
        return fs

    def two_epoch_sel(self, params, ns, pts):
        """Define a two-epoch demography, i.e., an instantaneous size change.

        This method incorporates a gamma parameter.

        params = (nu, T, gamma)
            nu: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
            gamma: Parameter tuple describing a gamma distribution.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nu, T, gamma = params  # Define given parameters.
        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx, gamma=gamma)  # Define initial phi

        phi = dadi.Integration.one_pop(phi, xx, T, nu, gamma=gamma)

        # Construct Spectrum object.
        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs

    def growth(self, params, ns, pts):
        """Exponential growth beginning some time ago.

        params = (nu, T)
            nu: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nu, T = params  # Define given parameters.
        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi.

        def nu_func(t): return numpy.exp(numpy.log(nu) * t / T)  # Exp growth.
        phi = dadi.Integration.one_pop(phi, xx, T, nu_func)  # Integrate.

        # Construct Spectrum object.
        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs

    def bottlegrowth(self, params, ns, pts):
        """Instantaneous size change followed by exponential growth.

        params = (nuB, nuF, T)
            nuB: Ratio of population size after instantaneous change to ancient
                population size.
            nuF: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nuB, nuF, T = params  # Define given parameters.

        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi

        # Exponential growth function
        def nu_func(t): return nuB * numpy.exp(numpy.log(nuF / nuB) * t / T)

        phi = dadi.Integration.one_pop(phi, xx, T, nu_func)  # Integrate.

        # Construct Spectrum object.
        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs

    def three_epoch(self, params, ns, pts):
        """Define a three-epoch demography.

        params = (nuB, nuF, TB, TF)
            nuB: Ratio of bottleneck population size to ancient
                population size.
            nuF: Ratio of contemporary to ancient population size.
            TB: Length of bottleneck, in units of 2 * N_a.
            TF: Time since bottleneck recovery, in units of 2 * N_a.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nuB, nuF, TB, TF = params  # Define given parameters.

        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi.

        phi = dadi.Integration.one_pop(phi, xx, TB, nuB)  # Integrate 1 to 2.
        phi = dadi.Integration.one_pop(phi, xx, TF, nuF)  # Integrate 2 to 3.

        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs

    def four_epoch(self, params, ns, pts):
        """Define a four-epoch demography.

        params = (Na, Nb, Nc, Ta, Tb, Tc)
            Na: ratio of population size between epoch 1 and 2.
            Nb: ratio of population size between epoch 2 and 3.
            Nc: ratio of population size between epoch 3 and 4.
            Ta: Bottleneck length between epoch 1 and 2, in units of 2 * N_a.
            Tb: Length of bottleneck between epoch 2 and 3,
                in units of 2 * N_a.
            Tc: Length of bottleneck between epoch 3 and 4,
                in units of 2 * N_a.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        Na, Nb, Nc, Ta, Tb, Tc = params  # Define given parameters.

        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi.

        # Integrate epochs.
        phi = dadi.Integration.one_pop(phi, xx, Ta, Na)  # Integrate 1 to 2.
        phi = dadi.Integration.one_pop(phi, xx, Tb, Nb)  # Integrate 2 to 3.
        phi = dadi.Integration.one_pop(phi, xx, Tc, Nc)  # Integrate 3 to 4.

        # Construct spectrum object.
        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs

    def one_epoch(self, params, ns, pts):
        """Define a one-epoch demography.
        params = (nu, T)
            nu: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
	phi = dadi.PhiManip.phi_1D(xx)  # Define initial phi.

        # Construct spectrum object
        fs = dadi.Spectrum.from_phi(phi, ns, (xx, ))
        return fs


    def one_pop(phi, xx, T, nu=1, gamma=0, h=0.5, theta0=1.0, initial_t=0,
                frozen=False, beta=1):
        """
        Integrate a 1-dimensional phi foward.

        phi: Initial 1-dimensional phi
        xx: Grid upon (0,1) overwhich phi is defined.

        nu, gamma, and theta0 may be functions of time.
        nu: Population size
        gamma: Selection coefficient on *all* segregating alleles
        h: Dominance coefficient. h = 0.5 corresponds to genic selection.
        Heterozygotes have fitness 1+2sh and homozygotes have fitness 1+2s.
        theta0: Propotional to ancestral size. Typically constant.
        beta: Breeding ratio, beta=Nf/Nm.

        T: Time at which to halt integration
        initial_t: Time at which to start integration. (Note that this only
            matters if one of the demographic parameters is a function of time.)

        frozen: If True, population is 'frozen' so that it does not change.
            In the one_pop case, this is equivalent to not running the
            integration at all.
        """
        phi = phi.copy()

        # For a one population integration, freezing means just not integrating.
        if frozen:
            return phi

        if T - initial_t == 0:
            return phi
        elif T - initial_t < 0:
            raise ValueError('Final integration time T (%f) is less than '
                            'intial_time (%f). Integration cannot be run '
                            'backwards.' % (T, initial_t))

        vars_to_check = (nu, gamma, h, theta0, beta)
        if numpy.all([numpy.isscalar(var) for var in vars_to_check]):
            return _one_pop_const_params(phi, xx, T, nu, gamma, h, theta0,
                                     initial_t, beta)

        nu_f = Misc.ensure_1arg_func(nu)
        gamma_f = Misc.ensure_1arg_func(gamma)
        h_f = Misc.ensure_1arg_func(h)
        theta0_f = Misc.ensure_1arg_func(theta0)
        beta_f = Misc.ensure_1arg_func(beta)

        current_t = initial_t
        nu, gamma, h = nu_f(current_t), gamma_f(current_t), h_f(current_t)
        beta = beta_f(current_t)
        dx = numpy.diff(xx)
        while current_t < T:
            dt = _compute_dt(dx,nu,[0],gamma,h)
            this_dt = min(dt, T - current_t)

            # Because this is an implicit method, I need the *next* time's params.
            # So there's a little inconsistency here, in that I'm estimating dt
            # using the last timepoints nu,gamma,h.
            next_t = current_t + this_dt
            nu, gamma, h = nu_f(next_t), gamma_f(next_t), h_f(next_t)
            beta = beta_f(next_t)
            theta0 = theta0_f(next_t)

            if numpy.any(numpy.less([T,nu,theta0], 0)):
                raise ValueError('A time, population size, migration rate, or '
                             'theta0 is < 0. Has the model been mis-specified?')
            if numpy.any(numpy.equal([nu], 0)):
                raise ValueError('A population size is 0. Has the model been '
                             'mis-specified?')

            _inject_mutations_1D(phi, this_dt, xx, theta0)
            # Do each step in C, since it will be faster to compute the a,b,c
            # matrices there.
            phi = int_c.implicit_1Dx(phi, xx, nu, gamma, h, beta, this_dt,
                                 use_delj_trick=use_delj_trick)
            current_t = next_t
        return phi

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computeDownSampledSFSParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        species = args['species']

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # Output files: logfile, snp_matrix.csv
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        output_matrix = \
            '{0}{1}{2}_downsampled_sfs.csv'.format(
                args['outprefix'], underscore, species)
        exponential_growth_demography = \
            '{0}{1}{2}_exponential_growth_demography.txt'.format(
                args['outprefix'], underscore, species)
        two_epoch_demography = \
            '{0}{1}{2}_two_epoch_demography.txt'.format(
                args['outprefix'], underscore, species)
        bottleneck_growth_demography = \
            '{0}{1}{2}_bottleneck_growth_demography.txt'.format(
                args['outprefix'], underscore, species)
        three_epoch_demography = \
            '{0}{1}{2}_three_epoch_demography.txt'.format(
                args['outprefix'], underscore, species)
        one_epoch_demography = \
           '{0}{1}{2}_one_epoch_demography.txt'.format(
                args['outprefix'], underscore, species)
        logfile = '{0}{1}log.log'.format(
            args['outprefix'], underscore, species)
        to_remove = [logfile, exponential_growth_demography,
                     two_epoch_demography, bottleneck_growth_demography,
                     three_epoch_demography]
        for f in to_remove:
            if os.path.isfile(f):
                os.remove(f)

        # Set up to log everything to logfile.
        logging.shutdown()
        logging.captureWarnings(True)
        logging.basicConfig(
            format='%(asctime)s - %(levelname)s - %(message)s',
            level=logging.INFO)
        logger = logging.getLogger(prog)
        warning_logger = logging.getLogger("py.warnings")
        logfile_handler = logging.FileHandler(logfile)
        logger.addHandler(logfile_handler)
        warning_logger.addHandler(logfile_handler)
        formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - %(message)s')
        logfile_handler.setFormatter(formatter)
        logger.setLevel(logging.INFO)

        # print some basic information
        logger.info('Beginning execution of {0} in directory {1}\n'.format(
            prog, os.getcwd()))
        logger.info('Progress is being logged to {0}\n'.format(logfile))
        logger.info('Parsed the following arguments:\n{0}\n'.format(
            '\n'.join(['\t{0} = {1}'.format(*tup) for tup in args.items()])))

        # Load core genes
        core_genes = parse_midas_data.load_core_genes(species)

        # Default parameters
        alpha = 0.5 # Confidence interval range for rate estimates
        low_divergence_threshold = 5e-04
        min_change = 0.8

        snp_samples = diversity_utils.calculate_haploid_samples(species)
        snp_samples = [s.decode("utf-8")  for s in snp_samples]

        # Pre-computed substituion rates for species
        substitution_rate_map = self.load_substitution_rate_map(species)
        dummy_samples, snp_difference_matrix, snp_opportunity_matrix = self.calculate_matrices_from_substitution_rate_map(substitution_rate_map, 'core', allowed_samples=snp_samples)
        snp_samples = numpy.array(dummy_samples)
        substitution_rate = snp_difference_matrix*1.0/(snp_opportunity_matrix+(snp_opportunity_matrix==0))


        coarse_grained_idxs, coarse_grained_cluster_list = clade_utils.cluster_samples(substitution_rate, min_d=low_divergence_threshold)

        coarse_grained_samples = snp_samples[coarse_grained_idxs]
        clade_sets = clade_utils.load_manual_clades(species)

        clade_idxss = clade_utils.calculate_clade_idxs_from_clade_sets(coarse_grained_samples, clade_sets)

        clade_sizes = numpy.array([clade_idxs.sum() for clade_idxs in clade_idxss])

        largest_clade_samples = coarse_grained_samples[ clade_idxss[clade_sizes.argmax()] ]


        # Load allele counts map for this species
        snp_samples, allele_counts_map, passed_sites_map, final_line_number = parse_midas_data.parse_snps(species, allowed_variant_types=['1D','2D','3D','4D'], allowed_samples=largest_clade_samples, allowed_genes=core_genes, debug=True)

        allowed_variant_types = '4D'
        allowed_genes = core_genes
        pi_min_k = 4
        lower_threshold = 0.2
        upper_threshold = 0.8

        # Pooled counts: for reference
        # synonymous_counts, synonymous_weights = diversity_utils.calculate_pooled_counts(allele_counts_map, passed_sites_map, allowed_variant_types = set(['4D']), allowed_genes=core_genes,pi_min_k=4)

        # Dive into internals of calculatealle_pooled_counts
        allowed_variant_types = '4D'
        allowed_genes = core_genes
        pi_min_k = 4
        lower_threshold = 0.2
        upper_threshold = 0.8

        with open(output_matrix, 'w+') as f:
            header = 'Bacl' + '\t' + 'Dsim' + '\t' + 'Allele1' + '\t' + 'BAC' + '\t' + 'Allele2' + '\t' + 'BAC'+ '\t' + 'SNPid'
            f.write(header + '\n')
        for gene_name in allele_counts_map:
            for variant_type in allele_counts_map[gene_name].keys():
                if variant_type not in allowed_variant_types:
                    continue
                allele_counts = allele_counts_map[gene_name][variant_type]['alleles']
            if len(allele_counts)==0:
                continue
            allele_counts = allele_counts
            # passed_sites_matrix = boolean of whether a site passes or note
            # genotype_matrix = 1 or 0, 1 --> alt allele, 0 --> reference or false in passed sites_matrix
            # output alt ref fail depth
            # For each site, you want to know (1) how many samples have alternate allele
            # (freq>0.8, (2) how many samples have reference allele (freq>0.8),
            # and (3) how many samples have 0 coverage
            # site number --> gene + position within gene
            genotype_matrix, passed_sites_matrix = diversity_utils.calculate_consensus_genotypes(
                allele_counts, lower_threshold, upper_threshold)
            num_sites = passed_sites_matrix.shape[0]
            num_samples = passed_sites_matrix.shape[1]
            with open(output_matrix, 'a+') as f:
                for i in range(0, num_sites):
                    site_id = gene_name + '.site.' + str(i+1)
                    alts = int(sum(genotype_matrix[i]))
                    fails = int(sum(numpy.invert(passed_sites_matrix[i]))) # HEY LOOK AT ME
                    refs = int(len(genotype_matrix[i]) - fails)
                    string = '-A-' + '\t' + '---' + '\t' + 'A' + '\t'
                    string = string + str(refs) + '\t' + 'G' + '\t' + str(alts)
                    string = string + '\t' + site_id + '\n'
                    f.write(string)

        dadi_dict = dadi.Misc.make_data_dict(output_matrix)
        syn_data = dadi.Spectrum.from_data_dict(dadi_dict, ['BAC'], [25], polarized=False)
        syn_data.mask[1] = True
        syn_ns = syn_data.sample_sizes  # Number of samples.
        pts_l = [1200, 1400, 1600]

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # create output directory if needed
        outdir = os.path.dirname(args['outprefix'])
        if outdir:
            if not os.path.isdir(outdir):
                if os.path.isfile(outdir):
                    os.remove(outdir)
                os.mkdir(outdir)



        # Optomize parameters for this model.
        # First set parameter bounds for optimization
        model_list = ['one_epoch', 'two_epoch']
        for model in model_list:
            if model == 'exponential_growth':
                upper_bound = [80, 0.15]
                lower_bound = [0, 0]
                initial_guesses = []
                initial_guesses.append([0.1, 0.00001])
                initial_guesses.append([0.2, 0.00001])
                initial_guesses.append([0.3, 0.00001])
                initial_guesses.append([0.4, 0.00001])
                initial_guesses.append([0.5, 0.00001])
                initial_guesses.append([0.6, 0.00001])
                initial_guesses.append([0.7, 0.00001])
                initial_guesses.append([0.8, 0.00001])
                initial_guesses.append([0.9, 0.00001])
                initial_guesses.append([1, 0.00001])
                initial_guesses.append([1.25, 0.00001])
                initial_guesses.append([1.5, 0.00001])
                initial_guesses.append([1.75, 0.00001])
                initial_guesses.append([2, 0.00001])
                initial_guesses.append([2.25, 0.00001])
                initial_guesses.append([2.5, 0.00001])
                initial_guesses.append([2.75, 0.00001])
                initial_guesses.append([3, 0.00001])
                initial_guesses.append([3.33, 0.00001])
                initial_guesses.append([3.66, 0.00001])
                initial_guesses.append([4, 0.00001])
                initial_guesses.append([5, 0.00001])
                initial_guesses.append([6, 0.00001])
                initial_guesses.append([7, 0.00001])
                initial_guesses.append([8, 0.00001])
                file = exponential_growth_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.growth)
                logger.info('Beginning demographic inference for exponential '
                            'growth demographic model.')
            elif model == 'two_epoch':
                upper_bound = [80, 0.15]
                lower_bound = [0, 0]
                initial_guesses = []
                initial_guesses.append([0.1, 0.000001])
                initial_guesses.append([0.2, 0.000001])
                initial_guesses.append([0.3, 0.000001])
                initial_guesses.append([0.4, 0.000001])
                initial_guesses.append([0.5, 0.000001])
                initial_guesses.append([0.6, 0.000001])
                initial_guesses.append([0.7, 0.000001])
                initial_guesses.append([0.8, 0.000001])
                initial_guesses.append([0.9, 0.000001])
                initial_guesses.append([1, 0.000001])
                initial_guesses.append([1.25, 0.000001])
                initial_guesses.append([1.5, 0.000001])
                initial_guesses.append([1.75, 0.000001])
                initial_guesses.append([2, 0.000001])
                initial_guesses.append([2.25, 0.000001])
                initial_guesses.append([2.5, 0.000001])
                initial_guesses.append([2.75, 0.000001])
                initial_guesses.append([3, 0.000001])
                initial_guesses.append([3.33, 0.000001])
                initial_guesses.append([3.66, 0.000001])
                initial_guesses.append([4, 0.000001])
                initial_guesses.append([5, 0.000001])
                initial_guesses.append([6, 0.000001])
                initial_guesses.append([7, 0.000001])
                initial_guesses.append([8, 0.000001])
                file = two_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.two_epoch)
                logger.info('Beginning demographic inference for two-epoch '
                            'demographic model.')
            elif model == 'bottleneck_growth':
                upper_bound = [80, 80, 0.15]
                lower_bound = [0, 0, 0]
                initial_guess = [0.1, 0.1, 0.00001]
                initial_guesses = []
                initial_guesses.append([0.1, 0.1, 0.000001])
                initial_guesses.append([0.2, 0.2, 0.000001])
                initial_guesses.append([0.3, 0.3, 0.000001])
                initial_guesses.append([0.4, 0.4, 0.000001])
                initial_guesses.append([0.5, 0.5, 0.000001])
                initial_guesses.append([0.6, 0.6, 0.000001])
                initial_guesses.append([0.7, 0.7, 0.000001])
                initial_guesses.append([0.8, 0.8, 0.000001])
                initial_guesses.append([0.9, 0.9, 0.000001])
                initial_guesses.append([1, 1, 0.000001])
                initial_guesses.append([1.25, 1.25, 0.000001])
                initial_guesses.append([1.5, 1.5, 0.000001])
                initial_guesses.append([1.75, 1.75, 0.000001])
                initial_guesses.append([2, 2, 0.000001])
                initial_guesses.append([2.25, 2.25, 0.000001])
                initial_guesses.append([2.5, 2.5, 0.000001])
                initial_guesses.append([2.75, 2.75, 0.000001])
                initial_guesses.append([3, 3, 0.000001])
                initial_guesses.append([3.33, 3.33, 0.000001])
                initial_guesses.append([3.66, 3.66, 0.000001])
                initial_guesses.append([4, 4, 0.000001])
                initial_guesses.append([5, 5, 0.000001])
                initial_guesses.append([6, 6, 0.000001])
                initial_guesses.append([7, 7, 0.000001])
                initial_guesses.append([8, 8, 0.000001])
                file = bottleneck_growth_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.bottlegrowth)
                logger.info('Beginning demographic inference for bottleneck + '
                            'growth demographic model.')
            elif model == 'three_epoch':
                upper_bound = [80, 80, 0.15, 0.15]
                lower_bound = [0.0, 0.0, 0.0, 0.0]
                initial_guesses = []
                initial_guesses.append([0.1, 0.1, 0.000001, 0.000001])
                initial_guesses.append([0.2, 0.2, 0.000001, 0.000001])
                initial_guesses.append([0.3, 0.3, 0.000001, 0.000001])
                initial_guesses.append([0.4, 0.4, 0.000001, 0.000001])
                initial_guesses.append([0.5, 0.5, 0.000001, 0.000001])
                initial_guesses.append([0.6, 0.6, 0.000001, 0.000001])
                initial_guesses.append([0.7, 0.7, 0.000001, 0.000001])
                initial_guesses.append([0.8, 0.8, 0.000001, 0.000001])
                initial_guesses.append([0.9, 0.9, 0.000001, 0.000001])
                initial_guesses.append([1, 1, 0.000001, 0.000001])
                initial_guesses.append([1.25, 1.25, 0.000001, 0.000001])
                initial_guesses.append([1.5, 1.5, 0.000001, 0.000001])
                initial_guesses.append([1.75, 1.75, 0.000001, 0.000001])
                initial_guesses.append([2, 2, 0.000001, 0.000001])
                initial_guesses.append([2.25, 2.25, 0.000001, 0.000001])
                initial_guesses.append([2.5, 2.5, 0.000001, 0.000001])
                initial_guesses.append([2.75, 2.75, 0.000001, 0.000001])
                initial_guesses.append([3, 3, 0.000001, 0.000001])
                initial_guesses.append([3.33, 3.33, 0.000001, 0.000001])
                initial_guesses.append([3.66, 3.66, 0.000001, 0.000001])
                initial_guesses.append([4, 4, 0.000001, 0.000001])
                initial_guesses.append([5, 5, 0.000001, 0.000001])
                initial_guesses.append([6, 6, 0.000001, 0.000001])
                initial_guesses.append([7, 7, 0.000001, 0.000001])
                initial_guesses.append([8, 8, 0.000001, 0.000001])
                file = three_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.three_epoch)
                logger.info('Beginning demographic inference for three-epoch '
                            'demographic model.')
            elif model == 'one_epoch':
                upper_bound = [80]
                lower_bound = [0]
                initial_guesses = []
                initial_guesses.append([0.1])
                initial_guesses.append([0.2])
                initial_guesses.append([0.3])
                initial_guesses.append([0.4])
                initial_guesses.append([0.5])
                initial_guesses.append([0.6])
                initial_guesses.append([0.7])
                initial_guesses.append([0.8])
                initial_guesses.append([0.9])
                initial_guesses.append([1])
                initial_guesses.append([1.25])
                initial_guesses.append([1.5])
                initial_guesses.append([1.75])
                initial_guesses.append([2])
                initial_guesses.append([2.25])
                initial_guesses.append([2.5])
                initial_guesses.append([2.75])
                initial_guesses.append([3])
                initial_guesses.append([3.33])
                initial_guesses.append([3.66])
                initial_guesses.append([4])
                initial_guesses.append([5])
                initial_guesses.append([6])
                initial_guesses.append([7])
                initial_guesses.append([8])
                # initial_guess = [0.1]
                file = one_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.snm)
                logger.info('Beginning demographic inference for one-epoch '
                            'demographic model.')
            with open(file, 'w') as f:
                max_likelihood = -1e25
                for i in range(25):
                    # Start at initial guess
                    p0 = initial_guesses[i]
                    # Randomly perturb parameters before optimization.
                    p0 = dadi.Misc.perturb_params(
                        p0, fold=1, upper_bound=upper_bound,
                        lower_bound=lower_bound)
                    logger.info(
                        'Beginning optimization with guess, {0}.'.format(p0))
                    popt = dadi.Inference.optimize_log_lbfgsb(
                        p0=p0, data=syn_data, model_func=func_ex, pts=pts_l,
                        lower_bound=lower_bound,
                        upper_bound=upper_bound,
                        verbose=len(p0), maxiter=50)
                    logger.info(
                        'Finished optimization with guess, ' + str(p0) + '.')
                    logger.info('Best fit parameters: {0}.'.format(popt))
                    # Calculate the best-fit model allele-frequency spectrum.
                    # Note, this spectrum needs to be multiplied by "theta".
                    non_scaled_spectrum = func_ex(popt, syn_ns, pts_l)
                    # Likelihood of the data given the model AFS.
                    multinomial_ll_non_scaled_spectrum = \
                        dadi.Inference.ll_multinom(
                            model=non_scaled_spectrum, data=syn_data)
                    logger.info(
                        'Maximum log composite likelihood: {0}.'.format(
                            multinomial_ll_non_scaled_spectrum))
                    theta = dadi.Inference.optimal_sfs_scaling(
                        non_scaled_spectrum, syn_data)
                    logger.info(
                        'Optimal value of theta: {0}.'.format(theta))
                    if multinomial_ll_non_scaled_spectrum > max_likelihood:
                        best_params = popt
                        best_non_scaled_spectrum = non_scaled_spectrum
                        max_likelihood = multinomial_ll_non_scaled_spectrum
                        theta_syn = theta
                best_scaled_spectrum = theta_syn * best_non_scaled_spectrum
                theta_nonsyn = theta_syn * 2.14
                poisson_ll = dadi.Inference.ll(
                    model=best_scaled_spectrum, data=syn_data)
                f.write('Best fit parameters: {0}.\n'.format(best_params))
                f.write(
                    'Maximum multinomial log composite '
                    'likelihood: {0}.\n'.format(
                        max_likelihood))
                f.write(
                    'Maximum poisson log composite likelihood: {0}.\n'.format(
                        poisson_ll))
                f.write('Non-scaled best-fit model spectrum: {0}.\n'.format(
                    best_non_scaled_spectrum))
                f.write('Optimal value of theta_syn: {0}.\n'.format(theta_syn))
                f.write('Optimal value of theta_nonsyn: {0}.\n'.format(
                    theta_nonsyn))
                f.write('Scaled best-fit model spectrum: {0}.\n'.format(
                    best_scaled_spectrum))
        logger.info('Finished demographic inference.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ComputeDownSampledSFS().main()
