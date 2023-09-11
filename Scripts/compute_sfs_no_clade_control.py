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
import random

import numpy
import bz2
import pandas as pd
from utils import parse_midas_data, diversity_utils, clade_utils, parse_HMP_data
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

    def calculate_unique_samples(self, subject_sample_map, sample_list=[]):

        if len(sample_list)==0:
            sample_list = list(sorted(flatten_samples(subject_sample_map).keys()))

        # invert subject sample map
        sample_subject_map = {}
        for subject in subject_sample_map.keys():
            for sample in subject_sample_map[subject].keys():
                sample_subject_map[sample] = subject

        subject_idx_map = {}

        for i in xrange(0,len(sample_list)):
            sample = sample_list[i]
            if sample.endswith('c'):
                sample = sample[:-1]
            subject = sample_subject_map[sample]
            if not subject in subject_idx_map:
                subject_idx_map[subject] = i

        unique_idxs = numpy.zeros(len(sample_list),dtype=numpy.bool_)
        for i in subject_idx_map.values():
            unique_idxs[i]=True

        return


    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computeDownSampledSFSParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        species = args['species']
        random.seed(1)

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # create output directory if needed
        outdir = os.path.dirname(args['outprefix'])
        if outdir:
            if not os.path.isdir(outdir):
                if os.path.isfile(outdir):
                    os.remove(outdir)
                os.mkdir(outdir)

        # Output files: logfile, snp_matrix.csv
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        syn_output_matrix = \
            '{0}{1}downsampled_syn_sfs.csv'.format(
                args['outprefix'], underscore, species)
        empirical_syn_sfs = \
           '{0}{1}empirical_syn_sfs.txt'.format(
                args['outprefix'], underscore, species)
        nonsyn_output_matrix = \
            '{0}{1}downsampled_nonsyn_sfs.csv'.format(
                args['outprefix'], underscore, species)
        empirical_nonsyn_sfs = \
           '{0}{1}empirical_nonsyn_sfs.txt'.format(
                args['outprefix'], underscore, species)
        logfile = '{0}{1}downsampling.log'.format(
            args['outprefix'], underscore, species)
        to_remove = [logfile,
                     syn_output_matrix, empirical_syn_sfs,
                     nonsyn_output_matrix, empirical_nonsyn_sfs]
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
        subject_sample_map = parse_HMP_data.parse_subject_sample_map()
        core_genes = parse_midas_data.load_core_genes(species)

        # Default parameters
        alpha = 0.5 # Confidence interval range for rate estimates
        low_divergence_threshold = 5e-04
        min_change = 0.8

        snp_samples = diversity_utils.calculate_haploid_samples(species)
        snp_samples = snp_samples[self.calculate_unique_samples(subject_sample_map, snp_samples)]
        snp_samples = snp_samples.ravel()
        snp_samples = [s.decode("utf-8")  for s in snp_samples]

        # Pre-computed substituion rates for species
        subject_sample_map = parse_HMP_data.parse_subject_sample_map()
        substitution_rate_map = self.load_substitution_rate_map(species)
        dummy_samples, snp_difference_matrix, snp_opportunity_matrix = self.calculate_matrices_from_substitution_rate_map(substitution_rate_map, 'core', allowed_samples=snp_samples)
        snp_samples = numpy.array(dummy_samples)
        substitution_rate = snp_difference_matrix*1.0/(snp_opportunity_matrix+(snp_opportunity_matrix==0))

        #  coarse_grained_idxs, coarse_grained_cluster_list = clade_utils.cluster_samples(substitution_rate, min_d=low_divergence_threshold)

        # coarse_grained_samples = snp_samples[coarse_grained_idxs]
        # clade_sets = clade_utils.load_manual_clades(species)

        # clade_idxss = clade_utils.calculate_clade_idxs_from_clade_sets(coarse_grained_samples, clade_sets)

        # clade_sizes = numpy.array([clade_idxs.sum() for clade_idxs in clade_idxss])

        # largest_clade_samples = coarse_grained_samples[ clade_idxss[clade_sizes.argmax()] ]

        # Load allele counts map for this species
        # Pooled counts: for reference
        # synonymous_counts, synonymous_weights = diversity_utils.calculate_pooled_counts(allele_counts_map, passed_sites_map, allowed_variant_types = set(['4D']), allowed_genes=core_genes,pi_min_k=4)
        # Dive into internals of calculatealle_pooled_counts
        snp_samples, allele_counts_map, passed_sites_map, final_line_number = parse_midas_data.parse_snps(species, allowed_variant_types=['1D','2D','3D','4D'], allowed_samples=snp_samples, allowed_genes=core_genes, debug=False)
        allowed_variant_types = '4D'
        allowed_genes = core_genes
        pi_min_k = 4
        lower_threshold = 0.2
        upper_threshold = 0.8

        with open(syn_output_matrix, 'w+') as f:
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
            with open(syn_output_matrix, 'a+') as f:
                for i in range(0, num_sites):
                    site_id = gene_name + '.site.' + str(i+1)
                    alts = int(sum(genotype_matrix[i]))
                    fails = int(sum(numpy.invert(passed_sites_matrix[i])))
                    refs = int(len(genotype_matrix[i]) - fails - alts)
                    string = '-A-' + '\t' + '---' + '\t' + 'A' + '\t'
                    string = string + str(refs) + '\t' + 'G' + '\t' + str(alts)
                    string = string + '\t' + site_id + '\n'
                    f.write(string)

        # Now, for nonsynonymous mutations
        allowed_variant_types = '1D'
        with open(nonsyn_output_matrix, 'w+') as f:
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
            with open(nonsyn_output_matrix, 'a+') as f:
                for i in range(0, num_sites):
                    site_id = gene_name + '.site.' + str(i+1)
                    alts = int(sum(genotype_matrix[i]))
                    fails = int(sum(numpy.invert(passed_sites_matrix[i])))
                    refs = int(len(genotype_matrix[i]) - fails - alts)
                    string = '-A-' + '\t' + '---' + '\t' + 'A' + '\t'
                    string = string + str(refs) + '\t' + 'G' + '\t' + str(alts)
                    string = string + '\t' + site_id + '\n'
                    f.write(string)
        syn_sfs_dict = {}

        syn_input_tsv_file = open(syn_output_matrix, 'r')
        for line in syn_input_tsv_file:
            allele_frequency = line.split('\t')[5]
            if allele_frequency == 'BAC':
                pass
            else:
                allele_frequency = int(allele_frequency)
                if allele_frequency in syn_sfs_dict.keys():
                    syn_sfs_dict[allele_frequency] += 1
                else:
                    syn_sfs_dict[allele_frequency] = 0

        nonsyn_sfs_dict = {}

        nonsyn_input_tsv_file = open(nonsyn_output_matrix, 'r')
        for line in nonsyn_input_tsv_file:
            allele_frequency = line.split('\t')[5]
            if allele_frequency == 'BAC':
                pass
            else:
                allele_frequency = int(allele_frequency)
                if allele_frequency in nonsyn_sfs_dict.keys():
                    nonsyn_sfs_dict[allele_frequency] += 1
                else:
                    nonsyn_sfs_dict[allele_frequency] = 0

        max_bin = max(syn_sfs_dict.keys())
        syn_sfs_array = []
        for i in range(max_bin + 1):
            if i not in syn_sfs_dict.keys():
                syn_sfs_array.append(0)
            else:
                syn_sfs_array.append(syn_sfs_dict[i])

        max_bin = max(nonsyn_sfs_dict.keys())
        nonsyn_sfs_array = []
        for i in range(max_bin + 1):
            if i not in nonsyn_sfs_dict.keys():
                nonsyn_sfs_array.append(0)
            else:
                nonsyn_sfs_array.append(nonsyn_sfs_dict[i])

        syn_data = dadi.Spectrum(data=syn_sfs_array)
        syn_data.to_file(empirical_syn_sfs)
        nonsyn_data = dadi.Spectrum(data=nonsyn_sfs_array)
        nonsyn_data.to_file(empirical_nonsyn_sfs)
        logger.info('Finished downsampling.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ComputeDownSampledSFS().main()
