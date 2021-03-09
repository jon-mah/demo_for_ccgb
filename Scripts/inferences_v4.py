"""
Uses dadi and fitdadi to infer the DFE of a given synonynmous sfs.

JCM 201907011
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy
import dadi
import Selection
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


class DemographicAndDFEInference():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def ExistingBreed(self, breed):
        """Return *breed* if existing, else raise ValueError.

        "AW" indicates arctic wolf data.
        "LB" indicates labrador data.
        "PG" indicates pug data.
        "TM" indicates tibetan mastiff data.
        "MD" indicates mixed dog data.
        "MW" indicates mixed wolf data.
        """
        if breed == "AW":
            return breed
        elif breed == "LB":
            return breed
        elif breed == "PG":
            return breed
        elif breed == "TM":
            return breed
        elif breed == "MD":
            return breed
        elif breed == "MW":
            return breed
        elif breed == "BC":
        	return breed
        else:
            raise ValueError('%s must specify a valid breed.' % breed)

    def inferDFEParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Given the number of individuals in population one and two, '
                'this script outputs a `*pops_file.txt` in the format '
                'specified for use by the python package, `easySFS.py`.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'syn_input_sfs', type=self.ExistingFile,
            help=('Synonynomous site-frequency spectrum from which the '
                  'demographic parameters should be inferred.'))
        parser.add_argument(
            'nonsyn_input_sfs', type=self.ExistingFile,
            help=('Nonsynonynomous site-frequency spectrum from which the '
                  'distribution of fitness effects should be inferred.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output `*inferred_demography.txt`.')
        parser.add_argument(
            '--breed', type=self.ExistingBreed,
            help=('The breed of organism from which the input `.vcf` is drawn '
                  'from. Must be "AW", for arctic wolves, "LB" for labrador '
                  'or "PG" for pug. The default is arctic wolves.'),
            default='AW')
        return parser

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

    def gamma_dist(self, mgamma, alpha, beta):
        """Define a gamma distribution.

        self: reference to this instance of a gamma distribution.
        mgamma: float which describes the mean value of this gamma
             distribution.
        alpha: shape parameter of the gamma distribution.
        beta: scale parameter of the gamma distribution.
        """
        return scipy.stats.distributions.gamma.pdf(-mgamma, alpha, scale=beta)

    def neugamma(self, mgamma, pneu, alpha, beta):
        """Define a neutral-gamma distribution.

        self: reference to this instance of a neutral-gamma distribution.
        mgamma: float which describes the mean value of this neutral-gamma
            distribution.
        pneu: proportion of elements which are assumed to be neutral, i.e.,
            equal to 0.
        alpha: shape parameter of the non-neutral elements of the
            neutral-gamma distribution.
        beta: scale parameter of the non-neutral elements of the
            neutral-gamma distribution.
        """
        mgamma = -mgamma
        # Assume anything with gamma < 1e-4 is neutral
        if (0 <= mgamma) and (mgamma < 1e-4):
            return pneu / (1e-4) + (1 - pneu) * self.gamma_dist(
                -mgamma, alpha, beta)
        else:
            return self.gamma_dist(-mgamma, alpha, beta) * (1 - pneu)

    def mixunif(self, mgamma, p1, p2, p3, p4, p5):
		mgamma = -mgamma
		b0 = 0.; b1 = 1.; b2 = 10.; b3 = 100.;b4 = 1000.;b5=100000
		if ((mgamma >=b0) and (mgamma < b1)):
			h = p1/(b1-b0)
		elif ((mgamma >=b1) and (mgamma <= b2)):
			h = p2/(b2-b1)
		elif ((mgamma >=b2) and (mgamma <= b3)):
			h = p3/(b3-b2)
		elif ((mgamma >=b3) and (mgamma <= b4)):
			h = p4/(b4-b3)
		elif ((mgamma >=b4) and (mgamma <= b5)):
			h = p5/(b5-b4)
		else:
			h = 0
		return h

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.inferDFEParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        syn_input_sfs = args['syn_input_sfs']
        nonsyn_input_sfs = args['nonsyn_input_sfs']
        outprefix = args['outprefix']
        breed = args['breed']

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # create output directory if needed
        outdir = os.path.dirname(args['outprefix'])
        if outdir:
            if not os.path.isdir(outdir):
                if os.path.isfile(outdir):
                    os.remove(outdir)
                os.mkdir(outdir)

        # Output files: logfile
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        inferred_demography = \
            '{0}{1}inferred_demography.txt'.format(
                args['outprefix'], underscore)
        inferred_DFE = \
            '{0}{1}inferred_DFE.txt'.format(args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, inferred_demography, inferred_DFE]
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

        # Construct initial Spectrum object from input synonymous sfs.
        syn_data = dadi.Spectrum.from_file(syn_input_sfs)
        syn_ns = syn_data.sample_sizes  # Number of samples.
        pts_l = [80, 100, 120]

        # Optomize parameters for this model.
        # First set parameter bounds for optimization
        upper_bound = [8, 3]
        lower_bound = [1e-4, 0]

        initial_guess = [0.5, 0.1]
        with open(inferred_demography, 'w') as f:
            f.write('Beginning with demographic inference.\n')
            max_likelihood = -1e25
            for i in range(10):
                # Start at initial guess
                p0 = initial_guess
                # Randomly perturb parameters before optimization.
                p0 = dadi.Misc.perturb_params(
                    p0, fold=1, upper_bound=upper_bound,
                    lower_bound=lower_bound)
                # Make the extrapolating version of demographic model function.
                func_ex = dadi.Numerics.make_extrap_log_func(self.two_epoch)
                logger.info(
                    'Beginning optimization with guess, {0}.'.format(p0))
                popt = dadi.Inference.optimize_log_lbfgsb(
                    p0=p0, data=syn_data, model_func=func_ex, pts=pts_l,
                    lower_bound=lower_bound, upper_bound=upper_bound,
                    verbose=len(p0), maxiter=100)
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
                'Maximum multinomial log composite likelihood: {0}.\n'.format(
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
        logger.info('Beginning DFE inference.')
        nonsyn_data = dadi.Spectrum.from_file(nonsyn_input_sfs)
        nonsyn_ns = nonsyn_data.sample_sizes

        demog_params = best_params

        Lsyn_dict = {'AW': 5320115, 'LB': 5043978, 'PG': 5270603,
                     'TM': 2693499, 'BC': 5205136, 'MW': 4015625, 'MD':3820358}

        Lsyn = Lsyn_dict[breed]  # Length of synonymous sites.
        u = 5.38E-09
        u_exon = u * 1.25  # Mutation rate of dog exons.
        Na = theta_syn / (4 * u_exon * Lsyn)

        max_s = 0.5
        max_gam = max_s * 2 * Na

        pts_l = [1200, 1400, 1600]
        spectra = Selection.spectra(demog_params, nonsyn_ns,
                                    self.two_epoch_sel,
                                    pts_l=pts_l, int_bounds=(1e-5, max_gam),
                                    Npts=300, echo=True, mp=True)

#mixed_unif
        mixunif_vec = numpy.frompyfunc(self.mixunif, 6, 1)
        def consfunc(x, *args):
        	return 1-sum(x)

        initial_guess = [0.20,0.20,0.20,0.20,0.20]
        lower_bound = [0, 0, 0,0,0]
        upper_bound = [1, 1, 1,1,1]
        mixunif_max_likelihoods = []
        mixunif_guesses = dict()
        for i in range(3):
            p0_mixunif = initial_guess
            p0_mixunif = dadi.Misc.perturb_params(p0_mixunif, lower_bound=lower_bound, upper_bound=upper_bound)
            logger.info('Beginning optimization with guess, {0}.'.format(p0_mixunif))
            popt = numpy.copy(Selection.optimize_cons(p0_mixunif, nonsyn_data,
                                                     spectra.integrate,
                                                     mixunif_vec,
                                                     theta_nonsyn,
                                                     lower_bound=lower_bound,
                                                     upper_bound=upper_bound,
                                                     verbose=len(p0_mixunif),
                                                     maxiter=25, constraint=consfunc))
            logger.info('Finished optimization, results are {0}.'.format(popt))
            mixunif_max_likelihoods.append(popt[0])
            mixunif_guesses[popt[0]] = popt


## GAMMA

        BETAinit = max_gam / 3
        initial_guess = [0.09, BETAinit]
        upper_beta = 10 * max_gam
        lower_bound = [1e-3, 0]
        upper_bound = [1, upper_beta]

        gamma_max_likelihoods = []
        gamma_guesses = dict()
        for i in range(3):
            p0 = initial_guess
            p0 = dadi.Misc.perturb_params(p0, lower_bound=lower_bound,
                                          upper_bound=upper_bound)
            logger.info('Beginning optimization with guess, {0}.'.format(p0))
            popt = numpy.copy(Selection.optimize_log(p0, nonsyn_data,
                                                     spectra.integrate,
                                                     self.gamma_dist,
                                                     theta_nonsyn,
                                                     lower_bound=lower_bound,
                                                     upper_bound=upper_bound,
                                                     verbose=len(p0),
                                                     maxiter=25))
            logger.info('Finished optomization, results are {0}.'.format(popt))

            gamma_max_likelihoods.append(popt[0])
            gamma_guesses[popt[0]] = popt

        neugamma_vec = numpy.frompyfunc(self.neugamma, 4, 1)

## NEU GAMMA

        initial_guess = [0.999999999, 0.09, BETAinit]
        lower_bound = [0, 1e-3, 1e-2]
        upper_bound = [1, 1, upper_beta]
        neugamma_max_likelihoods = []
        neugamma_guesses = dict()
        for i in range(3):
            p0_neugamma = initial_guess
            p0_neugamma = dadi.Misc.perturb_params(p0_neugamma,
                                                   lower_bound=lower_bound,
                                                   upper_bound=upper_bound)
            logger.info('Beginning optimization with guess, {0}.'.format(
                p0_neugamma))
            popt = numpy.copy(Selection.optimize_log(p0_neugamma, nonsyn_data,
                                                     spectra.integrate,
                                                     neugamma_vec,
                                                     theta_nonsyn,
                                                     lower_bound=lower_bound,
                                                     upper_bound=upper_bound,
                                                     verbose=len(p0_neugamma),
                                                     maxiter=25))
            logger.info('Finished optimization, results are {0}.'.format(popt))
            neugamma_max_likelihoods.append(popt[0])
            neugamma_guesses[popt[0]] = popt        

## neutral
        neutral_vec = numpy.frompyfunc(self.neugamma, 4, 1)
        initial_guess = [1, 0.09, BETAinit]
        lower_bound = [1, 1e-3, 1e-2]
        upper_bound = [1, 1, upper_beta]
        neutral_max_likelihoods = []
        neutral_guesses = dict()
        for i in range(3):
            p0_neutral = initial_guess
            p0_neutral = dadi.Misc.perturb_params(p0_neutral,
                                                  lower_bound=lower_bound,
                                                  upper_bound=upper_bound)
            logger.info('Beginning optimization with guess, {0}.'.format(
                p0_neutral))
            popt = numpy.copy(Selection.optimize_log(p0_neutral, nonsyn_data,
                                                     spectra.integrate,
                                                     neutral_vec,
                                                     theta_nonsyn,
                                                     lower_bound=lower_bound,
                                                     upper_bound=upper_bound,
                                                     verbose=len(p0_neutral),
                                                     maxiter=1))
            logger.info('Finished optimization, results are {0}.'.format(popt))
            neutral_max_likelihoods.append(popt[0])
            neutral_guesses[popt[0]] = popt

        logger.info('Finished DFE inference.')

        gamma_max_likelihoods.sort()
        neugamma_max_likelihoods.sort()
        mixunif_max_likelihoods.sort()
        neutral_max_likelihoods.sort()

        logger.info('Integrating expected site-frequency spectrum.')

        logger.info('Outputing results.')

        with open(inferred_DFE, 'w') as f:
            f.write('Assuming a gamma-distributed DFE...\n')
            f.write('Outputting best 5 MLE estimates.\n')
            for i in range(3):
                best_popt = gamma_guesses[gamma_max_likelihoods[i]]
                expected_sfs = spectra.integrate(
                    best_popt[1], self.gamma_dist, theta_nonsyn)
                f.write(
                    'The population-scaled best-fit parameters: {0}.\n'.format(
                        best_popt))
                # Divide output scale parameter by 2 * N_a
                f.write(
                    'The non-scaled best-fit parameters: '
                    '[{0}, array({1})].\n'.format(
                        best_popt[0],
                        numpy.divide(best_popt[1], numpy.array([1, 2 * Na]))))
                f.write('The expected SFS is: {0}.\n\n'.format(expected_sfs))
                
                
            f.write('Assuming a neutral-gamma-distributed DFE...\n')
            f.write('Outputting best 5 MLE estimates.\n')
            for i in range(3):
                best_popt_neugamma = neugamma_guesses[
                    neugamma_max_likelihoods[i]]
                expected_sfs_neugamma = spectra.integrate(
                    best_popt_neugamma[1], neugamma_vec, theta_nonsyn)
                f.write(
                    'The population-scaled best-fit parameters: {0}.\n'.format(
                        best_popt_neugamma))
                # Divide output scale parameter by 2 * N_a
                f.write(
                    'The non-scaled best-fit parameters: '
                    '[{0}, array({1})].\n'.format(
                        best_popt_neugamma[0],
                        numpy.divide(
                            best_popt_neugamma[1],
                            numpy.array([1, 1, 2 * Na]))))
                f.write('The expected SFS is: {0}.\n\n'.format(
                    expected_sfs_neugamma))
                    
            f.write('Assuming a mixed-unif-distributed DFE...\n')
            f.write('Outputting best 5 MLE estimates.\n')
            for i in range(3):
                best_popt_mixunif = mixunif_guesses[
                    mixunif_max_likelihoods[i]]
                expected_sfs_mixunif = spectra.integrate(
                    best_popt_mixunif[1], mixunif_vec, theta_nonsyn)
                f.write(
                    'The population-scaled best-fit parameters: {0}.\n'.format(
                        best_popt_mixunif))
                f.write(
                    'The non-scaled best-fit parameters: '
                    '[{0}, array({1})].\n'.format(
                        best_popt_mixunif[0],
                        numpy.divide(
                            best_popt_mixunif[1],
                            numpy.array([1, 1, 1, 1,1]))))
                f.write('The expected SFS is: {0}.\n\n'.format(
                    expected_sfs_mixunif))

                    
            # f.write('Assuming a neutral-distributed DFE...\n')
            # f.write('Outputting best 5 MLE estimates.\n')
            # for i in range(25):
            #     best_popt_neutral = neutral_guesses[
            #         neutral_max_likelihoods[i]]
            #     expected_sfs_neutral = spectra.integrate(
            #         best_popt_neutral[1], neutral_vec, theta_nonsyn)
            #     f.write(
            #         'The population-scaled best-fit parameters: {0}.\n'.format(
            #             best_popt_neutral))
            #     # Divide output scale parameter by 2 * N_a
            #     f.write(
            #         'The non-scaled best-fit parameters: '
            #         '[{0}, array({1})].\n'.format(
            #             best_popt_neutral[0],
            #             numpy.divide(
            #                 best_popt_neutral[1],
            #                 numpy.array([1, 1, 2 * Na]))))
            #     f.write('The expected SFS is: {0}.\n\n'.format(
            #         expected_sfs_neutral))

        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    DemographicAndDFEInference().main()

