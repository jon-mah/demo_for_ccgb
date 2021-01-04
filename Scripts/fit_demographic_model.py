"""
Uses dadi to infer the best-fit demographic model for given sfs.

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


class DemographicInference():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def fitDemographicModelParser(self):
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
            'outprefix', type=str,
            help='The file prefix for the output files')
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

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.fitDemographicModelParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        syn_input_sfs = args['syn_input_sfs']
        outprefix = args['outprefix']

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
        exponential_growth_demography = \
            '{0}{1}exponential_growth_demography.txt'.format(
                args['outprefix'], underscore)
        two_epoch_demography = \
            '{0}{1}two_epoch_demography.txt'.format(
                args['outprefix'], underscore)
        bottleneck_growth_demography = \
            '{0}{1}bottleneck_growth.txt'.format(
                args['outprefix'], underscore)
        three_epoch_demography = \
            '{0}{1}three_epoch_demography.txt'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, two_epoch_demography]
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
        pts_l = [800, 1000, 1200]

        # Optomize parameters for this model.
        # First set parameter bounds for optimization
        model_list = ['exponential_growth', 'two_epoch', 'bottleneck_growth',
                      'three_epoch']
        for model in model_list:
            if model is 'exponential_growth':
                upper_bound = [8, 3]
                lower_bound = [1e-4, 0]
                initial_guess = [0.5, 0.1]
                file = \
                    '{0}{1}exponential_growth_demography.txt'.format(
                        args['outprefix'], underscore)
                func_ex = dadi.Numerics.make_extrap_log_func(self.growth)
                logger.info('Beginning demographic inference for exponential '
                            'growth demographic model.')
            if model is 'two_epoch':
                upper_bound = [8, 3]
                lower_bound = [1e-4, 0]
                initial_guess = [0.5, 0.1]
                file = \
                    '{0}{1}two_epoch_demography.txt'.format(
                        args['outprefix'], underscore)
                func_ex = dadi.Numerics.make_extrap_log_func(self.two_epoch)
                logger.info('Beginning demographic inference for two-epoch '
                            'demographic model.')
            if model is 'bottleneck_growth':
                upper_bound = [8, 8, 3]
                lower_bound = [1e-4, 1e-4, 0]
                initial_guess = [0.5, 0.5, 0.1]
                file = \
                    '{0}{1}bottleneck_growth.txt'.format(
                        args['outprefix'], underscore)
                func_ex = dadi.Numerics.make_extrap_log_func(self.bottlegrowth)
                logger.info('Beginning demographic inference for bottleneck + '
                            'growth demographic model.')
            else:
                upper_bound = []
                lower_bound = []
                initial_guess = []
                file = \
                    '{0}{1}three_epoch_demography.txt'.format(
                        args['outprefix'], underscore)
                func_ex = dadi.Numerics.make_extrap_log_func(self.three_epoch)
                logger.info('Beginning demographic inference for three-epoch '
                            'demographic model.')
            with open(file, 'w') as f:
                max_likelihood = -1e25
                for i in range(5):
                    # Start at initial guess
                    p0 = initial_guess
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
                        verbose=len(p0), maxiter=25)
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
    DemographicInference().main()
