"""
Uses dadi and fitdadi to infer demography and DFE.

JCM 20220630
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy
import dadi
import dadi.DFE as DFE
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
            'nonsyn_input_sfs', type=self.ExistingFile,
            help=('Nonsynonynomous site-frequency spectrum from which the '
                  'distribution of fitness effects should be inferred.'))
        parser.add_argument(
            '--mask_singletons', dest='mask_singletons',
            help=('Boolean flag for masking singlestons in Spectrum.'),
            action='store_true')
        parser.add_argument(
            '--mask_doubletons', dest='mask_doubletons',
            help=('Boolean flag for masking doublestons in Spectrum.'),
            action='store_true')
        parser.set_defaults(mask_singletons=False)
        parser.set_defaults(mask_doubletons=False)
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

    def growth_sel(self, params, ns, pts):
        """Exponential growth beginning some time ago.

        This method incorporates a gamma parameter.

        params = (nu, T, gamma)
            nu: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
            gamma: Parameter tuple describing a gama distribution.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nu, T, gamma = params  # Define given parameters.
        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx, gamma=gamma)  # Define initial phi.

        def nu_func(t): return numpy.exp(numpy.log(nu) * t / T)  # Exp growth.
        # Integrate
        phi = dadi.Integration.one_pop(phi, xx, T, nu_func, gamma=gamma)

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

    def bottlegrowth_sel(self, params, ns, pts):
        """Instantaneous size change followed by exponential growth.

        This method incorporates a gamma parameter.

        params = (nuB, nuF, T, gamma)
            nuB: Ratio of population size after instantaneous change to ancient
                population size.
            nuF: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
            gamma: Parameter tuple describing a gamma distribution
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nuB, nuF, T, gamma = params  # Define given parameters.

        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx, gamma=gamma)  # Define initial phi

        # Exponential growth function
        def nu_func(t): return nuB * numpy.exp(numpy.log(nuF / nuB) * t / T)
        # Integrate.
        phi = dadi.Integration.one_pop(phi, xx, T, nu_func, gamma=gamma)

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

    def three_epoch_sel(self, params, ns, pts):
        """Define a three-epoch demography.

        This method incorporates a gamma parameter.

        params = (nuB, nuF, TB, TF, gamma)
            nuB: Ratio of bottleneck population size to ancient
                population size.
            nuF: Ratio of contemporary to ancient population size.
            TB: Length of bottleneck, in units of 2 * N_a.
            TF: Time since bottleneck recovery, in units of 2 * N_a.
            gamma: Parameter tuple describing a gamma distribution.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        nuB, nuF, TB, TF, gamma = params  # Define given parameters.

        xx = dadi.Numerics.default_grid(pts)  # Define likelihood surface.
        phi = dadi.PhiManip.phi_1D(xx, gamma=gamma)  # Define initial phi.
        # Integrate 1 to 2.
        phi = dadi.Integration.one_pop(phi, xx, TB, nuB, gamma=gamma)
        # Integrate 2 to 3.
        phi = dadi.Integration.one_pop(phi, xx, TF, nuF, gamma=gamma)

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

    def one_epoch_sel(self, params, ns, pts):
        """Define a one-epoch demography.

        This method incorporates a gamma parameter.

        params = (nu, T)
            nu: Ratio of contemporary to ancient population size.
            T: Time in the past at which size change occured,
                in units of 2*N_a.
            gamma: Parameter tuple describing a gamma distribution.
        ns = (n1, )
            n1: Number of samples in resulting Spectrum object.
        pts: Number of grid points to use in integration.
        """
        xx = dadi.Numerics.default_grid(pts)
        phi = dadi.PhiManip.phi_1D(xx)

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
        nonsyn_input_sfs = args['nonsyn_input_sfs']
        outprefix = args['outprefix']
        mask_singletons = args['mask_singletons']
        mask_doubletons = args['mask_doubletons']

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # create output directory if needed
        outdir = os.path.dirname(args['outprefix'])
        if outdir:
            if not os.path.isdir(outdir):
                if os.path.isfile(outdir):
                    os.remove(outdir)
                os.mkdir(outdir)

        # Output files: logfile, *demography.txt, inferred_DFE.txt
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        exponential_growth_demography = \
            '{0}{1}exponential_growth_demography.txt'.format(
                args['outprefix'], underscore)
        two_epoch_demography = \
            '{0}{1}two_epoch_demography.txt'.format(
                args['outprefix'], underscore)
        bottleneck_growth_demography = \
            '{0}{1}bottleneck_growth_demography.txt'.format(
                args['outprefix'], underscore)
        three_epoch_demography = \
            '{0}{1}three_epoch_demography.txt'.format(
                args['outprefix'], underscore)
        one_epoch_demography = \
           '{0}{1}one_epoch_demography.txt'.format(
                args['outprefix'], underscore)
        inferred_DFE = \
           '{0}{1}inferred_DFE.txt'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, exponential_growth_demography,
                     two_epoch_demography,
                     bottleneck_growth_demography,
                     three_epoch_demography,
                     one_epoch_demography, inferred_DFE]
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
        if mask_singletons:
            syn_data.mask[1] = True
        if mask_doubletons:
            syn_data.mask[2] = True
        syn_ns = syn_data.sample_sizes  # Number of samples.
        pts_l = [1200, 1400, 1600]

        # Optomize parameters for these models.
        # model_list = ['two_epoch', 'three_epoch', 'exponential_growth',
        #               'bottleneck_growth', 'one_epoch']
        model_list = ['two_epoch']
        model_LL_dict = {} # Track best log likelihood of each model
        model_params_dict = {} # Track best params of each model
        for model in model_list:
            if model == 'exponential_growth':
                # Allow for growth or decay
                upper_bound = [80, 0.15]
                lower_bound = [0, 0]
                # 25 initial guesses
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
                demography_file = exponential_growth_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.growth)
                logger.info('Beginning demographic inference for exponential '
                            'growth demographic model.')
            elif model == 'two_epoch':
                # Allow for growth or decay
                upper_bound = [80, 0.15]
                lower_bound = [0, 0]
                # 25 initial guesses
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
                demography_file = two_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.two_epoch)
                logger.info('Beginning demographic inference for two-epoch '
                            'demographic model.')
            elif model == 'bottleneck_growth':
                # Allow for growth or decay
                upper_bound = [80, 80, 0.15]
                lower_bound = [0, 0, 0]
                # 25 initial guesses
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
                demography_file = bottleneck_growth_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.bottlegrowth)
                logger.info('Beginning demographic inference for bottleneck + '
                            'growth demographic model.')
            elif model == 'three_epoch':
                # Allow for growth or decay
                upper_bound = [80, 80, 0.15, 0.15]
                lower_bound = [0.0, 0.0, 0.0, 0.0]
                # 25 initial guesses
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
                demography_file = three_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.three_epoch)
                logger.info('Beginning demographic inference for three-epoch '
                            'demographic model.')
            elif model == 'one_epoch':
                # No demographic change
                upper_bound = [80]
                lower_bound = [0]
                # 25 initial guesses
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
                demography_file = one_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.snm)
                logger.info('Beginning demographic inference for one-epoch '
                            'demographic model.')
            with open(demography_file, 'w') as f:
                max_likelihood = -1e25 # Assume impossibly low likelihood
                for i in range(3):
                    # Start at initial guess i from one of 25
                    p0 = initial_guesses[i]
                    # Randomly perturb parameters before optimization.
                    p0 = dadi.Misc.perturb_params(
                        p0, fold=1, upper_bound=upper_bound,
                        lower_bound=lower_bound)
                    logger.info(
                        'Beginning optimization with guess, {0}.'.format(p0))
                    # Fit MLE demographic model
                    popt = dadi.Inference.optimize_log_lbfgsb(
                        p0=p0, data=syn_data, model_func=func_ex, pts=pts_l,
                        lower_bound=lower_bound,
                        upper_bound=upper_bound,
                        verbose=len(p0), maxiter=5)
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
                    # Compute theta
                    theta = dadi.Inference.optimal_sfs_scaling(
                        non_scaled_spectrum, syn_data)
                    logger.info(
                        'Optimal value of theta: {0}.'.format(theta))
                    # Update best MLE based on this run
                    if multinomial_ll_non_scaled_spectrum > max_likelihood:
                        best_params = popt
                        best_non_scaled_spectrum = non_scaled_spectrum
                        max_likelihood = multinomial_ll_non_scaled_spectrum
                        theta_syn = theta
                best_scaled_spectrum = theta_syn * best_non_scaled_spectrum
                # Nonsyn is based on Kim 2017
                theta_nonsyn = theta_syn * 2.14
                # Compute poisson log likelihood
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
            model_params_dict[model] = best_params
            model_LL_dict[model] = max_likelihood

        logger.info('Finished demographic inference.')
        logger.info('Beginning DFE inference.')

        # Load in nonsynonymous data
        nonsyn_data = dadi.Spectrum.from_file(nonsyn_input_sfs)
        nonsyn_ns = nonsyn_data.sample_sizes

        # Use best fit demographic model for DFE inference
        best_model = max(model_LL_dict, key=model_LL_dict.get)

        one_epoch_bool = False
        if best_model == 'exponential_growth':
            func_sel = self.growth_sel
        elif best_model == 'two_epoch':
            func_sel = self.two_epoch_sel
        elif best_model == 'bottleneck_growth':
            func_sel = self.bottlegrowth_sel
        elif best_model == 'three_epoch':
            func_sel = self.three_epoch_sel
        else:
            # Best model is one-epoch
            # There is no way to incorporate selection in a one-epoch model
            # We use two epoch assumption instead
            one_epoch_bool = True
            best_model = 'two_epoch'
            func_sel = self.two_epoch_sel

        # Infer DFE based on best demographic parameters
        demog_params = model_params_dict[best_model]

        # Define standard mutation rates
        mu = 5E-10 # Change this as necessary
        Ne = theta_syn / 4 / mu
        if (best_model == 'exponential_growth' or best_model == 'two_epoch' \
            or best_model == 'one_epoch'):
            Na = Ne / float(demog_params[0])
        else:
            Na = Ne / float(demog_params[1])
        max_s = 0.5
        max_gam = max_s * 2 * Na

        # Redefine grid for MLE search
        pts_l = [1200, 1400, 1600]

        logger.info('Generating spectra object.')

        spectra = DFE.Cache1D(demog_params, nonsyn_ns,
            func_sel, pts=pts_l,
            gamma_bounds=(1e-5, 500),
            gamma_pts=100, verbose=True)

        # Assume gamma-distributed DFE
        BETAinit = 3 * max_gam
        initial_guess = [1e-3, BETAinit]
        upper_beta = 12 * max_gam
        lower_bound = [1e-3, 0]
        upper_bound = [100, upper_beta]

        # Track DFE inferences to find best MLE
        gamma_max_likelihoods = []
        gamma_guesses = dict()
        for i in range(3):
            p0 = initial_guess
            # Randomly perturb around initial guess
            p0 = dadi.Misc.perturb_params(p0, lower_bound=lower_bound,
                                          upper_bound=upper_bound)
            logger.info(
                'Beginning optimization with guess, {0}.'.format(p0))
            # MLE search for DFE
            popt = numpy.copy(dadi.Inference.optimize_log(p0, nonsyn_data, spectra.integrate, pts=None,
                      func_args=[DFE.PDFs.gamma, theta_nonsyn],
                      lower_bound=lower_bound, upper_bound=upper_bound,
                      verbose=len(best_params), maxiter=5, multinom=True))
            logger.info(
                'Finished optomization, results are {0}.'.format(popt))
            gamma_max_likelihoods.append(popt[0])
            gamma_guesses[popt[0]] = popt

        logger.info('Finished DFE inference.')

        # Sort likelihoods
        gamma_max_likelihoods.sort(reverse=True)

        logger.info('Integrating expected site-frequency spectrum.')

        logger.info('Outputting results.')

        with open(inferred_DFE, 'w') as f:
            if one_epoch_bool:
                f.write('The best demographic model is one-epoch.\n')
                f.write('Take these inferences with a grain of salt.\n')
            else:
                f.write('Best demographic model: {0}.\n'.format(best_model))
            f.write('Assuming a gamma-distributed DFE...\n')
            f.write('Outputting MLE estimates in order.\n')
            for i in range(25):
                best_popt = gamma_guesses[gamma_max_likelihoods[i]]
                # Compute model SFS under inferred DFE
                expected_sfs = spectra.integrate(
                    best_popt, None, DFE.PDFs.gamma, theta_nonsyn, None)
                f.write('The population-scaled '
                        'best-fit parameters: {0}.\n'.format(best_popt))
                # Divide output scale parameter by 2 * N_a
                f.write(
                    'The non-scaled best-fit parameters: '
                    '[{0}, array({1})].\n'.format(
                        best_popt[0],
                        numpy.divide(best_popt[1],
                                     numpy.array([1, 2 * Na]))))
                f.write('The expected SFS is: {0}.\n\n'.format(
                    expected_sfs))
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    DemographicInference().main()
