"""
Uses dadi and fitdadi to infer demography and DFE.

JCM 20210303
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

        This method incorporates a gamma parameter

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
        """Define a mixed-uniform distribution.

        self: reference to this instance of a neutral-gamma distribution.
        mgamma: float which describes the mean value of this neutral-gamma
            distribution.
        p1: proportion of sites with s between -1 and -0.1
        p2: proportion of sites with s between -0.1 and -0.01
        p3: proportion of sites with s between -0.01 and -0.001
        p4: proportion of sites with s between -0.001 and -0.0001
        p5: proportion of sites with s between -0.0001 and -0.00001
        """
        mgamma = -mgamma
        b0 = 0.
        b1 = 1.
        b2 = 10
        b3 = 100
        b4 = 1000
        b5 = 10000
        if ((mgamma >= b0) and (mgamma < b1)):
            height = p1 / (b1 - b0)
        elif ((mgamma >= b1) and (mgamma <= b2)):
            height = p2 / (b2 - b1)
        elif ((mgamma >= b2) and (mgamma <= b3)):
            height = p3 / (b3 - b2)
        elif ((mgamma >= b3) and (mgamma <= b4)):
            height = p4 / (b4 - b3)
        elif ((mgamma >= b4) and (mgamma <= b5)):
            height = p5 / (b5 - b4)
        else:
            height = 0
        return height

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

        fs = dadi.Spectrum.from_phi(phi, nx, (xx, ))
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

        # Output files: logfile
        # Remove demography output files if they already exist
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
        one_epoch_DFE = \
           '{0}{1}one_epoch_DFE.txt'.format(
                args['outprefix'], underscore)
        inferred_DFE = \
           '{0}{1}inferred_DFE.txt'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, exponential_growth_demography,
                     two_epoch_demography,
                     bottleneck_growth_demography,
                     three_epoch_demography,
                     one_epoch_demography]
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

        # Optomize parameters for this model.
        # First set parameter bounds for optimization
        model_list = ['exponential_growth', 'two_epoch', 'bottleneck_growth',
                      'three_epoch']
        model_LL_dict = {}
        model_params_dict = {}
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
                demography_file = exponential_growth_demography
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
                demography_file = two_epoch_demography
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
                demography_file = bottleneck_growth_demography
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
                demography_file = three_epoch_demography
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
                demography_file = one_epoch_demography
                func_ex = dadi.Numerics.make_extrap_log_func(self.snm)
                logger.info('Beginning demographic inference for one-epoch '
                            'demographic model.')
            with open(demography_file, 'w') as f:
                max_likelihood = -1e25
                for i in range(3):
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
            model_params_dict[model] = best_params
            model_LL_dict[model] = max_likelihood

        logger.info('Finished demographic inference.')
        logger.info('Beginning DFE inference.')

        # Load in nonsynonymous data
        nonsyn_data = dadi.Spectrum.from_file(nonsyn_input_sfs)
        nonsyn_ns = nonsyn_data.sample_sizes

        # Use best fit demographic model for DFE inference
        best_model = max(model_LL_dict, key=model_LL_dict.get)

        if best_model == 'exponential_growth':
            func_sel = self.growth_sel
        elif best_model == 'two_epoch':
            func_sel = self.two_epoch_sel
        elif best_model == 'bottleneck_growth':
            func_sel = self.bottlegrowth_sel
        elif best_model == 'three_epoch':
            func_sel = self.three_epoch_sel
        else:
            best_model == 'two_epoch'
            func_sel = self.two_epoch_sel

        demog_params = model_params_dict[best_model]

        mu = 5E-10
        Ne = theta_syn / 4 / mu
        if (best_model == 'exponential_growth' or best_model == 'two_epoch' or best_model == 'one_epoch'):
            Na = Ne / float(demog_params[0])
        else:
            Na = Ne / float(demog_params[1])
        max_s = 0.5
        max_gam = max_s * 2 * Na

        pts_l = [1200, 1400, 1600]

        logger.info('Generating spectra object.')

        spectra = Selection.spectra(demog_params, nonsyn_ns,
                                    func_sel,
                                    pts_l=pts_l,
                                    int_bounds=(1e-5, max_gam),
                                    Npts=750, echo=True, mp=True)

        # Assume gamma-distributed DFE
        BETAinit = 3 * max_gam
        initial_guess = [1e-3, BETAinit]
        upper_beta = 12 * max_gam
        lower_bound = [1e-3, 0]
        upper_bound = [100, upper_beta]

        gamma_max_likelihoods = []
        gamma_guesses = dict()
        for i in range(1):
            p0 = initial_guess
            p0 = dadi.Misc.perturb_params(p0, lower_bound=lower_bound,
                                          upper_bound=upper_bound)
            logger.info(
                'Beginning optimization with guess, {0}.'.format(p0))
            popt = numpy.copy(
                Selection.optimize_log(p0, nonsyn_data,
                                       spectra.integrate,
                                       self.gamma_dist,
                                       theta_nonsyn,
                                       lower_bound=lower_bound,
                                       upper_bound=upper_bound,
                                       verbose=len(p0),
                                       maxiter=5))
            logger.info(
                'Finished optomization, results are {0}.'.format(popt))

        gamma_max_likelihoods.append(popt[0])
        gamma_guesses[popt[0]] = popt

        neugamma_vec = numpy.frompyfunc(self.neugamma, 4, 1)

        initial_guess = [0.3, 0.09, BETAinit]
        lower_bound = [0, 0, 0]
        upper_bound = [1, 10, upper_beta]
        neugamma_max_likelihoods = []
        neugamma_guesses = dict()
        for i in range(1):
            p0_neugamma = initial_guess
            p0_neugamma = dadi.Misc.perturb_params(p0_neugamma,
                                                   lower_bound=lower_bound,
                                                   upper_bound=upper_bound)
            logger.info('Beginning optimization with guess, {0}.'.format(
                p0_neugamma))
            popt = numpy.copy(
                Selection.optimize_log(p0_neugamma, nonsyn_data,
                                       spectra.integrate,
                                       neugamma_vec,
                                       theta_nonsyn,
                                       lower_bound=lower_bound,
                                       upper_bound=upper_bound,
                                       verbose=len(p0_neugamma),
                                       maxiter=5))
            logger.info(
                'Finished optimization, results are {0}.'.format(popt))
            neugamma_max_likelihoods.append(popt[0])
            neugamma_guesses[popt[0]] = popt

        mixunif_vec = numpy.frompyfunc(self.mixunif, 6, 1)

        def constraint_func(x, *args):
            return 1-sum(x)

        initial_guess = [0.20, 0.20, 0.20, 0.20, 0.20]
        lower_bound = [0, 0, 0, 0, 0]
        upper_bound = [1, 1, 1, 1, 1]
        mixunif_max_likelihoods = []
        mixunif_guesses = dict()
        for i in range(1):
            p0_mixunif = initial_guess
            p0_mixunif = dadi.Misc.perturb_params(
                p0_mixunif, lower_bound=lower_bound,
                upper_bound=upper_bound)
            logger.info('Beginning optimization with guess, {0}.'.format(
                p0_mixunif))
            popt = numpy.copy(
                Selection.optimize_cons(p0_mixunif, nonsyn_data,
                                        spectra.integrate,
                                        mixunif_vec,
                                        theta_nonsyn,
                                        lower_bound=lower_bound,
                                        upper_bound=upper_bound,
                                        verbose=len(p0_mixunif),
                                        maxiter=5,
                                        constraint=constraint_function))
            logger.info(
                'Finished optimization, results are {0}.'.format(popt))
            mixunif_max_likelihoods.append(popt[0])
            mixunif_guesses[popt[0]] = popt

        logger.info('Finished DFE inference.')

        gamma_max_likelihoods.sort(reverse=True)
        neugamma_max_likelihoods.sort(reverse=True)
        mixunif_max_likelihoods.sort(reverse=True)

        logger.info('Integrating expected site-frequency spectrum.')

        logger.info('Outputing results.')

        with open(inferred_DFE, 'w') as f:
            f.write('Best demographic model:{0}.\n'.format(best_model))
            f.write('Assuming a gamma-distributed DFE...\n')
            f.write('Outputting best 5 MLE estimates.\n')
            for i in range(1):
                best_popt = gamma_guesses[gamma_max_likelihoods[i]]
                expected_sfs = spectra.integrate(
                    best_popt[1], self.gamma_dist, theta_nonsyn)
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

            f.write('Assuming a neutral-gamma-distributed DFE...\n')
            f.write('Outputting best 5 MLE estimates.\n')
            for i in range(1):
                best_popt_neugamma = neugamma_guesses[
                    neugamma_max_likelihoods[i]]
                expected_sfs_neugamma = spectra.integrate(
                    best_popt_neugamma[1], neugamma_vec, theta_nonsyn)
                f.write(
                    'The population-scaled best-fit '
                    'parameters: {0}.\n'.format(best_popt_neugamma))
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

            f.write('Assuming a mixed-uniform-distributed DFE...\n')
            f.write('Outputting best 5 MLE estimates.\n')
            for i in range(1):
                best_popt_mixunif = mixunif_guesses[
                    mixunif_max_likelihoods[i]]
                expected_sfs_mixunif = spectra.integrate(
                    best_popt_mixunif[1], mixunif_vec, theta_nonsyn)
                f.write(
                    'The population-scaled best-fit '
                    'parameters: {0}.\n'.format(best_popt_mixunif))
                f.write(
                    'The non-scaled best-fit parameters: '
                    '[{0}, array({1})].\n'.format(
                        best_popt_mixunif[0],
                        numpy.divide(
                            best_popt_mixunif[1],
                            numpy.array([1, 1, 1, 1, 1]))))
                f.write('The expected SFS is: {0}.\n\n'.format(
                    expected_sfs_mixunif))

        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    DemographicInference().main()
