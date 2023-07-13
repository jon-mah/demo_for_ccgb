""" Finds the MLE DFE between two given SFSs.

JCM 20230713
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
from dadi.DFE import *
import scipy.stats.distributions
import scipy.integrate
import scipy.optimize
import matplotlib
matplotlib.use('Agg') # Must be before importing matplotlib.pyplot or pylab!
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import ticker, cm
import pandas as pd

class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class MidpointNormalize(matplotlib.colors.Normalize):
    def __init__(self, vmin=None, vmax=None, vcenter=None, clip=False):
        self.vcenter = vcenter
        super().__init__(vmin, vmax, clip)

    def __call__(self, value, clip=None):
        # I'm ignoring masked values and all kinds of edge cases to make a
        # simple example...
        # Note also that we must extrapolate beyond vmin/vmax
        x, y = [self.vmin, self.vcenter, self.vmax], [0, 0.5, 1.]
        return numpy.ma.masked_array(numpy.interp(value, x, y,
                                            left=-numpy.inf, right=numpy.inf))

    def inverse(self, value):
        y, x = [self.vmin, self.vcenter, self.vmax], [0, 0.5, 1]
        return numpy.interp(value, x, y, left=-numpy.inf, right=numpy.inf)

class CrossSpeciesDFEInferece():
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
            'input_demography_A', type=self.ExistingFile,
            help=('Best-fit demographic parameters for species A.'))
        parser.add_argument(
            'input_demography_B', type=self.ExistingFile,
            help=('Best-fit demographic parameters for species B.'))
        parser.add_argument(
            'input_nonsyn_sfs_A', type=self.ExistingFile,
            help=('Nonsynonymous SFS for species A.'))
        parser.add_argument(
            'input_nonsyn_sfs_B', type=self.ExistingFile,
            help=('Nonsynonymous SFS for species B.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files.')
        return parser

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

    def likelihood(self, spectra, theta, alpha, beta, nonsyn_data, func_ex, pts_l):
        p0 = [alpha, beta]
        theta_nonsyn = theta * 2.21
        popt = p0
        #popt = dadi.Inference.optimize_log_fmin(p0, nonsyn_data,
        #    spectra.integrate, pts=None,
        #    func_args=[DFE.PDFs.gamma, theta_nonsyn],
        #    verbose=len(sel_params), maxiter=0,
        #    multinom=False)
        model_sfs = spectra.integrate(
            popt, None, DFE.PDFs.gamma, theta_nonsyn, None)

        loglik = dadi.Inference.ll_multinom(
            model=non_scaled_spectrum, data=nonsyn_data)
        return(loglik)

    def read_input_demography(self, input_demography):
        with open(input_demography, 'r') as file:
            first_line = file.readline()
            start_idx = first_line.find('[')
            end_idx = first_line.find(']')
            params_str = first_line[start_idx+1:end_idx]
            if ',' in params_str:
                params = [float(param) for param in params_str.split(',')]
            else:
                params = [float(param) for param in params_str.split()]
        return params[0], params[1]

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.fitDemographicModelParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        input_demography_A = args['input_demography_A']
        input_demography_B = args['input_demography_B']
        demog_params_A = self.read_input_demography(
            input_demography_A)
        start_idx = input_demography_A.find("Analysis") + 9
        end_idx = input_demography_A.find("_downsampled")
        species_A = input_demography_A[start_idx:end_idx]
        demog_params_B = self.read_input_demography(
            input_demography_B)
        start_idx = input_demography_B.find("Analysis") + 9
        end_idx = input_demography_B.find("_downsampled")
        species_B = input_demography_B[start_idx:end_idx]
        nonsyn_sfs_A = args['input_nonsyn_sfs_A']
        nonsyn_sfs_B = args['input_nonsyn_sfs_B']

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
        likelihood_surface_A = \
            '{0}{1}likelihood_surface_{2}.csv'.format(
                args['outprefix'], underscore, species_A)
        likelihood_surface_A = \
            '{0}{1}likelihood_surface_{2}.csv'.format(
                args['outprefix'], underscore, species_B)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile]
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
        nonsyn_data_A = dadi.Spectrum.from_file(nonsyn_sfs_A).fold()
        nonsyn_data_B = dadi.Spectrum.from_file(nonsyn_sfs_B).fold()
        nonsyn_ns_A = nonsyn_data_A.sample_sizes  # Number of samples.
        nonsyn_ns_B = nonsyn_data_B.sample_sizes  # Number of samples.
        pts_l = [1200, 1400, 1600]

        # Construct demography from input params
        spectra_A = DFE.Cache1D(demog_params_A, nonsyn_ns_A, DFE.DemogSelModels.two_epoch,
                              pts=pts_l, gamma_bounds=(1e-5, 500), gamma_pts=100,
                              verbose=True, cpus=1)
        spectra_B  = DFE.Cache1D(demog_params_B, nonsyn_ns_B, DFE.DemogSelModels.two_epoch,
                              pts=pts_l, gamma_bounds=(1e-5, 500), gamma_pts=100,
                              verbose=True, cpus=1)

        # x = input_alpha  # Initial x value
        # y = input_beta  # Initial y value

        x = 0.02
        y = 5E4

        npts = 25
        x_range = numpy.linspace(0.01 * x, x *  3.99, npts)
        y_range = numpy.linspace(y * 0.01, x * 3.99, npts)

        X, Y = numpy.meshgrid(x_range, y_range)

        Z = numpy.empty((npts, npts))

        x_val = []
        y_val = []
        z_val = []
        for i in range(0, npts):
            for j in range(0, npts):
                Z[i, j] = self.likelihood(spectra_A, theta_A, x_range[i], y_range[j], nonsyn_data, pts_l)
                x_val.append(x_range[i])
                y_val.append(y_range[j])
                z_val.append(Z[i, j])
        df = pd.DataFrame({'X': x_val, 'Y': y_val, 'Z': z_val})
        df.to_csv(likelihood_surface_A)
        logger.info('Maximum likelihood computed to be: {0}.'.format(max_likelihood))
        logger.info('Maximum likelihood found at ({0}).'.format(best_params))
        logger.info('Finished plotting likelihood surface.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    CrossSpeciesDFEInferece().main()
