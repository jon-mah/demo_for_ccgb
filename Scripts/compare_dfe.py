"""
Performs a goodness-of-fit likelihood test for a given model nonsynonymous
SFS to a given reference nonsynonymous SFS.

JCM 20230707
"""


import sys
import os
import logging
import time
import argparse
import warnings
import re

import ast
import numpy as np
import nlopt
import dadi
import dadi.DFE as DFE
from dadi.DFE import *
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


class DFEComparison():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def neugamma(self, xx, params):
        """Return Neutral + Gamma distribution"""
        pneu, alpha, beta = params
        xx = np.atleast_1d(xx)
        out = (1-pneu)*DFE.PDFs.gamma(xx, (alpha, beta))
        # Assume gamma < 1e-4 is essentially neutral
        out[np.logical_and(0 <= xx, xx < 1e-4)] += pneu/1e-4
        # Reduce xx back to scalar if it's possible
        return np.squeeze(out)

    def extract_sfs_array(self, file_path):
        with open(file_path, 'r') as file:
            for line in file:
                if line.startswith('The best fit model SFS is:'):
                    sfs_line = line.strip().replace('--', '0')
                    print(sfs_line)
                    sfs_array = re.findall(r'\d+(?:\.\d+)?', sfs_line)
                    print(sfs_array)
                    sfs_array_float = [float(val) for val in sfs_array]
                    return sfs_array_float
        return None

    def compareDFEParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Given inferred demographic model parameters, this script '
                'infers a maximum-likelihood distriubtion of fitness '
                'effects.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'nonsyn_reference_sfs', type=self.ExistingFile,
            help=('Nonsynonynomous site-frequency spectrum from which the '
                  'distribution of fitness effects was inferred.'))
        parser.add_argument(
            'nonsyn_model_sfs', type=self.ExistingFile,
            help=('Nonsynonynomous site-frequency spectrum for which the '
                  'distribution of fitness effects should be fit.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.compareDFEParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        nonsyn_reference_sfs = self.extract_sfs_array(
            args['nonsyn_reference_sfs'])
        nonsyn_model_sfs = self.extract_sfs_array(
            args['nonsyn_model_sfs'])
        outprefix = args['outprefix']


        # Numpy options
        np.set_printoptions(linewidth=np.inf)

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
        inferred_DFE = \
           '{0}{1}inferred_DFE.txt'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, inferred_DFE]
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

        # Construct initial Spectrum object from input sfs's.
        nonsyn_reference_sfs  = dadi.Spectrum(nonsyn_reference_sfs).fold()
        nonsyn_model_sfs  = dadi.Spectrum(nonsyn_model_sfs).fold()
        loglik  = dadi.Inference.ll_multinom(nonsyn_model_sfs, nonsyn_reference_sfs)
        print(loglik)
        # scaling =  dadi.Inference.optimal_sfs_scaling(nonsyn_model_sfs, nonsyn_reference_sfs)
        # print(scaling)
        # nonsyn_model_sfs = nonsyn_model_sfs * scaling
        # print(nonsyn_model_sfs)
        reference_fit = dadi.Inference.ll_multinom(nonsyn_reference_sfs, nonsyn_reference_sfs)
        loglik = dadi.Inference.ll_multinom(nonsyn_model_sfs, nonsyn_reference_sfs)

        logger.info('Input reference SFS is: {}.'.format(nonsyn_reference_sfs))
        logger.info('Input model SFS is: {}.'.format(nonsyn_model_sfs))
        print(loglik)
        # nonsyn_data = dadi.Spectrum.from_file(nonsyn_reference_sfs).fold()
        # nonsyn_ns = nonsyn_data.sample_sizes # Number of samples

        logger.info('Pipeline executed succesfully.')

if __name__ == '__main__':
    DFEComparison().main()
