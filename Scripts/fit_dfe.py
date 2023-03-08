"""
Uses dadi and fitdadi to infer DFE.

JCM 20230308
"""


import sys
import os
import logging
import time
import argparse
import warnings

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


class DFEInference():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def fitDFEParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Given inferred demographic model parameters, this script '
                'infers a maximum-likelihood distriubtion of fitness '
                'effects.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'syn_input_sfs', type=self.ExistingFile,
            help=('Synonynomous site-frequency spectrum from which the '
                  'demographic parameters were inferred.'))
        parser.add_argument(
            'nonsyn_input_sfs', type=self.ExistingFile,
            help=('Nonsynonynomous site-frequency spectrum from which the '
                  'distribution of fitness effects should be inferred.'))
        parser.add_argument(
            'input_demography', type=self.ExistingFile,
            help=('Inferred demographic parameters output by Dadi.'))
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

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.fitDFEParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        syn_input_sfs = args['syn_input_sfs']
        nonsyn_input_sfs = args['nonsyn_input_sfs']
        outprefix = args['outprefix']
        input_demography = args['input_demography']
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
        syn_data = dadi.Spectrum.from_file(syn_input_sfs)
        nonsyn_dad = dadi.Spectrum.from_file(nonsyn_input_sfs)
        if mask_singletons:
            syn_data.mask[1] = True
        if mask_doubletons:
            syn_data.mask[2] = True
        syn_ns = syn_data.sample_sizes # Number of samples.
        nonsyn_ns = nonsyn_data.sample_sizes # Number of samples

        logger.info('Loading input demography')
        with open(input_demography, 'r') as f:
            lines = [line for line in f]

        for line in lines:
            if 'Best fit parameters:' in line:
                demog_params = str(line.split(': ')[1])[:-1]
                demog_params = as.literal_eval(demog_params)
        print(demog_params)


        pts_l = [1200, 1400, 1600]


        # theta_nonsyn = theta_syn * 2.31
        logger.info('Integrating expected site-frequency spectrum.')
        # model_sfs = spectra.integrate(
        #    popt, None, DFE.PDFs.gamma, theta_nonsyn, None)

        logger.info('Outputting results.')

        # with open(inferred_DFE, 'w') as f:
        #     f.write('Assuming a gamma-distributed DFE...\n')
        #     f.write('Outputting MLE estimates in order.\n')
        #     for i in range(25):
        #         best_popt = gamma_guesses[gamma_max_likelihoods[i]]
        #        # Compute model SFS under inferred DFE
        #         expected_sfs = spectra.integrate(
        #             best_popt, None, DFE.PDFs.gamma, theta_nonsyn, None)
        #         f.write('The population-scaled '
        #                'best-fit parameters: {0}.\n'.format(best_popt))
        #        # Divide output scale parameter by 2 * N_a
        #         f.write(
        #             'The non-scaled best-fit parameters: '
        #            '[{0}, array({1})].\n'.format(
        #                best_popt[0],
        #                numpy.divide(best_popt[1],
        #                              numpy.array([1, 2 * Na]))))
        #        f.write('The expected SFS is: {0}.\n\n'.format(
        #            expected_sfs))
        logger.info('Pipeline executed succesfully.')

if __name__ == '__main__':
    DFEInference().main()
