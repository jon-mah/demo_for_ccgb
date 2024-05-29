"""
Compute summary statistics for a given input SFS.

JCM 20230404
"""


import sys
import os
import logging
import time
import argparse
import warnings
import random

import numpy
import itertools
import glob
import pandas as pd
import config
from utils import parse_midas_data, diversity_utils
from utils import clade_utils, parse_HMP_data
from utils import gene_diversity_utils
import numpy as np
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
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


class ReturnSummaryStatisticsForPeter():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def computeSFSParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Computes a downsampled SFS for a given species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_syn_sfs', type=self.ExistingFile,
            help=('Synonymous SFS for given species.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser


    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computeSFSParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        input_syn_sfs = args['input_syn_sfs']

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
        logfile = '{0}{1}return_summary_statistics.log'.format(
            args['outprefix'], underscore, e)
        to_remove = [logfile, sfs_dataframe,
                     empirical_syn_sfs, empirical_nonsyn_sfs]
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

        logger.info('Loading in given input file.')
        syn_data = dadi.Spectrum.from_file(syn_input_sfs)

        logger.info("Computing Watterson's Theta for input file.")
        thetaW = syn_data.Watterson_theta()
        logger.info("Watterson's Theta for input file is {}.".format(str(thetaW)))

        logger.info("Computing pi per base-pair")
        pi = syn_data.pi()
        logger.info("Pi per base pair for input file is {}.".format(str(pi)))

        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ReturnSummaryStatisticsForPeter().main()
