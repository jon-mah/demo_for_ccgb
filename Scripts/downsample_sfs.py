"""
Downsamples a given SFS by projecting to a smaller bin count.

JCM 20230323
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

    def downsampleSFSParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Computes a downsampled SFS for a given species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_sfs', type=self.ExistingFile,
            help='Input SFS to be downsampled.')
        parser.add_argument(
            'sample_size', type=int,
            help='The sample size that you want to downsample to.')
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.downsampleSFSParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        input_sfs = args['input_sfs']
        sample_size = args['sample_size']
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
        downsampled_sfs = \
           '{0}{1}downsampled_sfs.txt'.format(
                args['outprefix'], underscore, species)
        logfile = '{0}{1}downsampling.log'.format(
            args['outprefix'], underscore, species)
        to_remove = [logfil, downsampled_sfs]
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

        logger.info('Parsing input SFS.')
        input_spectrum =  dadi.Spectrum.from_file(input_sfs)

        print(input_spectrum)

        output_spectrum = input_spectrum.project([sample_size])

        print(output_spectrum)

        output_spectrum.to_file(downsampled_sfs)

        logger.info('Formatting output SFSs.')
        dadi_syn_dict = dadi.Misc.make_data_dict(output_syn_matrix)
        syn_data = dadi.Spectrum.from_data_dict(dadi_syn_dict, ['BAC'], [sample_size], polarized=False)
        dadi_nonsyn_dict = dadi.Misc.make_data_dict(output_nonsyn_matrix)
        nonsyn_data = dadi.Spectrum.from_data_dict(dadi_nonsyn_dict, ['BAC'], [sample_size], polarized=False)
        syn_data.to_file(downsampled_syn_sfs)
        nonsyn_data.to_file(downsampled_nonsyn_sfs)
        logger.info('Finished downsampling.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ComputeDownSampledSFS().main()
