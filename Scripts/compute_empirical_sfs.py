"""
Computes the empirical SFS from a given input `tsv` file.

JCM 20230225
"""


import sys
import os
import logging
import time
import argparse
import warnings
import random

import numpy
import pandas as pd
import numpy
# import dadi


class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class ComputeEmpiricalSFS():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def computeEmpiricalSFSParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Computes a downsampled SFS for a given species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_tsv', type=str,
            help=('Input tab-separated-values file.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser


    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computeEmpiricalSFSParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        input_tsv = args['input_tsv']
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
        empirical_sfs = \
           '{0}{1}original_empirical_sfs.txt'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}compute_empirical_sfs.log'.format(
            args['outprefix'], underscore)
        to_remove = [logfile, empirical_sfs]
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

        sfs_dict = {}

        input_tsv_file = open(input_tsv, 'r')
        for line in input_tsv_file:
            allele_frequency = line.split('\t')[5]
            if allele_frequency == 'BAC':
                pass
            else:
                allele_frequency = int(allele_frequency)
                if allele_frequency in sfs_dict.keys():
                    sfs_dict[allele_frequency] += 1
                else:
                    sfs_dict[allele_frequency] = 0


        max_bin = max(sfs_dict.keys())
        sfs_array = []
        for i in range(max_bin + 1):
            if i not in sfs_dict.keys():
                sfs_array.append(0)
            else:
                sfs_array.append(sfs_dict[i])

        syn_data = dadi.Spectrum(data=sfs_array)
        syn_data.to_file(empirical_sfs)
        logger.info('Finished downsampling.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ComputeEmpiricalSFS().main()
