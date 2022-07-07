"""
Removes "N" entries from a given `.fastq.gz` file.

JCM 20220214
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy
import bz2
import pandas as pd
import gzip
import re

class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class removeNs():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def removeNsParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Given a specified `*snps_ref_freq.txt.bz2, this script '
                'computes average Pi across samples matrix for that species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_fastq_gz', type=self.ExistingFile,
            help=('String containing path to desired input file')),
        parser.add_argument(
            'output_fastq_gz', type=str,
            help=('String containing path to desired output file')),
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.removeNsParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        input_fastq_gz = args['input_fastq_gz']
        output_fastq_gz = args['output_fastq_gz']
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

        # Output files: logfile, snp_matrix.csv
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        # output_file = \
        #     '{0}{1}{2}.fastq.gz'.format(
        #         args['outprefix'], underscore, file_prefix)
        logfile = '{0}{1}compute_pi.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, output_fastq_gz]
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

        open post-processed MIDAS output
        # input_file = bz2.BZ2File(input_fastq_gz)
        with gzip.open(input_fastq_gz, 'rb') as input:
            lines = input.readlines()
            with gzip.open(output_fastq_gz, 'wb') as output:
                for i in range(0, len(lines)-3):
                   if lines[i+1]:
                        next_line = lines[i+1]
                        if re.search('N', next_line):
                            print('only N')
                        else:
                            print('not only N')
                            output.write(lines[i])
                            output.write(lines[i+1])
                            output.write(lines[i+2])
                            output.write(lines[i+3])

        # with gzip.open(input_fastq_gz, 'rb') as input:
        #     with gzip.open(output_fastq_gz, 'wb') as output:
        #         for line in input:
        #             output.write(line)

if __name__ == '__main__':
    removeNs().main()
