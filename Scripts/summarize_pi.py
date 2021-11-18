"""
Summarizes the average pi values per sample across species and populations.

JCM 20211118
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

class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class ComputePi():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def computePiParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Given computed average pi values, summarizes across '
                'species and sample.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_csv', type=self.ExistingFile,
            help=('File containing average pi values per sample in .csv '
                  'format'))
        parser.add_argument(
            'species', type=str,
            help=('String describing the species being analyzed.'))
        parser.add_argument(
            'cohort', type=str,
            help=('String describing the cohort from which the data '
                  'was sequenced.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computePiParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        input_csv = args['input_csv']
        outprefix = args['outprefix']
        species = args['species']
        cohort = args['cohort']

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # Output files: logfile, snp_matrix.csv
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        output_matrix = \
            '{0}{1}summarized_pi.csv'.format(
                args['outprefix'], underscore)

        # open post-processed MIDAS output

        with open(output_matrix, 'a') as f:
            input_file = open(input_csv, 'r')
            pi_values = input_file.readlines()[1]
            pi_values = pi_values.split(', ')[1:]
            num_samples = len(pi_values)
            for item in pi_values:
                string = str(species) + ', '
                string += str(cohort) + ', ' + str(item) + ', ' + str(num_samples)
                f.write(string + '\n')

        print(species)
        print(cohort)
        print(pi_values)
        print(num_samples)

if __name__ == '__main__':
    ComputePi().main()
