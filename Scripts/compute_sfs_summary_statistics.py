"""
Compute Watterson's Theta, expected heterozygosity, Tajima's D from SFS

JCM 20230906
"""


import sys
import os
import logging
import time
import argparse
import warnings
import random

import dadi
import numpy

class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class SFSSummaryStatistics():
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
                'Computes a downsampled SFS for a given species_string.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_sfs', type=self.ExistingFile,
            help='Input SFS to be downsampled.')
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
        output_csv = \
           '{0}{1}sfs_summary_statistics.csv'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}sfs_summary_statistics.log'.format(
            args['outprefix'], underscore)
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

        logger.info('Parsing input SFS.')
        input_spectrum =  dadi.Spectrum.from_file(input_sfs)

        logger.info('Computing summary statistics from SFS.')
        species_string = str(input_sfs)

        # Find the starting index after "Analysis/"
        start_index = species_string.find("Analysis/") + len("Analysis/")

        # Find the ending index before "_downsampled_14"
        end_index = species_string.find("_downsampled_14")

        # Extract the desired substring
        species = species_string[start_index:end_index]

        theta_w = str(input_spectrum.Watterson_theta())
        heterozygosity = str(input_spectrum.pi())
        tajima_d = str(input_spectrum.Tajima_D())

        output_string = species + ', '
        output_string += theta_w + ', '
        output_string += heterozygosity + ', '
        output_string += tajima_d + '\n'

        with open(output_csv, 'a+') as f:
            f.write(output_string)
        logger.info('Finished computing summary statistics.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    SFSSummaryStatistics().main()
