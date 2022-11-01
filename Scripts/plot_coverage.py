""" Plots out a 2D heatmap of log likelihoods around a given set of model
    parameters.

JCM 20220729
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg') # Must be before importing matplotlib.pyplot or pylab!
import matplotlib.pyplot as plt
from matplotlib import ticker, cm
import seaborn as sns


class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class PlotCoverage():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def plotCoverageParser(self):
        """Return *argparse.ArgumentParser* for ``plot_coverage.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Generates a plot describing coverage for given species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'inprefix', type=str,
            help=('The file prefix for the input files.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files.')
        return parser


    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.plotCoverageParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        inprefix = args['inprefix']
        outprefix = args['outprefix']
        species_prevalence = inprefix + 'species/species_prevalence.txt'
        species_coverage = inprefix + 'species/species_coverage.txt'
        species_count_reads = inprefix + 'species/count_reads.txt'

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
        output_plot = \
            '{0}{1}coverage.jpg'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, output_plot]
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

        species_prevalence_df = pd.read_csv(species_prevalence, sep='\t')
        species_prevalence_df = species_prevalence_df.sort_values(
            by = 'mean_coverage', ascending=False)
        # print(species_prevalence_df)

        species_coverage_df = pd.read_csv(species_coverage, sep='\t')
        species_count_reads_df = pd.read_csv(species_count_reads, sep='\t')

        # Create a dataframe of top 20 highest coverage species in each sample

        n = 40
        coverage_df_wide = pd.DataFrame({'species_id': np.arange(n)})
        species_ids_df = pd.DataFrame()

        for s in species_coverage_df.columns[1:]:
            temp_df = species_coverage_df.sort_values(by = s, ascending=False)
            coverage_df_wide = np.array(temp_df[s][:n])
            species_ids_df[s] = np.array(temp_df['species_id'])

        logger.info('Finished plotting coverage.')
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    PlotCoverage().main()
