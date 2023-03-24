"""
Writes to file the manual clade identification.

JCM 20230323
Adapted from code written by Richard Wolff
"""


import sys
import os
import logging
import time
import argparse
import warnings

import config
import pandas as pd
import numpy as np
from skbio import DistanceMatrix
from skbio.tree import nj
import scipy.cluster.hierarchy as shc
from matplotlib import pyplot as pyplot
import sys
from scipy.cluster.hierarchy import fcluster
from scipy.spatial.distance import squareform


class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class CreateClade():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def createCladeParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Write to file the manual clade identifications.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_species', type=str,
            help='The species for which we are defining clades.')
        parser.add_argument(
            'input_threshold', type=float,
            help='The threshold for automatically cutting clades.')
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser


    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.createCladeParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        input_species = args['input_species']
        man_clus_thresh = args['input_threshold']
        genetic_distances_dir = config.genetic_distances_dir
        dist_loc = f"{genetic_distances_dir}/genetic_distance_matrices"
        good_species = config.good_species

        # Numpy options
        np.set_printoptions(linewidth=np.inf)

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
        manual_clade = \
            '{0}{1}manual_clade_definitions.txt'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}create_clade.log'.format(args['outprefix'], underscore)
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

        # Define manual cluster dictionary
        man_clus_dic = {}

        # Define species for clade identification
        species = input_species
        logger.info('Creating clade for: ' + str(species) + '.')

        # Read in distances for given species
        df = pd.read_csv(f"{dist_loc}/{species}_distance.txt", index_col=0)

        # Prune closely related samples
        close_thresh = 2.5 * 1e-5
        logger.info('Before pruning of closely related samples we '
                    'have ' + str(df.shape[0]) + ' samples total.')
        close_samples_dic = {}
        for row in df.index:
            close_samples = list(df.loc[row][(df.loc[row] < close_thresh)].index)
            close_samples.remove(row)
            if len(close_samples) > 0:
                close_samples_dic[row] = close_samples
        logger.info('There are a total of ' +
                    str(int(len(list(close_samples_dic.keys())))) +
                    ' samples with one or more closely related samples.')
        for key, value in close_samples_dic.items():
            for idx in value:
                if idx in df.index:
                    df = df.drop(idx, axis=0).drop(idx, axis=1)
        logger.info('After pruning of closely related samples, ' +
                    'we have ' + str(df.shape[0]) + 
                    ' samples remaining.')

        # Define clades through clustering and thresholding.
        D = df.values
        D = squareform(D)
        Z = shc.linkage(D, method='average')
        # man_clus_thresh = 7 * 1e-3
        clusters = fcluster(Z,  man_clus_thresh, criterion='distance')
        man_clus_dic[species] = {i:list(df.columns[clusters == i]) for i in list(set(clusters))}          

        logger.info('Outputting manual clade identification.')
        with open(manual_clade, 'a+') as f:
            print(species, file=f)
            i = 0
            item_list = man_clus_dic[species].items()
            max_clus_num = list(item_list)[-1][0]
            for item in item_list:
                cluster = item[0]
                samples = item[1]
                for sample in samples:
                    print(f"{i} {sample}", file=f)
                    i += 1
                if cluster < max_clus_num:
                    print("------------", file=f)

        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    CreateClade().main()
