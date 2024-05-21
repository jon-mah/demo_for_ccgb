"""
Compute the SFS of a given HMP1-2 species.

JCM 20230404
Adapted from code by Richard Wolff
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


class HighRecombination():
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
            'species', type=str,
            help=('String describing the species being analyzed.'))
        parser.add_argument(
            'percentile', type=float,
            help=
                ('Float describing the minimum percentile of recombination '
                 'rate for inclusion'))
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
        species = args['species']
        percentile = args['percentile']
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
        logfile = '{0}{1}high_recombination.log'.format(
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

        logger.info('Computing SFS of ' + str(species) + '.')

        LG = pd.read_csv("../HighRecombinationData/LiuGood2024TableS3.csv", index_col=1)
        iLDS = pd.read_csv("../HighRecombinationData/RW_TableS4.csv", index_col=1)

        # Remove potential duplicates
        LG = LG.loc[LG["Potential duplicate of other events?"] == False]

        LG = LG.loc[species]
        iLDS = iLDS.loc[iLDS['Species'] == species]
        iLDS_site_pos = iLDS.get('site_pos')

        print(iLDS_site_pos)

        LG = LG.loc[LG["between clade?"] == "N"]

        # set window size
        ws = 1000

        # sp = LG.index.get_level_values("Reference genome end loc")
        core_sp_max = LG["Core genome end loc"].max()
        core_sp_min = LG["Core genome start loc"].min()
        core_sp = np.arange(core_sp_min, core_sp_max, ws)

        # core_sp
        num_transfers = {}
        # print(core_sp_max)
        for i in range(len(core_sp)):
            if i + ws < len(core_sp):
                n = ((LG["Core genome start loc"] >= core_sp[i])&(LG["Core genome start loc"] < core_sp[int(i+ws)])).sum()
                num_transfers[(core_sp[int(i)],core_sp[int(i+ws)])] = n
            else:
                n = ((LG["Core genome start loc"] >= core_sp[i])&(LG["Core genome start loc"] < core_sp[-1])).sum()
                num_transfers[(core_sp[int(i)], core_sp[-1])] = n

        num_transfers = pd.Series(num_transfers)
        num_transfers.index.names = ["core_start", "core_end"]
        # print(num_transfers)

        midpoints = num_transfers.index.get_level_values("core_start") + (num_transfers.index.get_level_values("core_end") - num_transfers.index.get_level_values("core_start"))/2

        # Initialize pass_positions with False
        pass_positions = np.full(len(midpoints), False, dtype=bool)

        # Check each midpoint against all iLDS_site_pos values
        for i, midpoint in enumerate(midpoints):
            for pos in iLDS_site_pos:
                if abs(midpoint - pos) <= 1000:
                    pass_positions[i] = True
                    break  # No need to check further if we found a iLDS_site_pos within 1000 units

        # Output the pass_positions vector
        # print(pass_positions)

        fix, ax = plt.subplots(figsize=(16, 8))
        transfer_rate = num_transfers.values / (num_transfers.index.get_level_values("core_end") - num_transfers.index.get_level_values("core_start"))
        transfer_rate = pd.Series(transfer_rate, index=num_transfers.index)

        ax.plot(midpoints, transfer_rate.values)
        ax.set_ylabel("Recombinations / bp", size=25)
        ax.set_xlabel("Core genome position (bp)", size=25)
        ax.axhline(transfer_rate.quantile(percentile), color="k")
        ax.fill_between(midpoints, transfer_rate.quantile(percentile), transfer_rate,
                        where=transfer_rate > transfer_rate.quantile(percentile), alpha=.5)

        ax.scatter(midpoints, transfer_rate.values)
        # ax.scatter(midpoints[pass_positions], transfer_rate.values[pass_positions], color="red")
        plt.savefig('../HighRecombinationAnalysis/' + species + '_recombination_map.png')
        print(midpoints)
        print(num_transfers.values)
        print(transfer_rate.values)
        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    HighRecombination().main()
