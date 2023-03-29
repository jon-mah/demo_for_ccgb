"""
Summarizes the average pi values per sample across species and populations.

JCM 20220516
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
from numba import njit, jit
import pandas as pd
import config
from utils import parse_midas_data, diversity_utils, clade_utils, parse_HMP_data
import numpy as np
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


class ComputeSFS():
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
        parser.add_argument('--min_depth',
            help=("Minimum depth to use to call a variant at a site in "
                  "a sample"), type=int, default=20)
        parser.add_argument('--min_MAF',
            help=("Minimum allele frequency to be considered a polymorphic "
                 "site. Note: allele frequency is determined as percentage of "
                 "total number of reads across all hosts supporting an "
                 "allele. Large fluctuations in allele frequency across "
                 "hosts can bias this."), type=float, default=0.0)
        parser.add_argument('--f_star',
            help=("WIthin host allele frequency to call consensus"),
            type=float, default=0.2)
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
        min_depth = args['min_depth']
        min_MAF = args['min_MAF']
        f_star = args['f_star']
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
           '{0}{1}empirical_sfs.csv'.format(
                args['outprefix'], underscore, species)
        logfile = '{0}{1}compute_sfs.log'.format(
            args['outprefix'], underscore, species)
        to_remove = [output_matrix, logfile, empirical_sfs]
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

        clade = clade_utils.load_largest_clade(species)
        snps_dir = "%ssnps/%s" % (config.data_directory,species)
        snps_summary = pd.read_csv("%s/snps_summary.txt" % snps_dir,sep="\t",index_col=0)
        L = snps_summary["covered_bases"]
        mean_coverage = snps_summary["mean_coverage"]

        samples_host = list(pd.read_csv("%s/snps_depth.txt.bz2" % snps_dir,sep="\t",index_col=0, nrows=0))
        samples_tuples = list(itertools.combinations(samples_host, 2)) + [(s1,s1) for s1 in samples_host]

        reader=True

        haploid_samples = diversity_utils.calculate_haploid_samples(species)

        ## initialize chunk readers for sample depth and allele frequency
        df_depth_reader = pd.read_csv("%s/snps_depth.txt.bz2" % snps_dir,sep="\t",index_col=0, iterator=True,low_memory=False)
        df_refreq_reader = pd.read_csv("%s/snps_ref_freq.txt.bz2" % snps_dir,sep="\t",index_col=0, iterator=True,low_memory=False)

        ## reads header. chunking can now proceed on data in files
        df_depth_header = df_depth_reader.get_chunk(0)
        df_refreq_header = df_refreq_reader.get_chunk(0)

        ## read snps_info file, which contains cohort-wide information about allele frequency,
        ## as well as information about sites (contig/gene, S/NS etc)
        df_sites,gene_lengths,unq_genes,unq_cont = read_sites(species)

        ## frequency of each nucleotide at each site
        atcg = df_sites["allele_props"].dropna().str.split("|")
        atcg = pd.DataFrame([[elem[2:] for elem in e] for e in atcg],index=atcg.index,columns=["a","c","t","g"])
        atcg = atcg.astype(float)

        ## pulls S/NS status for each nucleotide mutation at each site relative to ref state
        syn_non = df_sites["snps"].loc[df_sites.index]
        syn_non = syn_non.fillna("A:SYN|C:SYN|T:SYN|G:SYN")
        syn_non = syn_non.loc[[len(s) > 1 for s in syn_non]]
        syn_non = pd.DataFrame([[elem[2:] for elem in e] for e in syn_non.str.split("|")],index=syn_non.index,columns=["a","c","t","g"])

        sys.stderr.write(f"Looping over genes \n")

        all_haplotypes = []

        sfs = []

        for i,chunk_size in enumerate(gene_lengths):

            ## read next chunk_size number of lines
            df_depth = df_depth_reader.get_chunk(chunk_size)
            df_refreq = df_refreq_reader.get_chunk(chunk_size)

            gene_name = unq_genes[i]

            sys.stderr.write(f"Processing {unq_genes[i]} \n")

            df_depth.columns = [d[:-1] if d[-1] == "c" else d for d in df_depth.columns]
            df_refreq.columns = [d[:-1] if d[-1] == "c" else d for d in df_refreq.columns]

            df_depth = df_depth[haploid_samples]
            df_refreq = df_refreq[haploid_samples]

            ## initialize haplotype dataframe as alternate allele freq in each sample
            df_haplotypes = 1 - df_refreq.copy()

            ## treat sites with less than min_depth coverage as missing data
            df_haplotypes = df_haplotypes.mask(df_depth < min_depth)

            ## if alternate is consensus, mark sample as 1. if ref is consensus, mark as 0.
            df_haplotypes = df_haplotypes.mask(df_haplotypes >= .8,1)
            df_haplotypes = df_haplotypes.mask(df_haplotypes <= .2,0)

            ## treat intermediate frequency variants as missing data
            df_depth = df_depth.mask(np.logical_and(df_haplotypes < .8,df_haplotypes > .2))
            df_haplotypes = df_haplotypes.mask(np.logical_and(df_haplotypes < .8,df_haplotypes > .2))

            df_haplotypes.index = [d.split("|")[1] for d in df_haplotypes.index]
            df_haplotypes.index.set_names("site_pos",inplace=True)
            df_haplotypes["gene_id"] = df_haplotypes.shape[0]*[gene_name]
            df_haplotypes["contig"] = df_haplotypes.shape[0]*[unq_cont[i]]
            df_haplotypes.set_index('gene_id', append=True, inplace=True)
            df_haplotypes.set_index('contig', append=True, inplace=True)
            df_haplotypes = df_haplotypes.reorder_levels(["contig",'gene_id', 'site_pos'])

            df_haplotypes = df_haplotypes.loc[[d for d in df_haplotypes.index if d in df_sites.index]]

            df_haplotypes["site_type"] = df_sites.loc[df_haplotypes.index]["site_type"]
            df_haplotypes.set_index('site_type', append=True, inplace=True)
            df_haplotypes = df_haplotypes.reorder_levels(["contig",'gene_id', 'site_pos','site_type'])

            sfs_all = df_haplotypes.T.mean()

            sfs_clade = df_haplotypes[clade].T.mean()

            sfs_df = pd.DataFrame(columns=["all","largest_clade"],index=sfs_all.index)

            sfs_df["all"] = sfs_all
            sfs_df["largest_clade"] = sfs_clade

            sfs.append(sfs_df)

            print(sfs[-1])

        sfs = pd.concat(sfs)

        sfs.to_csv(empirical_sfs)

        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ComputeSFS().main()
