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
            help=("Within host allele frequency to call consensus"),
            type=float, default=0.2)
        parser.add_argument('--core', default=False,
            action='store_true',
            help=('Boolean flag for only core genes.'))
        parser.add_argument('--accesory', default=False,
            action='store_true',
            help=('Boolean flag for only accessory  genes.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def read_sites(self, species):
        """Read sites from given species."""
        # Read input snps csv
        df_sites = pd.read_csv(str(config.snps_directory) + '/' +
                               str(species) + '/snps_info.txt.bz2', sep='\t',
                               index_col=0, na_values = 'NaN')

        # Gain list of contigs and assign row value
        df_sites["contig"] = [d.split("|")[0] for d in df_sites.index]
        df_sites.index = [d.split("|")[1] for d in df_sites.index]

        # Ignore non coding sites
        df_sites["gene_id"] = df_sites["gene_id"].fillna("non coding")
        gene_ids = df_sites["gene_id"].values

        gene_breaks = [0] # Initialize list

        gc = gene_ids[0] # Initialize list

        unq_genes = [gc] # Unique genes
        unq_cont = [df_sites["contig"][0]] # Unique contigs

        # append unique genes and contigs to initialized lists
        for i,g in enumerate(gene_ids):
            if g is not gc:
                gene_breaks.append(i)
                gc = g
                unq_genes.append(gc)
                unq_cont.append(df_sites["contig"][i])

        gene_breaks = np.array(gene_breaks) # Cast as array
        # Compute gene lengths from gene breakpoints
        gene_lengths = gene_breaks[1:] - gene_breaks[:-1]

        df_sites.index.set_names("site_pos",inplace=True) # Rename index

        # Multi-index dataframe
        df_sites.set_index('gene_id', append=True, inplace=True)
        df_sites.set_index('contig', append=True, inplace=True)

        df_sites = df_sites.reorder_levels(["contig",'gene_id', 'site_pos'])

        return(df_sites,gene_lengths,unq_genes,unq_cont)

    def load_substitution_rate_map(self, species):
        """Read pre-computed substitution rate map for species."""
        data_directory = os.path.expanduser(
            "/u/project/ngarud/Garud_lab/metagenomic_fastq_files/HMP1-2/data")
        substitution_rate_directory = '%s/substitution_rates/' % data_directory
        intermediate_filename_template = '%s%s.txt.gz'

        intermediate_filename = intermediate_filename_template % (
            substitution_rate_directory, species)

        substitution_rate_map = {}

        if not os.path.isfile(intermediate_filename):
            return substitution_rate_map

        file = gzip.open(intermediate_filename,"r")
        file.readline() # header
        for line in file:
            items = line.split(",")
            if items[0].strip()!=species:
                continue

            record_strs = [", ".join(['Species', 'Sample1', 'Sample2', 'Type',
                                      'Num_muts', 'Num_revs',
                                      'Num_mut_opportunities',
                                      'Num_rev_opportunities'])]

            sample_1 = items[1].strip()
            sample_2 = items[2].strip()
            type = items[3].strip()
            num_muts = float(items[4])
            num_revs = float(items[5])
            num_mut_opportunities = float(items[6])
            num_rev_opportunities = float(items[7])

            num_changes = num_muts+num_revs
            num_opportunities = num_mut_opportunities+num_rev_opportunities

            sample_pair = (sample_1, sample_2)

            if type not in substitution_rate_map:
                substitution_rate_map[type] = {}

            substitution_rate_map[type][sample_pair] = (num_muts, num_revs,
                num_mut_opportunities, num_rev_opportunities)

        return substitution_rate_map

    def calculate_mutrev_matrices_from_substitution_rate_map(self, substitution_rate_map, type, allowed_samples=[]):
        # Rewritten to preserve order of allowed samples
        # If allowed samples contains things that are not in DB, it returns zero opportunities

        total_sample_set = set([])
        for sample_1, sample_2 in substitution_rate_map[type].keys():
            total_sample_set.add(sample_1)
            total_sample_set.add(sample_2)

        if len(allowed_samples)==0:
            allowed_samples = list(sorted(total_sample_set))

        # allows us to go from sample name to idx in allowed samples (to preserve order)
        sample_idx_map = {allowed_samples[i]:i for i in xrange(0,len(allowed_samples))}

        mut_difference_matrix = numpy.zeros((len(allowed_samples), len(allowed_samples)))*1.0
        rev_difference_matrix = numpy.zeros_like(mut_difference_matrix)

        mut_opportunity_matrix = numpy.zeros_like(mut_difference_matrix)
        rev_opportunity_matrix = numpy.zeros_like(mut_difference_matrix)

        for sample_pair in substitution_rate_map[type].keys():

            sample_i = sample_pair[0]
            sample_j = sample_pair[1]

            if not ((sample_i in sample_idx_map) and (sample_j in sample_idx_map)):
                continue

            i = sample_idx_map[sample_i]
            j = sample_idx_map[sample_j]

            num_muts, num_revs, num_mut_opportunities, num_rev_opportunities = substitution_rate_map[type][sample_pair]

            mut_difference_matrix[i,j] = num_muts
            rev_difference_matrix[i,j] = num_revs

            mut_opportunity_matrix[i,j] = num_mut_opportunities
            rev_opportunity_matrix[i,j] = num_rev_opportunities

        return allowed_samples, mut_difference_matrix, rev_difference_matrix, mut_opportunity_matrix, rev_opportunity_matrix

    def calculate_matrices_from_substitution_rate_map(self, substitution_rate_map, type, allowed_samples=[]):
        # once the map is loaded, then we can compute rate matrices in this definition (so, it relies on the previous def)

        samples, mut_difference_matrix, rev_difference_matrix, mut_opportunity_matrix, rev_opportunity_matrix = self.calculate_mutrev_matrices_from_substitution_rate_map( substitution_rate_map, type, allowed_samples)

        difference_matrix = mut_difference_matrix+rev_difference_matrix
        opportunity_matrix = mut_opportunity_matrix+rev_opportunity_matrix

        return samples, difference_matrix, opportunity_matrix

    def calculate_unique_samples(self, subject_sample_map, sample_list=[]):

        if len(sample_list)==0:
            sample_list = list(sorted(flatten_samples(subject_sample_map).keys()))

        # invert subject sample map
        sample_subject_map = {}
        for subject in subject_sample_map.keys():
            for sample in subject_sample_map[subject].keys():
                sample_subject_map[sample] = subject

        subject_idx_map = {}

        for i in xrange(0,len(sample_list)):
            sample = sample_list[i]
            if sample.endswith('c'):
                sample = sample[:-1]
            subject = sample_subject_map[sample]
            if not subject in subject_idx_map:
                subject_idx_map[subject] = i

        unique_idxs = numpy.zeros(len(sample_list),dtype=numpy.bool_)
        for i in subject_idx_map.values():
            unique_idxs[i]=True

        return


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
        core_bool = args['core']
        accessory_bool = args['accesory']
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
        if core_bool:
           file_tag = 'core_'
        elif accessory_bool: 
           file_tag = 'accessory_'
        else:
           file_tag = ''
        sfs_dataframe = \
           '{0}{1}{2}sfs_dataframe.csv'.format(
                args['outprefix'], underscore, file_tag)
        empirical_syn_sfs = \
            '{0}{1}{2}empirical_syn_sfs.txt'.format(
                args['outprefix'], underscore, file_tag)
        empirical_nonsyn_sfs = \
            '{0}{1}{2}empirical_nonsyn_sfs.txt'.format(
                args['outprefix'], underscore, file_tag)
        logfile = '{0}{1}{2}compute_sfs.log'.format(
            args['outprefix'], underscore, file_tag)
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

        logger.info('Computing SFS of ' + str(species) + '.')

        # Load core genes
        subject_sample_map = parse_HMP_data.parse_subject_sample_map()
        core_genes = parse_midas_data.load_core_genes(species)

        # Default parameters
        alpha = 0.5 # Confidence interval range for rate estimates
        low_divergence_threshold = 5e-04
        min_change = 0.8

        snp_samples = diversity_utils.calculate_haploid_samples(species)
        snp_samples = snp_samples[self.calculate_unique_samples(
            subject_sample_map, snp_samples)]
        snp_samples = snp_samples.ravel()
        snp_samples = [s.decode("utf-8")  for s in snp_samples]

        # Pre-computed substituion rates for species
        subject_sample_map = parse_HMP_data.parse_subject_sample_map()
        substitution_rate_map = self.load_substitution_rate_map(species)
        dummy_samples, snp_difference_matrix, snp_opportunity_matrix = \
            self.calculate_matrices_from_substitution_rate_map(
                substitution_rate_map, 'core', allowed_samples=snp_samples)
        snp_samples = numpy.array(dummy_samples)
        substitution_rate = snp_difference_matrix * 1.0 / \
            (snp_opportunity_matrix+(snp_opportunity_matrix==0))

        coarse_grained_idxs, coarse_grained_cluster_list = \
            clade_utils.cluster_samples(
                substitution_rate, min_d=low_divergence_threshold)

        coarse_grained_samples = snp_samples[coarse_grained_idxs]
        clade_sets = clade_utils.load_manual_clades(species)

        clade_idxss = clade_utils.calculate_clade_idxs_from_clade_sets(
            coarse_grained_samples, clade_sets)

        clade_sizes = numpy.array(
            [clade_idxs.sum() for clade_idxs in clade_idxss])

        largest_clade_samples = \
            coarse_grained_samples[ clade_idxss[clade_sizes.argmax()] ]

        # clade = clade_utils.load_largest_clade(species)
        clade = largest_clade_samples
        snps_dir = "%ssnps/%s" % (config.data_directory,species)
        snps_summary = pd.read_csv(
            "%s/snps_summary.txt" % snps_dir,sep="\t",index_col=0)
        L = snps_summary["covered_bases"]
        mean_coverage = snps_summary["mean_coverage"]

        samples_host = list(pd.read_csv(
            "%s/snps_depth.txt.bz2" % snps_dir,sep="\t",index_col=0, nrows=0))
        samples_tuples = list(itertools.combinations(
            samples_host, 2)) + [(s1,s1) for s1 in samples_host]

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
        df_sites,gene_lengths,unq_genes,unq_cont = self.read_sites(species)

        ## frequency of each nucleotide at each site
        atcg = df_sites["allele_props"].dropna().str.split("|")
        atcg = pd.DataFrame([[elem[2:] for elem in e] for e in atcg],index=atcg.index,columns=["a","c","t","g"])
        atcg = atcg.astype(float)

        ## pulls S/NS status for each nucleotide mutation at each site relative to ref state
        syn_non = df_sites["snps"].loc[df_sites.index]
        syn_non = syn_non.fillna("A:SYN|C:SYN|T:SYN|G:SYN")
        syn_non = syn_non.loc[[len(s) > 1 for s in syn_non]]
        syn_non = pd.DataFrame([[elem[2:] for elem in e] for e in syn_non.str.split("|")],index=syn_non.index,columns=["a","c","t","g"])

        logger.info('Looping over genes.')

        all_haplotypes = []

        sfs = []

        # max_gene_count = 25 # Uncomment for testing



        for i,chunk_size in enumerate(gene_lengths):

            # if i > max_gene_count:
            #     break
            ## read next chunk_size number of lines
            df_depth = df_depth_reader.get_chunk(chunk_size)
            df_refreq = df_refreq_reader.get_chunk(chunk_size)

            gene_name = unq_genes[i]
            # print(str(unq_genes[i]))
            if 'non' in str(unq_genes[i]):
                logger.info('Ignoring non-coding gene.')
                continue

            if core_bool and gene_name not in core_genes:
                logger.info('Ignoring non-core gene.')
                continue

            if accessory_bool and gene_name not in accessory_genes:
                logger.info('Ignoring non-accessory gene.')
                continue
            logger.info('Processing ' + str(unq_genes[i]) + '.')

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
            # print(df_haplotypes)
            sfs_clade = df_haplotypes[clade].T.mean()

            sfs_df = pd.DataFrame(columns=["all","largest_clade"],index=sfs_all.index)

            sfs_df["all"] = sfs_all
            sfs_df["largest_clade"] = sfs_clade

            sfs.append(sfs_df)

        df = pd.concat(sfs)

        # Save intermediate dataframe if desired
        # df.to_csv(sfs_dataframe)

        df.loc[df["all"] > 0.5]["all"] = df.loc[df["all"] > 0.5]["all"].values
        df.loc[df["largest_clade"] > 0.5]["largest_clade"] = df.loc[df["largest_clade"] > 0.5]["largest_clade"].values

        all_1 = df.dropna()["all"].xs("1D",level="site_type")
        all_4 = df.dropna()["all"].xs("4D",level="site_type")

        clade_1 = df.dropna()["largest_clade"].xs("1D",level="site_type")
        clade_4 = df.dropna()["largest_clade"].xs("4D",level="site_type")

        # Compute frequency of singletons
        all_min = min(all_1.loc[all_1 > 0].min(),all_4.loc[all_4 > 0].min())
        clade_min = min(clade_1.loc[clade_1 > 0].min(),clade_4.loc[clade_4 > 0].min())

        # Include 0-tons and n-tons
        bins_all = list(np.arange(0, 1.0 + all_min, all_min))

        # Include 0-tons and n-tons
        bins_clade = list(np.arange(0, 1.0 + clade_min, clade_min))

        sfs_1 = np.histogram(all_1, bins=bins_all, density=False)
        sfs_4 = np.histogram(all_4, bins=bins_all, density=False)
        sfs_clade_1 = np.histogram(clade_1, bins=bins_clade, density=False)
        sfs_clade_4 = np.histogram(clade_4, bins=bins_clade, density=False)

        logger.info('There are {0} species in the largest clade.'.format(
            str(len(clade))))
        logger.info('There are {0} bins in the SFS.'.format(
            str(len(bins_clade))))

        logger.info('Outputting SFSs in Dadi format.')
        if core_bool:
            logger.info('Outputting SFS of just core genes.')
        if accessory_bool:
            logger.info('Outputting SFS of just accessory genes.')

        syn_clade_sfs = dadi.Spectrum(sfs_clade_4[0])
        syn_clade_sfs.to_file(empirical_syn_sfs)
        nonsyn_clade_sfs = dadi.Spectrum(sfs_clade_1[0])
        nonsyn_clade_sfs.to_file(empirical_nonsyn_sfs)


        logger.info('Pipeline executed succesfully.')


if __name__ == '__main__':
    ComputeSFS().main()
