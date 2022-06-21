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

import numpy
import bz2
import pandas as pd
from utils import parse_midas_data, diversity_utils
import numpy

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

    def computeDownSampledSFSParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Computes a downsampled SFS for a given species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'species', type=str,
            help=('String describing the species being analyzed.'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computeDownSampledSFSParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        outprefix = args['outprefix']
        species = args['species']

        # Numpy options
        numpy.set_printoptions(linewidth=numpy.inf)

        # Output files: logfile, snp_matrix.csv
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        output_matrix = \
            '{0}{1}{2}_downsampled_sfs.csv'.format(
                args['outprefix'], underscore, species)

        # Load core genes
        core_genes = parse_midas_data.load_core_genes(species)

        # Default parameters
        alpha = 0.5 # Confidence interval range for rate estimates
        low_divergence_threshold = 5e-04
        min_change = 0.8

        # Pre-computed substituion rates for species
        substitution_rate_map = calculate_substitution_rates.load_substitution_rate_map(species)
        dummy_samples, snp_difference_matrix, snp_opportunity_matrix = calculate_substitution_rates.calculate_matrices_from_substitution_rate_map(substitution_rate_map, 'core', allowed_samples=snp_samples)
        snp_samples = numpy.array(dummy_samples)
        substitution_rate = snp_difference_matrix*1.0/(snp_opportunity_matrix+(snp_opportunity_matrix==0))


        coarse_grained_idxs, coarse_grained_cluster_list = clade_utils.cluster_samples(substitution_rate, min_d=low_divergence_threshold)

        coarse_grained_samples = snp_samples[coarse_grained_idxs]
        clade_sets = clade_utils.load_manual_clades(species)

        clade_idxss = clade_utils.calculate_clade_idxs_from_clade_sets(coarse_grained_samples, clade_sets)

        clade_sizes = numpy.array([clade_idxs.sum() for clade_idxs in clade_idxss])

        largest_clade_samples = coarse_grained_samples[ clade_idxss[clade_sizes.argmax()] ]


        # Load allele counts map for this species
        snp_samples, allele_counts_map, passed_sites_map, final_line_number = parse_midas_data.parse_snps(species, allowed_variant_types=['1D','2D','3D','4D'], allowed_samples=largest_clade_samples, allowed_genes=core_genes, debug=True)

        allowed_variant_types = '4D'
        allowed_genes = core_genes
        pi_min_k = 4
        lower_threshold = 0.2
        upper_threshold = 0.8

        # Pooled counts: for reference
        # synonymous_counts, synonymous_weights = diversity_utils.calculate_pooled_counts(allele_counts_map, passed_sites_map, allowed_variant_types = set(['4D']), allowed_genes=core_genes,pi_min_k=4)

        # Dive into internals of calculatealle_pooled_counts
        allowed_variant_types = '4D'
        allowed_genes = core_genes
        pi_min_k = 4
        lower_threshold = 0.2
        upper_threshold = 0.8

        with open(output_matrix, 'w+') as f:
          header = 'Bacl' + '\t' + 'Dsim' + '\t' + 'Allele1' + '\t' + 'BAC' + '\t' + 'Allele2' + '\t' + 'BAC'+ '\t' + 'SNPid'
          f.write(header + '\n')
        for gene_name in allele_counts_map:
          for variant_type in allele_counts_map[gene_name].keys():
            if variant_type not in allowed_variant_types:
              continue
            allele_counts = allele_counts_map[gene_name][variant_type]['alleles']
          if len(allele_counts)==0:
            continue
          allele_counts = allele_counts
          # passed_sites_matrix = boolean of whether a site passes or note
          # genotype_matrix = 1 or 0, 1 --> alt allele, 0 --> reference or false in passed sites_matrix
          # output alt ref fail depth
          # For each site, you want to know (1) how many samples have alternate allele
          # (freq>0.8, (2) how many samples have reference allele (freq>0.8),
          # and (3) how many samples have 0 coverage
          # site number --> gene + position within gene
          genotype_matrix, passed_sites_matrix = diversity_utils.calculate_consensus_genotypes(
            allele_counts, lower_threshold, upper_threshold)
          num_sites = passed_sites_matrix.shape[0]
          num_samples = passed_sites_matrix.shape[1]
          with open(output_matrix, 'a+') as f:
            for i in range(0, num_sites):
              site_id = gene_name + '.site.' + str(i+1)
              alts = int(sum(genotype_matrix[i]))
              fails = int(sum(numpy.invert(passed_sites_matrix[i])))
              refs = int(len(genotype_matrix[i]) - fails)
              string = '-A-' + '\t' + '---' + '\t' + 'A' + '\t'
              string = string + str(refs) + '\t' + 'G' + '\t' + str(alts)
              string = string + '\t' + site_id + '\n'
              f.write(string)





if __name__ == '__main__':
    ComputeDownSampledSFS().main()
