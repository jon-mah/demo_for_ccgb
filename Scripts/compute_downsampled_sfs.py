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
        core_genes = parse_midas_data.load_core_genes(species_name)

        # Load allele counts map for this species
        snp_samples, allele_counts_map, passed_sites_map, final_line_number = parse_midas_data.parse_snps(species_name, allowed_variant_types=['1D','2D','3D','4D'], allowed_genes=core_genes, debug=True)

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
          f.write('site, alts, refs, total\n')
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
          genotype_matrix, passed_sites_matrix = diversity_utils.calculate_consensus_genotypes(allele_counts,lower_threshold,upper_threshold)
          num_sites = passed_sites_matrix.shape[0]
          num_samples = passed_sites_matrix.shape[1]
          with open(output_matrix, 'a+') as f:
            for i in range(0, num_sites):
              site_id = gene_name + '.site.' + str(i+1)
              alts = int(sum(genotype_matrix[i]))
              fails = int(sum(numpy.invert(passed_sites_matrix[i])))
              refs = int(len(genotype_matrix[i]) - fails)
              f.write(site_id + ', ' + str(alts) + ', ' + str(refs) + ', ' + str(alts + refs) + '\n')
          # prevalences = (genotype_matrix*passed_sites_matrix).sum(axis=1)
          # min_prevalences = 0.5
          # max_prevalences = (passed_sites_matrix).sum(axis=1)-0.5
          # polymorphic_sites = (prevalences>min_prevalences)*(prevalences<max_prevalences)
          # ks = prevalences[polymorphic_sites]
          # ns = passed_sites_matrix.sum(axis=1)[polymorphic_sites]
          # minor_ks = numpy.fmin(ks,ns-ks)
          # break

        # Finally, do whatever you need to do to figure out what the data structure and data is
        # print(allele_counts)
        # print(genotype_matrix)
        # print(passed_sites_matrix)
        # print(prevalences)




if __name__ == '__main__':
    ComputeDownSampledSFS().main()
