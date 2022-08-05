"""
Computes SNP matrix from a given `*snps_ref_freq.txt.bz2`.

Recall that pi is the expected heterozygosity.
Recall that heterozygosity is the probability of grabbing
    two disinct alleles, i.e., randomly selecting a
    heterozygote.

JCM 20211116
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

class HumanHost(object):
    """docstring for HumanHost."""

    def __init__(self, avg_pi_val, daepths_bool, p_count, q_count):
        self.avg_pi_val = avg_pi_val
        self.depths_bool = depths_bool
        self.p_count = p_count
        self.q_count = q_count



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
                'Given a specified `*snps_ref_freq.txt.bz2, this script '
                'computes average Pi across samples matrix for that species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_depth', type=self.ExistingFile,
            help=('Annotated SNPs document in the format output by Midas'))
        parser.add_argument(
            'input_ref_freq', type=self.ExistingFile,
            help=('Reference SNP frequency document in the format output '
                  'by Midas'))
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
        input_depth = args['input_depth']
        input_ref_freq = args['input_ref_freq']
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
        output_matrix = \
            '{0}{1}output_matrix.csv'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}compute_pi.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, output_matrix]
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

        # open post-processed MIDAS output
        depth_file = bz2.BZ2File(input_depth, "r")
        site_ids = depth_file.readline().split()[1:]

        num_ids = len(site_ids)
        host_list = []
        with open(output_matrix, 'w') as f:
            avg_pi_vals = []
            depth_per_sample = []
            for i in range(1, num_ids + 1):
                depth_file = bz2.BZ2File(input_depth, "r")
                depth_header = depth_file.readline()
                depths = []
                depths_bool = []
                for line in depth_file:
                    items = line.split()
                    depth_val = items[i]
                    depths.append(int(depth_val))
                depths_bool = [x>=20 for x in depths]
                if len(depths_bool) < 1:
                    continue
                ref_freq_file = bz2.BZ2File(input_ref_freq, "r")
                ref_freq_header = ref_freq_file.readline()
                pi_vals = []
                p_count = []
                q_count = []
                j = 0
                for line in ref_freq_file:
                    items = line.split()
                    this_pi_val = float(items[i])
                    this_p = this_pi_val
                    this_q = (1 - this_pi_val)
                    p_count.append(this_p * depths[j])
                    q_count.append(this_q * depths[j])
                    if depths_bool[j]:
                        pi_val = 2 * this_p * this_q
                        pi_vals.append(pi_val)
                    j += 1
                avg_pi_val = sum(pi_vals) / len(pi_vals)
                depth_per_sample.append(len(pi_vals))
                print(this_p)
                avg_pi_vals.append(avg_pi_val)
                host_list.append(HumanHost(avg_pi_val,
                                           depths_bool,
                                           p_count,
                                           q_count))
                del depths
                del depths_bool
                del pi_vals
                del p_count
                del q_count
                del ref_freq_file
                del depth_file
            output_header = 'site id, ' + str(site_ids[0])
            if len(site_ids) > 1:
                for id in site_ids[1:]:
                    output_header += ', ' + str(id)
            output_pi_values = 'average pi, ' + str(avg_pi_vals[0])
            if len(avg_pi_vals) > 1:
                for val in avg_pi_vals[1:]:
                    output_pi_values += ', ' + str(val)
            num_sites = 'num_sites, ' + str(depth_per_sample[0])
            if len(num_sites) > 1:
                for num in depth_per_sample[1:]:
                    num_sites += ', ' + str(num)
            f.write(output_header + '\n')
            f.write(output_pi_values + '\n')
            f.write(num_sites + '\n')

            # aggregate across host pi
            # total_count_p = 0
            # total_count_q = 0
            # for i in range(len(count_list)):
            #     total_count_p += count_list[i][0]
            #     total_count_q += count_list[i][1]
            # total_denom = total_count_p + total_count_q
            # total_freq_p = total_count_p / total_denom
            # total_freq_q = total_count_q / total_denom
            # aggregate_across_pi = 2 * total_freq_p * total_freq_q
            # print(len(count_list))
            # output_aggregate = 'aggregate_across_pi'
            # for i in range(len(count_list)):
            #     output_aggregate += ', ' + str(aggregate_across_pi)
            # f.write(output_aggregate + '\n')

            # distributed across host pi
            # distributed_across_pi = []
            # if len(count_list) > 1:
            #    for i in range(0, len(count_list)):
            #        for j in range(i + 1, len(count_list)):
            #            count_p = count_list[i][0] + count_list[j][0]
            #            count_q = count_list[i][1] + count_list[j][1]
            #            pair_denom = count_p + count_q
            #            freq_p = count_p / pair_denom
            #            freq_q = count_q / pair_denom
            #            distributed_across_pi.append(2 * freq_p * freq_q)
            #            print(str(i) + ', ' +
            #                  str(j) + ', ' + str(2 * freq_p * freq_q)  +
            #                  ', ' + str(count_p) + ', ' + str(count_q) + ', ' +
            #                  str(freq_p) + ', ' + str(freq_q))
            # else:
            #    distributed_across_pi = [aggregate_across_pi]
            # print(depth_per_sample)
            # print(distributed_across_pi)
            # distributed_across_pi = (sum(distributed_across_pi) /
            #                          len(distributed_across_pi))
            # print(distributed_across_pi)
            # output_distributed = 'distributed_across_pi'

            # for i in range(len(count_list)):
            #     output_distributed += ', ' + str(distributed_across_pi)
            # f.write(output_distributed)

if __name__ == '__main__':
    ComputePi().main()
