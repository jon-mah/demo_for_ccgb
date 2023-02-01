"""
Parses the `out.snps` output from MUMmer into human-readable format.

JCM 20230131
"""


import sys
import os
import logging
import time
import argparse
import warnings
import random


class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class ParseSnps():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def ParseSnpsParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Parses a given `*.snps` file into human-readable format.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_snps', type=self.ExistingFile,
            help='The input `*.snps` file output by MUMmer.')
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files.')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.ParseSnpsParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        input_snps  = args['input_snps']
        outprefix = args['outprefix']

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
        output_tsv = \
            '{0}{1}parsed_snps.tsv'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}parse_snps.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, output_tsv]
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

        # Initialize dict object for snps, queries
        snps_dict = {}

        logger.info('Parsing input snps.')
        # Open output input file and read into snps dict
        with open(input_snps, 'r') as f:
            for line in f:
                line = line.strip().split('\t')
                position = line[0]
                ref_allele = line[1]
                alt_allele = line[2]
                ref_contig = line[-2]
                query_contig = line[-1]

                if position not in snps_dict:
                    snps_dict[position] = [ref_contig, ref_allele, {query_contig: alt_allele}]
                else:
                    snps_dict[position][2][query_contig] = alt_allele

        logger.info('Outputting parsed output.')
        with open(output_tsv, 'w') as f:
            f.write('Position\tRef_Contig\tRef_Allele\tQuery_Contig\n')
            for position, values in snps_dict.items():
                f.write(position + '\t' + str(values[0]) + '\t' + str(values[1]) + '\t' + str(values[2]) + '\n')
        # print(snps_dict.items())
        logger.info('Pipeline executed successfully.')


if __name__ == '__main__':
    ParseSnps().main()
