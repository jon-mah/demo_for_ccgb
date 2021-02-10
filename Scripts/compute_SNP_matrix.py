"""
Computes SNP matrix from a given `annotated_snps.txt.bz2`.

JCM 20210209
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy
import bz2


class ArgumentParserNoArgHelp(argparse.ArgumentParser):
    """Like *argparse.ArgumentParser*, but prints help when no arguments."""

    def error(self, message):
        """Print error message, then help."""
        sys.stderr.write('error: %s\n\n' % message)
        self.print_help()
        sys.exit(2)


class ComputeSNPMatrix():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def computeSNPMatrixParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'Given a specified `annotated_snps.txt.bz2, this script'
                'computes the SNP matrix for that species.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'input_snps', type=self.ExistingFile,
            help=('Annotated SNPs document in the format output by Midas'))
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.computeSNPMatrixParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        input_snps = args['input_snps']
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
        snp_matrix = \
            '{0}{1}snp_matrix.csv'.format(
                args['outprefix'], underscore)
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
        to_remove = [logfile, snp_matrix]
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

        with open(snp_matrix, 'w') as f:
            # Open post-processed MIDAS output
            snp_file = bz2.BZ2File(input_snps, "r")

            line = snp_file.readline()  # Grab header
            items = line.split()[1:]
            samples = numpy.array([item.strip() for item in items])
            f.write(items[0])
            for item in items[1:]:
                f.write(', ' + item)
            f.write('\n')
            for line in snp_file:
                items = line.split()
                # Load information about site
                info_items = items[0].split("|")
                chromosome = info_items[0]
                location = long(info_items[1])
                gene_name = info_items[2]
                variant_type = info_items[3]

                if len(info_items) > 5:  # for backwards compatability
                    polarization = info_items[4]
                    p_value = float(info_items[5])

                else:
                    polarization = "?"
                    pvalue = float(info_items[4])

                if p_value <= 0.05:
                    # Load alt and depth counts
                    above_minimum_depth = True
                    for item in items[1:]:
                        depth = int(item.split(",")[1])
                        if depth < 5:
                            above_minimum_depth = False
                    if above_minimum_depth:
                        # Load alt and depth counts
                        first_alt_ref = items[1].split(",")
                        if int(first_alt_ref[0]) >= int(first_alt_ref[1]) / 2 \
                           and int(first_alt_ref[0]) >= 1:
                            f.write('1')
                        else:
                            f.write('0')
                        for other_alt_ref in items[2:]:
                            alt_ref = other_alt_ref.split(",")
                            if int(alt_ref[0]) >= int(alt_ref[1]) / 2 and \
                               int(alt_ref[0]) >= 1:
                                f.write(', 1')
                            else:
                                f.write(', 0')
                        f.write('\n')
            snp_file.close()


if __name__ == '__main__':
    ComputeSNPMatrix().main()
