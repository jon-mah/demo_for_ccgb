""" Constructs and SFS from consensus alleles for given oral data.

JCM 20221017
"""


import sys
import os
import logging
import time
import argparse
import warnings

import numpy
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


class ConstructCrudeSFS():
    """Wrapper class to allow functions to reference each other."""

    def ExistingFile(self, fname):
        """Return *fname* if existing file, otherwise raise ValueError."""
        if os.path.isfile(fname):
            return fname
        else:
            raise ValueError("%s must specify a valid file name" % fname)

    def fitDemographicModelParser(self):
        """Return *argparse.ArgumentParser* for ``fitdadi_infer_DFE.py``."""
        parser = ArgumentParserNoArgHelp(
            description=(
                'This script constructs a crude SFS using consensus alleles.'),
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser.add_argument(
            'inprefix', type=str,
            help=('The file prefix for the input files.'))
        parser.add_argument(
            'species', type=str,
            help=('The species for which we are constructing an SFS.'))
        parser.add_argument(
            '--min_depth', type=int,
            help=('The minimum depth necessary to call a consensus allele'),
            default=2)
        parser.add_argument(
            'outprefix', type=str,
            help='The file prefix for the output files.')
        return parser

    def main(self):
        """Execute main function."""
        # Parse command line arguments
        parser = self.fitDemographicModelParser()
        args = vars(parser.parse_args())
        prog = parser.prog

        # Assign arguments
        inprefix = args['inprefix']
        species = args['species']
        min_depth = args['min_depth']
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

        indir = os.path.dirname(args['inprefix'])
        if indir:
            if not os.path.isdir(indir):
                if os.path.isfile(indir):
                    os.remove(indir)
                os.mkdir(indir)

        # Output files: logfile
        # Remove output files if they already exist
        underscore = '' if args['outprefix'][-1] == '/' else '_'
        logfile = '{0}{1}log.log'.format(args['outprefix'], underscore)
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

        # Grab species-specific data from input directory
        snps_depth = '{0}{1}/snps_depth.txt'.format(args['inprefix'], species)
        snps_alt_allele = '{0}{1}/snps_alt_allele.txt'.format(args['inprefix'], species)
        depth_handle = open(snps_depth, 'r')
        alt_handle = open(snps_alt_allele, 'r')
        depth_header = depth_handle.readline()
        num_hosts = len(depth_header.split('\t')) - 1
        alt_header = alt_handle.readline()
        n_tons = [0] * (num_hosts + 1)
        iter = 0
        while True:
            # Read line by line
            depth_line = depth_handle.readline()
            if not depth_line:
                break
            else:
                alt_line = alt_handle.readline()
                depth_elements = depth_line.split('\t')
                if depth_elements[0] == 'site_id':
                    print('Error.')
                    break
                else:
                    iter = iter + 1
                    if iter > 100:
                        break
                    alt_elements = alt_line.split('\t')
                    if alt_elements[0] != depth_elements[0]:
                        break
                    A_counter = 0
                    C_counter = 0
                    G_counter = 0
                    T_counter = 0
                    for i in range(num_hosts):
                        if int(depth_elements[i+1]) < min_depth:
                            continue
                        else:
                            if alt_elements[i+1] == 'NA':
                                continue
                            elif alt_elements[i+1] == 'A':
                                A_counter += 1
                            elif alt_elements[i+1] == 'C':
                                C_counter += 1
                            elif alt_elements[i+1] == 'G':
                                G_counter += 1
                            elif alt_elements[i+1] == 'T':
                                T_counter += 1
                            else:
                                # Something weird is happening
                                continue
                        if A_counter + C_counter + G_counter + T_counter == 0:
                            n_tons[0] += 1
                        if A_counter != 0:
                            n_tons[A_counter] += 1
                        if C_counter != 0:
                            n_tons[C_counter] += 1
                        if G_counter != 0:
                            n_tons[G_counter] += 1
                        if T_counter != 0:
                            n_tons[T_counter] += 1
                    print(depth_line)
                    print(alt_line)
        depth_handle.close()
        alt_handle.close()
        print(n_tons)

if __name__ == '__main__':
    ConstructCrudeSFS().main()
