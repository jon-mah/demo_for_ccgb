#!/bin/bash
#$ -N strep_mutans_midas_genes
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=02:00:00
#$ -l highp

python ../../Scripts/parse_snps.py MUMmer_output/out.snps MUMmer_output/
python ../../Scripts/parse_snps.py MUMmer_temp_output/out.snps MUMmer_temp_output/
