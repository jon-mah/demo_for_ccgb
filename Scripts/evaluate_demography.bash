#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:10:00

# This script infers the demography of a given example synonymous sfs.

python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt two_epoch 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_14/
