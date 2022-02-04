#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Ruminococcus_bromii_62047_syn.sfs ../Analysis/Ruminococcus_bromii_62047/ --mask_singletons

# two_epoch and exponential
# upper_bound = [8, 0.00025]
# lower_bound = [0, 0]
# initial_guess = [0.1, 0.00005]

