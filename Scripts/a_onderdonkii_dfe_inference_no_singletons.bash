#!/bin/bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l time=01:00:00
#$ -pe shared 4

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model_and_DFE.py ../Data/Alistipes_onderdonkii_55464_syn.sfs ../Data/Alistipes_onderdonkii_55464_nonsyn.sfs ../Analysis/Alistipes_onderdonkii_55464_no_singletons/ --mask_singletons
