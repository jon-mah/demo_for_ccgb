#!/bin/bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=10G
#$ -l time=01:00:00
#$ -pe shared 4

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model_and_DFE.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs ../Data/Bacteroides_thetaiotaomicron_56941_nonsyn.sfs ../Analysis/Bacteroides_thetaiotamicron_56491_no_singletons/ --mask_singletons
