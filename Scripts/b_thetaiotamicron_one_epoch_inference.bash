#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

# python fit_one_epoch.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs  ../Analysis/Bacteroides_thetaiotaomicron_56941/ --mask_singletons
python fit_one_epoch.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs  ../Analysis/Bacteroides_thetaiotaomicron_56941_no_mask/
