#!/bin/bash
#$ -cwd
#$ -V
#$ -N b_thetaiotamicron
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=36:00:00
#$ -l highp

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs  ../Analysis/Bacteroides_thetaiotaomicron_56941/ --mask_singletons
# python fit_one_epoch.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs  ../Analysis/Bacteroides_thetaiotaomicron_56941_no_mask/
