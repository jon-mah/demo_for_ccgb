#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=36:00:00
#$ -l highp

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Barnesiella_intestinihominis_62208_syn.sfs  ../Analysis/Barnesiella_intestinihominis_62208/ --mask_singletons
# python fit_one_epoch.py ../Data/Barnesiella_intestinihominis_62208_syn.sfs  ../Analysis/Barnesiella_intestinihominis_62208_no_mask/
