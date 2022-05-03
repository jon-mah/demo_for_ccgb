#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=36:00:00
#$ -l highp

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Akkermansia_muciniphila_55290_syn.sfs  ../Analysis/Akkermansia_muciniphila_55290/ --mask_singletons
# python fit_one_epoch.py ../Data/Akkermansia_muciniphila_55290_syn.sfs  ../Analysis/Akkermansia_muciniphila_55290_no_mask/
