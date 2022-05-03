#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=36:00:00
#$ -l highp

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Parabacteroides_distasonis_56985_syn.sfs  ../Analysis/Parabacteroides_distasonis_56985/ --mask_singletons
# python fit_one_epoch.py ../Data/Parabacteroides_distasonis_56985_syn.sfs  ../Analysis/Parabacteroides_distasonis_56985_no_mask/
