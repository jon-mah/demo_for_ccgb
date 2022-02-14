#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

# python fit_one_epoch.py ../Data/Parabacteroides_distasonis_56985_syn.sfs  ../Analysis/Parabacteroides_distasonis_56985/ --mask_singletons
python fit_one_epoch.py ../Data/Parabacteroides_distasonis_56985_syn.sfs  ../Analysis/Parabacteroides_distasonis_56985_no_mask/