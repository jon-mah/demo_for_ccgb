#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=36:00:00
#$ -l highp

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Parabacteroides_merdae_56972_syn.sfs  ../Analysis/Parabacteroides_merdae_56972/ --mask_singletons
# python fit_one_epoch.py ../Data/Parabacteroides_merdae_56972_syn.sfs  ../Analysis/Parabacteroides_merdae_56972_no_mask/
