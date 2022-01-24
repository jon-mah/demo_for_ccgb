#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Bacteroides_caccae_53434_syn.sfs  ../Analysis/Bacteroides_caccae_53434/ --mask_singletons
