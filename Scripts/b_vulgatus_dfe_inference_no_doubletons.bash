#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=03:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model_and_DFE.py ../Data/Bacteroides_vulgatus_57955_syn.sfs ../Data/Bacteroides_vulgatus_57955_nonsyn.sfs ../Data/Bacteroides_vulgatus_57955_nonsyn.sfs ../Analysis/Bacteroides_vulgatus_57955_no_doubletons/ --mask_singletons --mask_doubletons
