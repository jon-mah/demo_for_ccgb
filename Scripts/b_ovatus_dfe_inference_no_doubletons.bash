#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=03:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model_and_DFE.py ../Data/Bacteroides_ovatus_58035_syn.sfs ../Data/Bacteroides_ovatus_58035_nonsyn.sfs ../Analysis/Bacteroides_ovatus_58035_no_doubletons/ --mask_singletons --mask_doubletons
