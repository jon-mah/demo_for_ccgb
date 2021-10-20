#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model.py ../Data/Phascolarctobacterium_sp_59817_syn.sfs  ../Analysis/Phascolarctobacterium_sp_59817/ --mask_singletons
