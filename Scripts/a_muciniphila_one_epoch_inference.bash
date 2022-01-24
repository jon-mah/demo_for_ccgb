#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Akkermansia_muciniphila_55290_syn.sfs  ../Analysis/Akkermansia_muciniphila_55290/ --mask_singletons
