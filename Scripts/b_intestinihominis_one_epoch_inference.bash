#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_one_epoch.py ../Data/Barnesiella_intestinihominis_62208_syn.sfs  ../Analysis/Barnesiella_intestinihominis_62208/ --mask_singletons
