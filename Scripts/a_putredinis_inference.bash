#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=1G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model.py ../Data/Alistipes_putredinis_61533_syn.sfs ../Analysis/Alistipes_putredinis_61533/
