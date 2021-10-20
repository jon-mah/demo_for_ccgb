#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model.py ../Data/Alistipes_shahii_62199_syn.sfs ../Analysis/Alistipes_Alistipes_shahii_62199/ --mask_singletons
