#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python downsample_sfs.py ../Data/Alistipes_finegoldii_56071_syn.sfs  ../Analysis/Alistipes_finegoldii_56071/ 25 --mask_singletons
