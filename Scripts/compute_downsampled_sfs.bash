#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

species=Veillonella_sp_62404

python compute_downsampled_sfs.py ${species} ../Analysis/${species}