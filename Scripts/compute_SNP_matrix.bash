#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=1G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python compute_SNP_matrix.py ../Data/microbiome_data/snps/Phascolarctobacterium_succinatutens_61948/annotated_snps.txt.bz2 ../Data/example_output/
