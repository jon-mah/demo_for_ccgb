#!/bin/bash
#$ -N construct_crude_sfs.bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l highp
#$ -l h_data=25G
#$ -l time=00:05:00

python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Veillonella_parvula_57794 --min_depth 2 ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/
