#!/bin/bash
#$ -N construct_crude_sfs.bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l highp
#$ -l h_data=25G
#$ -l time=00:10:00

python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Veillonella_parvula_57794 --min_depth 10 ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Veillonella_parvula_58184 --min_depth 10 ../Data/oral_microbiome_data/Veillonella_parvula_58184_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_58558 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_58558_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Rothia_dentocariosa_57938 --min_depth 10 ../Data/oral_microbiome_data/Rothia_dentocariosa_57938_sfs/
