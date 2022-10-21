#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l highp
#$ -l h_rt=00:10:00

python fit_one_epoch.py ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/folded_sfs_10.txt ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/
python fit_one_epoch.py ../Data/oral_microbiome_data/Veillonella_parvula_58184_sfs/folded_sfs_10.txt ../Data/oral_microbiome_data/Veillonella_parvula_58184_sfs/
python fit_one_epoch.py ../Data/oral_microbiome_data/Streptococcus_mitis_58558_sfs/folded_sfs_10.txt ../Data/oral_microbiome_data/Streptococcus_mitis_58558_sfs/
python fit_one_epoch.py ../Data/oral_microbiome_data/Rothia_dentocariosa_57938_sfs/folded_sfs_10.txt ../Data/oral_microbiome_data/Rothia_dentocariosa_57938_sfs/
