#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l highp
#$ -l h_rt=00:10:00

python fit_one_epoch.py ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/folded_sfs_2.txt ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/2/
python fit_one_epoch.py ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/folded_sfs_3.txt ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/3/
python fit_one_epoch.py ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/folded_sfs_5.txt ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/5/
python fit_one_epoch.py ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/folded_sfs_25.txt ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/25/
