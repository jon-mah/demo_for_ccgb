#!/bin/bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=15G
#$ -l h_rt=02:00:00
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N compare_dfe

python compare_dfe.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/inferred_DFE.txt ../Analysis/Bacteroides_fragilis_54507_downsampled_14/inferred_DFE.txt ../Analysis/Bacteroides_genus_DFE_comparison/
