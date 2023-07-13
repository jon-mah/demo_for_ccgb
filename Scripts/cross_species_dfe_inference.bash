#!/bin/bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=25G
#$ -l h_rt=12:00:00
#$ -l highp
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N cross_species_inference

# SGE_TASK_ID=1

python cross_species_dfe_inference.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/complete_two_epoch_demography.txt ../Analysis/Bacteroides_stercoris_56735_downsampled_14/complete_two_epoch_demography.txt  ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_nonsyn_downsampled_sfs.txt ../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_nonsyn_downsampled_sfs.txt ../Analysis/bacteroides_dfe/
