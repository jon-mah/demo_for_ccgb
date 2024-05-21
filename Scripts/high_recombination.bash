#!/bin/bash
#$ -cwd
#$ -V
#$ -l h_data=20G
#$ -l h_rt=2:00:00
#$ -t 1-30
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N fit_one_epoch_gut_accessory

species="Bacteroides_vulgatus_57955"
percentile=0.50

python return_high_recombination.py $species $percentile ../HighRecombinationAnalysis/
