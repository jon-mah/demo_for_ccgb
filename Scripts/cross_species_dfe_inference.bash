#!/bin/bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=25G
#$ -l h_rt=12:00:00
#$ -l highp
#$ -t 1-30
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N cross_species_inference

# SGE_TASK_ID=1

i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
   then
     species=$line
  fi
done < ../Data/good_species_list.txt

python cross_species_dfe_inference.py ../Analysis/${species}_downsampled_14/complete_two_epoch_demography.txt ../Analysis/${species}_downsampled_14/empirical_nonsyn_downsampled_sfs ../Analysis/bacteroides_dfe/
