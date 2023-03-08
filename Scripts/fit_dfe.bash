#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l highp
#$ -l h_rt=00:30:00
#$ -t 1-30
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N downsample_14_one_epoch

SGE_TASK_ID=1

i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
   then
     species=$line
  fi
done < ../Data/good_species_list.txt

python fit_dfe.py ../Analysis/${species}_downsampled_14/empirical_sfs.txt ../Analysis/${species}_downsampled_14/empirical_nonsyn_sfs.txt ../Analysis/${species}_downsampled_14/two_epoch_demography.txt ../Analysis/${species}_downsampled_14/
