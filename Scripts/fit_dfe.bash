#!/bin/bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=25G
#$ -l h_rt=02:00:00
#$ -t 1
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N fit_dfe

SGE_TASK_ID=19
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

sample_size=14

# python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/one_epoch_demography.txt ../Analysis/${species}_downsampled_${sample_size}/
python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/complete_two_epoch_demography.txt ../Analysis/${species}_downsampled_${sample_size}/
# python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/complete_three_epoch_demography.txt ../Analysis/${species}_downsampled_${sample_size}/
