#!/bin/bash
#$ -cwd
#$ -V
#$ -N complete_downsample_14_1_5_100
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00
#$ -t 1

SGE_TASK_ID=1

# Complete SFS
i=0
while read line;
  do
    i=$((i+1))
    # echo $line
    if [ $i -eq $SGE_TASK_ID ]; then
        species=$line
    fi
done < ../Data/good_species_list.txt

python plot_likelihood.py ../Analysis/${species}_downsampled_14/empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_14/complete_two_epoch_demography.txt ../Analysis/${species}_downsampled_14/
