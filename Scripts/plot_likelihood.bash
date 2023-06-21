#!/bin/bash
#$ -cwd
#$ -V
#$ -N plot_likelihood.bash
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=12:00:00
#$ -l highp
#$ -t 1-30

# SGE_TASK_ID=8

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
