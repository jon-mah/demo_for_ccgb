#!/bin/bash
#$ -cwd
#$ -V
#$ -N plot_likelihood
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=01:30:00
#$ -l highp
#$ -t 1-23

# SGE_TASK_ID=1

i=0
while read line;
  do
    i=$((i+1))
    # echo $line
    if [ $i -eq $SGE_TASK_ID ]; then
        species=$line
    fi
done < ./oral_sfs_list.txt

plot_likelihood.py ../Data/oral_microbiome_sfs/${species}_sfs/folded_sfs_10.txt 1.0 1.0 ../Analysis/${species}_oral/masked/ --mask_singletons
