#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:05:00
#$ -t 1-30
#$ -N compute_empirical_sfs

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

python compute_empirical_sfs.py ../Analysis/${species}_downsampled_10/syn_sfs.csv ../Analysis/${species}_downsampled_10/syn
python compute_empirical_sfs.py ../Analysis/${species}_downsampled_10/nonsyn_sfs.csv ../Analysis/${species}_downsampled_10/nonsyn
