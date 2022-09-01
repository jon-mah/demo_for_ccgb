#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l h_rt=2:00:00
#$ -t 1-30

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/good_species_list.txt

python compute_downsampled_sfs.py ${species} ../Analysis/${species}_downsampled/ 20
