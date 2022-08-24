#!/bin/bash
#$ -N construct_species_union
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=15G
#$ -l h_rt=2:00:00
#$ -t 2-340

# This script computes the species union of a given host

# SGE_TASK_ID=2

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]; then
      file=$(echo $line | cut -d ',' -f 1)
      echo $file
   fi
done < ../HMP1-2_oral_host_srs_list.csv

python3 construct_species_union.py ../HMP1-2_oral_host_srs_list.csv  $file
