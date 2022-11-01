#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=00:10:00
#$ -t 2

SGE_TASK_ID=30

i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
    then
      cat ../Analysis/gut_contraction_surfaces/${line} - > ../Analysis/gut_likelihood_surfaces/${line} ;
  fi
done < ../Analysis/temp_file_list.txt
