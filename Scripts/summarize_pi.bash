#!/bin/bash
#$ -N compute_HMP_pi.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=5G
#$ -l h_rt=02:00:00
#$ -t 1

# There are 94 species
path_to_data='../Analysis/'

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/all_species_list.txt

# HMP data
python summarize_pi.py ${path_to_data}${species}/HMP/output_matrix.csv ../Analysis/

# African data
python summarize_pi.py ${path_to_data}${speces}/African/output_matrix.csv ../Analysis/
