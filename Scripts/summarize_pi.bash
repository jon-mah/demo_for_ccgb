#!/bin/bash
#$ -N summarize_pi.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=1G
#$ -l h_rt=01:00:00
#$ -t 161-165

# There are 165 species
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
python summarize_pi.py ${path_to_data}${species}/HMP/output_matrix.csv ${species} HMP ../Analysis/

# African data
python summarize_pi.py ${path_to_data}${species}/African/output_matrix.csv ${species} African ../Analysis/
