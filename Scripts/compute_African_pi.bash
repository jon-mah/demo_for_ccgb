#!/bin/bash
#$ -N compute_African_pi.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=5G
#$ -l h_rt=12:00:00
#$ -t 1-128

# African data
# 128 species
path_to_data='/u/project/ngarud/Garud_lab/BIG_2021_microbiome_evolution/data/snps_just_africa/'

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/African_species_list.txt

python compute_pi.py ${path_to_data}${species}/snps_depth.txt.bz2 ${path_to_data}${species}/snps_ref_freq.txt.bz2 ../Analysis/${species}/African/
