#!/bin/bash
#$ -N compute_HMP_pi.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=5G
#$ -l h_rt=04:00:00
#$ -t 2-94

# HMP Data
# There are 94 species
path_to_data='../Data/microbiome_data/snps/'

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/HMP_species_list.txt

python compute_pi.py ${path_to_data}${species}/snps_depth.txt.bz2 ${path_to_data}${species}/snps_ref_freq.txt.bz2 ../Analysis/${species}/HMP/
