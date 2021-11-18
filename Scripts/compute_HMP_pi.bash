#!/bin/bash
#$ -N compute_HMP_pi.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=5G
#$ -l h_rt=02:00:00
#$ -t 2-94

# HMP Data
# There are 94 species
path_to_data='../Data/microbiome_data/snps/'

# species='Akkermansia_muciniphila_5529
# species='Clostridium_nexile_61654'
# species='Veillonella_atypica_58169'


# African data
# path_to_data='/u/project/ngarud/Garud_lab/BIG_2021_microbiome_evolution/data/snps_just_africa/'

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
