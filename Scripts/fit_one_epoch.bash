#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l highp
#$ -l h_rt=00:10:00
#$ -t 25

# SGE_TASK_ID=25

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/good_species_list.txt

# Downsampled analysis
python fit_one_epoch.py ../Analysis/${species}_downsampled/empirical_sfs.txt ../Analysis/${species}_downsampled/ --mask_singletons

# Original analysis
# python fit_one_epoch.py ../Data/${species}_syn.sfs ../Analysis/${species}/ --mask_singletons
