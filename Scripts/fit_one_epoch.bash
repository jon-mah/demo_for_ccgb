#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l highp
#$ -l h_rt=00:10:00
#$ -t 25

# SGE_TASK_ID=25

# i=0
# while read line;
# do
#    i=$((i+1))
#    if [ $i -eq $SGE_TASK_ID ]
#     then
#       species=$line
#    fi
# done < ../Data/good_species_list.txt

i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
   then
     species=$line
  fi
done < ../Data/oral_sfs_list.txt

# oral one epoch
python fit_one_epoch.py ../Data/oral_microbiome_sfs/${species}_sfs/folded_sfs_10.txt ../Analysis/${species}_oral/masked --mask_singletons

# Downsampled analysis
# python fit_one_epoch.py ../Analysis/${species}_downsampled/empirical_sfs.txt ../Analysis/${species}_downsampled/ --mask_singletons

# python fit_one_epoch.py ../Data/aina_folded_synonymous_sfs.txt ../Analysis/aina_test/

# Original analysis
# python fit_one_epoch.py ../Data/${species}_syn.sfs ../Analysis/${species}/ --mask_singletons
