#!/bin/bash
#$ -cwd
#$ -V
#$ -l h_data=25G
#$ -l h_rt=12:00:00
#$ -l highp
#$ -t 29-30
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N fit_one_epoch_gut_HMP

# SGE_TASK_ID=1

# i=0
# while read line;
# do
#    i=$((i+1))
#    if [ $i -eq $SGE_TASK_ID ]
#     then
#       species=$line
#    fi
# done < ../Data/good_species_list.txt

# i=0
# while read line;
# do
#   i=$((i+1))
#   if [ $i -eq $SGE_TASK_ID ]
#    then
#      species=$line
#   fi
# done < ./oral_sfs_list.txt

# i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
    then
      species=$line
  fi
done < ./gut_sfs_list.txt

# i=0
# while read line;
# do
#   i=$((i+1))
#   if [ $i -eq $SGE_TASK_ID ]
#    then
#      species=$line
#   fi
# done < ./isolate_sfs_list.txt

# Consensus gut one epoch
# python fit_one_epoch.py ../Data/gut_microbiome_sfs/${species}_sfs/folded_sfs_10.txt ../Data/gut_microbiome_sfs/${species}_sfs/masked/ --mask_singletons

# python fit_one_epoch.py ../Data/gut_microbiome_sfs/${species}_sfs/folded_sfs_10.txt ../Data/gut_microbiome_sfs/${species}_sfs/unmasked/

# oral one epoch
# python fit_one_epoch.py ../Data/oral_microbiome_sfs/${species}_sfs/folded_sfs_10.txt ../Analysis/${species}_oral/masked/ --mask_singletons

# python fit_one_epoch.py ../Data/oral_microbiome_sfs/${species}_sfs/folded_sfs_10.txt ../Analysis/${species}_oral/unmasked/

# Downsampled analysis
# python fit_one_epoch.py ../Analysis/${species}_downsampled_10/empirical_sfs.txt ../Analysis/${species}_downsampled_10/
# python fit_one_epoch.py ../Analysis/${species}_downsampled_12/empirical_sfs.txt ../Analysis/${species}_downsampled_12/
python fit_one_epoch.py ../Analysis/${species}_downsampled_14/empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_14/complete
# python fit_one_epoch.py ../Analysis/${species}_downsampled_16/empirical_sfs.txt ../Analysis/${species}_downsampled_16/
# python fit_one_epoch.py ../Analysis/${species}_downsampled_18/empirical_sfs.txt ../Analysis/${species}_downsampled_18/

# Original analysis
# python fit_one_epoch.py ../Data/${species}_syn.sfs ../Analysis/${species}/ --mask_singletons

# UHGG Isolates
# python fit_one_epoch.py ../Data/UHGG/UHGG_Bacteroides_A_coprocola/full_output_sfs.txt ../Data/UHGG/UHGG_Bacteroides_A_coprocola/
# python fit_one_epoch.py ../Data/UHGG/UHGG_Bacteroides_eggerthii/full_output_sfs.txt ../Data/UHGG/UHGG_Bacteroides_eggerthii/
# python fit_one_epoch.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt ../Data/UHGG/UHGG_Bacteroides_fragilis/
# python fit_one_epoch.py ../Data/UHGG/UHGG_Bacteroides_stercoris/full_output_sfs.txt ../Data/UHGG/UHGG_Bacteroides_stercoris/
# python fit_one_epoch.py ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/full_output_sfs.txt ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/
# python fit_one_epoch.py ../Data/UHGG/UHGG_${species}/downsampled_sfs.txt ../Data/UHGG/UHGG_${species}/
