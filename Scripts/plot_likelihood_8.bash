#!/bin/bash
#$ -cwd
#$ -V
#$ -N complete_downsample_14_2_1_5
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00
#$ -t 1-30

# SGE_TASK_ID=1

# i=0
# while read line;
#   do
#     i=$((i+1))
#     # echo $line
#     if [ $i -eq $SGE_TASK_ID ]; then
#         species=$line
#     fi
# done < ./oral_sfs_list.txt

# python plot_likelihood.py ../Data/oral_microbiome_sfs/${species}_sfs/folded_sfs_10.txt 0.1 1.0 ../Analysis/${species}_oral/masked/ --mask_singletons

# Oral with singletons
# python plot_likelihood.py ../Data/oral_microbiome_sfs/${species}_sfs/folded_sfs_10.txt 0.1 1.0 ../Analysis/${species}_oral/unmasked/


# i=0
# while read line;
#   do
#     i=$((i+1))
#     # echo $line
#     if [ $i -eq $SGE_TASK_ID ]; then
#         species=$line
#     fi
# done < ./gut_sfs_list.txt

# Gut with singletons
# python plot_likelihood.py ../Analysis/${species}_downsampled_14/empirical_sfs.txt 1.0 0.005 ../Analysis/${species}_downsampled_14/likelihood_surface_unmasked/

# i=0
# while read line;
#   do
#     i=$((i+1))
#     # echo $line
#     if [ $i -eq $SGE_TASK_ID ]; then
#         species=$line
#     fi
# done < ./isolate_sfs_list.txt

# UHGG Isolates
# python plot_likelihood.py ../Data/UHGG/UHGG_${species}/downsampled_sfs.txt 0.5 10 ../Data/UHGG/UHGG_${species}/


# Complete SFS
i=0
while read line;
  do
    i=$((i+1))
    # echo $line
    if [ $i -eq $SGE_TASK_ID ]; then
        species=$line
    fi
done < ../Data/good_species_list.txt

python plot_likelihood.py ../Analysis/${species}_downsampled_14/empirical_syn_downsampled_sfs.txt 2.0 1.5 ../Analysis/${species}_downsampled_14/8
