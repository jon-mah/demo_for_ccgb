#!/bin/bash
#$ -cwd
#$ -V
#$ -N gut_downsample_18
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=4:00:00
#$ -l highp
#$ -t 1-30

# SGE_TASK_ID=21

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


i=0
while read line;
  do
    i=$((i+1))
    # echo $line
    if [ $i -eq $SGE_TASK_ID ]; then
        species=$line
    fi
done < ./gut_sfs_list.txt

# Gut without singletons

# Gut with singletons
python plot_likelihood.py ../Analysis/${species}_downsampled_18/empirical_sfs.txt 1.0 1.0 ../Analysis/${species}_downsampled_18/likelihood_surface_unmasked/
