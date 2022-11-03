#!/bin/bash
#$ -N construct_crude_sfs.bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l highp
#$ -l h_data=25G
#$ -l time=00:10:00
#$ -t 1-123

# SGE_TASK_ID=1

# i=0
# while read line;
#  do
#    i=$((i+1))
#    # echo $line
#    if [ $i -eq $SGE_TASK_ID ]
#    then
#       species=$line
#    fi
# done < oral_species_list.txt

i=0
while read line;
  do
    i=$((i+1))
    # echo $line
    if [ $i -eq $SGE_TASK_ID ]
    then
      species=$line
    fi
done < gut_sfs_list.txt

# python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ $species --min_depth 10 ../Data/oral_microbiome_sfs/${species}_sfs/
python construct_crude_sfs.py ../Data/microbiome_data/merged_data/snps/ $species --min_depth 10 ../Data/gut_microbiome_sfs/${species}_sfs/
