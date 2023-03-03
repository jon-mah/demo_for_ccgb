#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:05:00
#$ -t 1-30
#$ -N compute_sfs_no_clade_control

# SGE_TASK_ID=1

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/good_species_list.txt

i=0
while read line;
do
   i=$((i+1))
   if [ $i -eq $SGE_TASK_ID ]
   then
      species=$line
   fi
done < ../Data/good_species_list.txt

python compute_sfs_no_clade_control.py ${species} ../Analysis/${species}_no_clade_control/
