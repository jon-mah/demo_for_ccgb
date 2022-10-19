#!/bin/bash
#$ -cwd
#$ -V
#$ -N plot_likelihood
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00
#$ -l highp
#$ -t 1-6

# SGE_TASK_ID=1

# This script infers the demography of a given example synonymous sfs.

i=0
while read line;
 do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]; then
       file=$line
   fi
done < ./likelihood_surface_list.txt

python $file
