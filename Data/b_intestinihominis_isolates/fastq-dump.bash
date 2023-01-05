#!/bin/bash
#$ -N fastq-dump
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=15G
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l time=2:00:00
#$ -t 1-14

# SGE_TASK_ID=1

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      file=$line
   fi
done < ./SraAccList.txt

fastq-dump $file --gzip -O ./fastq_MIDAS_intermediate
