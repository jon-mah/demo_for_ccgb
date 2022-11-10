#!/bin/bash
#$ -N gzip
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=4:00:00
#$ -l highp
#$ -t 1-100

# 20 lines

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

# esearch -db nucleotide -query "${file}" | efetch -format fasta > fastq_MIDAS_intermediate/${file}.fasta
gzip fastq_MIDAS_intermediate/${file}.fastq
