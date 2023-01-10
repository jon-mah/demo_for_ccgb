#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=2:00:00
#$ -t 1-329

# 329 lines

# SGE_TASK_ID=5

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

# mkdir $file
seqtk seq -a fastq_MIDAS_intermediate/${file}.fastq > fasta_MUMmer/${file}.fasta
# seqtk seq -a ${file}.fastq ../fasta_MUMmer/$file}.fasta
