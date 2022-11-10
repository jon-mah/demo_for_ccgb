#!/bin/bash
#$ -N edirect
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=00:20:00
#$ -l highp
#$ -t 1-48
#$ -tc 1

# 333 Lines

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      file=$line
   fi
done < ../streptococcus_mutans_genbank_accessions.txt

esearch -db nucleotide -query "${file}" | efetch -format fasta > fastq_MIDAS_intermediate/${file}.fasta
