#!/bin/bash
#$ -N strep_mutans_midas_genes
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=02:00:00
#$ -t 1-14

# 14 lines

# SGE_TASK_ID=1

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      file_name=$line
   fi
done < ./SraAccList.txt

OUTDIR=./midas_output/${file_name}

module load python/2.7.18
module load midas
. /u/local/apps/midas/1.3.2/python-2.7.18-MIDAS-VE/bin/activate

run_midas.py genes ${OUTDIR}/ -1 fastq_MIDAS_intermediate/${file_name}.fastq.gz --species_id Streptococcus_mutans_56116 --remove_temp
