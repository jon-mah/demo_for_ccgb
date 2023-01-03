#!/bin/bash
#$ -N strep_mutans_midas
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=2:00:00
#$ -t 10-329

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

# run_midas.py species -h
# run_midas.py species ${OUTDIR}/ -1 fastq_MIDAS_intermediate/${file_name}.fasta.gz --remove_temp
run_midas.py species ${OUTDIR}/ -1 fastq_MIDAS_intermediate/${file_name}.fastq.gz --remove_temp

# module load singularity
# export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
# export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
# export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

# file_1=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS014530/SRS014530.denovo_duplicates_marked.trimmed.1.fastq.gz
# file_2=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS014530/SRS014530.denovo_duplicates_marked.trimmed.2.fastq.gz

# singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species $OUTDIR -1 fastq_MIDAS_intermediate/${file_name}.fasta.gz --remove_temp --extra_species_id Streptococcus_mutans_56116
# singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species -h
