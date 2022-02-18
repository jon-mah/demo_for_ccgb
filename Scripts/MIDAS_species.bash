#!/bin/bash
#$ -N MIDAS_species_long.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=10G
#$ -l time=00:10:00
#$ -t 1-339

# module load python/2.7
export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      file=$line
   fi
done < ../Data/oral_microbiome_data/file_list.txt

OUTDIR=../Data/oral_microbiome_data/${file}/
file_1=${OUTDIR}${file}.denovo_duplicates_marked.trimmed.1.fastq.gz
file_2=${OUTDIR}${file}.denovo_duplicates_marked.trimmed.2.fastq.gz

# module load singularity

# echo ${file_1}
# echo ${file_2}
# echo $OUTDIR

if [ -f "${OUTDIR}/species/species_profile.txt" ]
then
echo "File is found"
else
   # echo "File is not found"
   # singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2}  --remove_temp
   echo $SGE_TASK_ID >> species_errors.txt
fi
