#!/bin/bash
#$ -N MIDAS_species.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=12G
#$ -l time=47:00:00
#$ -l highp
#$ -t 1-339

# 182, 276

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

# echo ${file_1}
# echo ${file_2}
# echo $OUTDIR

if [ -f "${OUTDIR}/species/species_profile.txt" ]
then
echo "File is found"
else
   echo "File is not found"
   singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2}
fi

# module load singularity
# singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2}
