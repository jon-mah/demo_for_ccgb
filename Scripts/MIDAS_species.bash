#!/bin/bash
#$ -N MIDAS_species.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l highp
#$ -pe shared 8
#$ -l time=72:00:00
#$ -t 1-180

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
done < ./failed_species_list.txt

OUTDIR=../Data/oral_microbiome_data/fastq_MIDAS_intermediate/${file}

file_1=${OUTDIR}/${file}.denovo_duplicates_marked.trimmed.1.fastq.gz
file_2=${OUTDIR}/${file}.denovo_duplicates_marked.trimmed.2.fastq.gz

OUTDIR=../Data/oral_microbiome_data/midas_output/${file}/
# mkdir $OUTDIR

module load python/2.7.18
module load midas
. /u/local/apps/midas/1.3.2/python-2.7.18-MIDAS-VE/bin/activate

if [ ! -f "${OUTDIR}/species/species_profile.txt" ]
then
   echo "Species Profile file is not found"
   run_midas.py species ${OUTDIR}/ -1 ${file_1} -2 ${file_2} --remove_temp
fi

# run_midas.py species ${OUTDIR}/ -1 ${file_1} -2 ${file_2} --remove_temp

# module load singularity

# singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species ${OUTDIR}/removed/ -1 ${file_1} -2 ${file_2} --remove_temp
