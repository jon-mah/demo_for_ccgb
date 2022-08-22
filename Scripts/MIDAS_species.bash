#!/bin/bash
#$ -N MIDAS_species.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=02:00:00
#$ -t 1-60
# module load python/2.7
export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

# SGE_TASK_ID=3

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

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      working_file=$line
   fi
done < ./working_species_list.txt

# echo $file

OUTDIR=../Data/oral_microbiome_data/fastq_MIDAS_intermediate/${file}

file_1=${OUTDIR}/removed_${file}.denovo_duplicates_marked.trimmed.1.fastq.gz
file_2=${OUTDIR}/removed_${file}.denovo_duplicates_marked.trimmed.2.fastq.gz

working_file_output=../Data/oral_microbiome_data/midas_output/${working_file}/species/species_profile.txt

OUTDIR=../Data/oral_microbiome_data/midas_output/${file}/
# mkdir $OUTDIR

module load python/2.7.18
module load midas
. /u/local/apps/midas/1.3.2/python-2.7.18-MIDAS-VE/bin/activate

if [ ! -f "${OUTDIR}/species/species_profile.txt" ]
then
   echo "Species Profile file is not found"
   # run_midas.py species ${OUTDIR}/ -1 ${file_1} -2 ${file_2} --remove_temp
   touch temp_failed_species_list.txt
   echo ${file} >> ./temp_failed_species_list.txt
fi
