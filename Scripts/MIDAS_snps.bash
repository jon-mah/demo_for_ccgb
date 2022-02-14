
#!/bin/bash
#$ -N MIDAS_snps.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=15G
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l highp
#$ -l time=24:00:00
#$ -t 1-379

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

if [ ! -d $OUTDIR/snps ] 
then
    mkdir $OUTDIR/snps
fi


file_1=${OUTDIR}${file}.denovo_duplicates_marked.trimmed.1.fastq.gz
file_2=${OUTDIR}${file}.denovo_duplicates_marked.trimmed.2.fastq.gz


# echo ${file_1}
# echo ${file_2}
# echo $OUTDIR

if [ -f "${OUTDIR}/species/species_profile.txt" ]
then
   echo "Species Profile file is found"
   singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py snps $OUTDIR -1 ${file_1} -2 ${file_2}  --remove_temp
else
   echo "Species Profile file is not found"
fi