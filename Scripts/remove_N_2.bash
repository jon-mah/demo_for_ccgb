#!/bin/bash
#$ -N remove_N.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=50G
#$ -l highp
#$ -l h_rt=04:00:00
#$ -t 1-60

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      file_prefix=$line
   fi
done < ./failed_species_list.txt

OUTDIR=../Data/oral_microbiome_data/fastq_MIDAS_intermediate/${file_prefix}/

file_extension_1='.denovo_duplicates_marked.trimmed.1.fastq.gz'
input_1=${OUTDIR}${file_prefix}${file_extension_1}
output_1=${OUTDIR}removed_${file_prefix}${file_extension_1}
file_extension_2='.denovo_duplicates_marked.trimmed.2.fastq.gz'
input_2=${OUTDIR}${file_prefix}${file_extension_2}
output_2=${OUTDIR}removed_${file_prefix}${file_extension_2}

# python remove_N_from_fastq.py $input_1 $output_1 $OUTDIR
python remove_N_from_fastq.py $input_2 $output_2 $OUTDIR

# OUTDIR=../Data/oral_microbiome_data/MIDAS_output/${file_prefix}

# file=SRS014530

# OUTDIR=../Data/oral_microbiome_data/test/${file}/
# input_file=${OUTDIR}/${file}.denovo_duplicates_marked.trimmed.1.fastq.gz
# output_file=${OUTDIR}/removed_${file}.denovo_duplicates_marked.trimmed.1.fastq.gz

# python remove_N_from_fastq.py $input_file $output_file $OUTDIR

# input_file=${OUTDIR}/${file}.denovo_duplicates_marked.trimmed.2.fastq.gz
# output_file=${OUTDIR}/removed_${file}.denovo_duplicates_marked.trimmed.2.fastq.gz

# python remove_N_from_fastq.py $input_file $output_file $OUTDIR

