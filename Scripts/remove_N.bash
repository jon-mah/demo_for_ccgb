#!/bin/bash
#$ -N summarize_pi.bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=1G
#$ -l h_rt=01:00:00
#$ -t 1

file=test
OUTDIR=../Data/oral_microbiome_data/${file}/
input_file=${OUTDIR}all_N_1.fastq.gz
output_file=${OUTDIR}removed_all_N_1.fastq.gz

python remove_N_from_fastq.py $input_file $output_file $OUTDIR
