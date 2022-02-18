#!/bin/bash
#$ -N failing_N_test.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=10G
#$ -l time=00:20:00

# module load python/2.7
export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

file=test
OUTDIR=../Data/oral_microbiome_data/${file}/

# one_N

# file_1=${OUTDIR}one_N_1.fastq.gz
# file_2=${OUTDIR}one_N_2.fastq.gz

# 20_percent_N

# file_1=${OUTDIR}20_percent_N_1.fastq.gz
# file_2=${OUTDIR}20_percent_N_2.fastq.gz

# 50_percent_N

# file_1=${OUTDIR}50_percent_N_1.fastq.gz
# file_2=${OUTDIR}50_percent_N_2.fastq.gz

# 90_percent_N

# file_1=${OUTDIR}90_percent_N_1.fastq.gz
# file_2=${OUTDIR}90_percent_N_2.fastq.gz

# 100_percent_N

# file_1=${OUTDIR}100_percent_N_1.fastq.gz
# file_2=${OUTDIR}100_percent_N_2.fastq.gz

# all_N

# file_1=${OUTDIR}all_N_1.fastq.gz
# file_2=${OUTDIR}all_N_2.fastq.gz

# large_N

file_1=${OUTDIR}large_N_1.fastq.gz
file_2=${OUTDIR}large_N_2.fastq.gz

# working_large_N

# file_1=${OUTDIR}working_large_N_1.fastq.gz
# file_2=${OUTDIR}working_large_N_2.fastq.gz

singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2} --remove_temp
