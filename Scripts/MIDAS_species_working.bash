#!/bin/bash
#$ -N MIDAS_species_working.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=10G
#$ -l highp
#$ -l time=12:00:00

# You need to define your own out directory
OUTDIR=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS893362/temp
# You need to define your own out directory

# module load python/2.7
# module load singularity
export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

file_1=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS893362/SRS893362.denovo_duplicates_marked.trimmed.1.fastq.gz
file_2=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS893362/SRS893362.denovo_duplicates_marked.trimmed.2.fastq.gz

singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2}  --remove_temp

