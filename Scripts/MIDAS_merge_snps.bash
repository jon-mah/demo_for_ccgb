#!/bin/bash
#$ -N merge_snps.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=15G
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l time=48:00:00
#$ -l highp

# module load python/2.7
export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

OUTDIR=../Data/oral_microbiome_data/merged_data
module load singularity
singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif merge_midas.py snps $OUTDIR/snps -i ../Data/oral_microbiome_data/midas_output -t dir --sample_depth 5 --site_depth 3 --min_samples 1 --site_prev 0.0

# merge_midas.py snps $OUTDIR/snps -i ../Data/oral_microbiome_data/midas_output -t dir –sample_depth 5 –site_depth 3 –min_samples 1 –site_prev 0.0
