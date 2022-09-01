#!/bin/bash
#$ -N merge_species.bash
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

OUTDIR=../Data/oral_microbiome_data/temp_merged_data
module load singularity
singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif merge_midas.py species $OUTDIR/species -i ../Data/oral_microbiome_data/temp_midas_output -t dir

# module load python/2.7.18
# module load midas
# . /u/local/apps/midas/1.3.2/python-2.7.18-MIDAS-VE/bin/activate

# run_midas.py species ${OUTDIR}/ -1 ${file_1} -2 ${file_2} --remove_temp

# merge_midas.py species $OUTDIR/species -i ../Data/oral_microbiome_data/midas_output -t dir
