#!/bin/bash
#$ -N dnadiff
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=30G
#$ -l time=2:00:00

REFERENCE_GENOME="../fasta_MUMmer/reference.fasta"
QUERY_GENOME="../fasta_MUMmer/temp_query.fasta"
# QUERY_GENOME=$file_name
# DELTA=${OUTDIR}.delta

# Run nucmer to align the genomes
# nucmer -p $OUTDIR $REFERENCE_GENOME $QUERY_GENOME
# dnadiff -p $OUTDIR -d $DELTA
# dnadiff -p $OUTDIR $REFERENCE_GENOME $QUERY_GENOME
dnadiff $REFERENCE_GENOME $QUERY_GENOME
