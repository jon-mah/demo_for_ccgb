#!/bin/bash
#$ -N MUMmer_msa.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=4:00:00
#$ -l highp

# Set the path to the MUMmer tools
# MUMMER_PATH="/path/to/mummer/bin"

# Set the input FASTQ files
REFERENCE_GENOME="./fasta_MUMmer/reference.fasta"
QUERY_GENOME="./fasta_MUMmer/query.fasta"

# REFERENCE_GENOME="./fasta_MUMmer/reference.fasta"
# QUERY_GENOME="./fasta_MUMmer/ERR105098.fasta"

# Run nucmer to align the genomes
nucmer -p output $REFERENCE_GENOME $QUERY_GENOME

# Run show-coords to view the alignment coordinates
show-coords -r output.delta > ./MUMmer_output/output.coords
