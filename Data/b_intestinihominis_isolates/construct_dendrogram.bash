#!/bin/bash
#$ -N construct_dendrogram.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V # Same environment
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l time=4:00:00
#$ -l highp

# Set the input and output files
ALIGNMENT_FILE="output.delta"
DISTANCE_FILE="distances.txt"
TREE_FILE="tree.phylip"
NEWICK_FILE="tree.newick"

# Calculate the pairwise distances between sequences using dnadist
dnadist -data $ALIGNMENT_FILE > $DISTANCE_FILE

# Build the tree using neighbor-joining method
neighbor -data $DISTANCE_FILE -outtree $TREE_FILE -method nj

# Write the tree to a file in Newick format using the -trout  option
drawtree -trout $TREE_FILE > $NEWICK_FILE
