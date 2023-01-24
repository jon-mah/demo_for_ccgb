#!/bin/bash
#$ -N parseOut.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=50G
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l time=12:00:00
#$ -l highp

#  parseOut output.parse names.txt output.dist 1206233
# parseOut output.parse names.txt output.dist 500
parseOut output.parse temp_names.txt output.dist 27
