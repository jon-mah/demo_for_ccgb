#!/bin/bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=1G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model.py ../Data/example.sfs ../Data/example_output/ 
