#!/bin/bash
#$ -cwd
#$ -V
#$ -m bea

# This script infers the demography of a given example synonymous sfs.

python fit_demographic_model.py ../Data/example.sfs ../Data/example_output/ 
