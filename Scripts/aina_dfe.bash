#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=25G
#$ -l h_rt=00:30:00
#$ -N aina_dfe_bash

python fit_demographic_model_and_DFE.py ../Data/aina_data/aina_unfolded_syn.txt ../Data/aina_data/aina_unfolded_nonsyn.txt ../Analysis/aina_test/
