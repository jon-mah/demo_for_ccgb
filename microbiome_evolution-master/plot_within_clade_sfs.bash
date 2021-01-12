#!/bin/bash
#$ -cwd
#$ -V
#$ -m bea
#$ -l h_data=10G
#$ -l h_rt=00:30:00

python plot_within_clade_sfs.py
