#!/bin/bash
#$ -cwd
#$ -V
#$ -m bea
#$ -l h_data=10G
#$ -l h_rt=01:00:00

species=Bacteroides_uniformis_57318
python plot_pi_distribution.py ${species}
