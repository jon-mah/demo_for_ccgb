#!/bin/bash
#$ -cwd
#$ -V
#$ -N b_thetaiotamicron
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:15:00

# This script infers the demography of a given example synonymous sfs.

python plot_likelihood.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs  temp ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled/ --mask_singletons
