#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00

# donni infer --input_fs ../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_syn_downsampled_sfs.txt --model two_epoch
donni infer --input_fs ../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_syn_downsampled_sfs.txt --model two_epoch
