#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00

donni infer --input_fs ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_syn_downsampled_sfs.txt --model three_epoch
