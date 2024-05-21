#!/bin/bash
#$ -N compute_sfs_accessory
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -cwd
#$ -V
#$ -l h_data=25G
#$ -l h_rt=11:50:00
#$ -l highp
#$ -t 1:72

SGE_TASK_ID=2

# readarray good_spec < ../Data/good_species_list.txt
readarray good_spec < ../HighRecombinationData/good_species_list.txt

good_spec=( null ${good_spec[@]} )

# python compute_sfs.py ${good_spec[$SGE_TASK_ID]} ../Analysis/${good_spec[$SGE_TASK_ID]}/
python compute_sfs.py ${good_spec[$SGE_TASK_ID]} ../HighRecombinationAnalysis/${good_spec[$SGE_TASK_ID]}/ --core
# python compute_sfs.py ${good_spec[$SGE_TASK_ID]} ../Analysis/${good_spec[$SGE_TASK_ID]}/ --accessory
# echo ${good_spec[SGE_TASK_ID]}
