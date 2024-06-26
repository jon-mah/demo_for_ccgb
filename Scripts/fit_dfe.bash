#!/bin/bash
#$ -cwd
#$ -V
#$ -m a
#$ -l h_data=25G
#$ -l h_rt=12:00:00
#$ -l highp
#$ -t 1-23
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N fit_dfe_FD_accessory

# SGE_TASK_ID=14
#### Python 3
# i=0
# while read line;
# do
#   i=$((i+1))
#   if [ $i -eq $SGE_TASK_ID ]
#    then
#      species=$line
#   fi
# done < ../Data/good_species_list.txt

# i=0
# while read line;
# do
#   i=$((i+1))
#   if [ $i -eq $SGE_TASK_ID ]
#     then
#       species=$line
#   fi
# done < ../HighRecombinationData/good_species_list.txt

i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
   then
     species=$line
  fi
done < ../SupplementaryAnalysis/temp_supplementary_species_list.txt

sample_size=14

# python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/core_empicial_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/core_empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/core_one_epoch_demography.txt one_epoch ../Analysis/${species}_downsampled_${sample_size}/core
# python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/core_empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/core_empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/core_two_epoch_demography.txt two_epoch ../Analysis/${species}_downsampled_${sample_size}/core
# python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/core_empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/core_empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/core_three_epoch_demography.txt three_epoch ../Analysis/${species}_downsampled_${sample_size}/core
# python fit_dfe.py ../Analysis/${species}_downsampled_${sample_size}/accessory_empirical_syn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/accessory_empirical_nonsyn_downsampled_sfs.txt ../Analysis/${species}_downsampled_${sample_size}/accessory_two_epoch_demography.txt two_epoch ../Analysis/${species}_downsampled_${sample_size}/accessory
# python fit_dfe.py ../SupplementaryAnalysis/${species}/core_empirical_syn_downsampled_sfs.txt ../SupplementaryAnalysis/${species}/core_empirical_nonsyn_downsampled_sfs.txt ../SupplementaryAnalysis/${species}/two_epoch_demography.txt two_epoch ../SupplementaryAnalysis/${species}/core
# python fit_dfe.py ../HighRecombinationAnalysis/${species}/core_0.5_empirical_syn_14_downsampled_sfs.txt ../HighRecombinationAnalysis/${species}/core_0.5_empirical_nonsyn_14_downsampled_sfs.txt ../HighRecombinationAnalysis/${species}/core_0.5_two_epoch_demography.txt two_epoch ../HighRecombinationAnalysis/${species}/core_0.5
python fit_dfe.py ../SupplementaryAnalysis/${species}/accessory_empirical_syn_downsampled_sfs.txt ../SupplementaryAnalysis/${species}/accessory_empirical_nonsyn_downsampled_sfs.txt ../SupplementaryAnalysis/${species}/accessory_two_epoch_demography.txt two_epoch ../SupplementaryAnalysis/${species}/accessory
