#!/bin/bash
#$ -cwd
#$ -V
#$ -l h_data=20G
#$ -l h_rt=4:00:00
#$ -t 1-22
#$ -l highp
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -N high_recombination_sfs

# species="Alistipes_shahii_62199"

# SGE_TASK_ID=1

i=0
while read line;
do
  i=$((i+1))
  if [ $i -eq $SGE_TASK_ID ]
    then
      species=$line
  fi
done < ../HighRecombinationData/good_species_list.txt

# percentile=0.50

percentile = 0.0
while percentile < 1.01
do
  python return_high_recombination.py $species $percentile ../HighRecombinationAnalysis/${species}/ --core
  percentile=$((percentile+0.01))

# mkdir ../HighRecombinationAnalysis/${species}/

# python return_high_recombination.py $species $percentile ../HighRecombinationAnalysis/${species}/ --core
