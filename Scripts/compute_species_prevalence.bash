#!/bin/bash
#$ -N MIDAS_snps.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=15G
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l time=0:10:00

i=0
while read line;
do
   i=$((i+1))
   file=$line
   OUTDIR=../Data/microbiome_data/snps/${file}
   echo -n $file >> ../Summary/species_prevalence.txt
   echo -n ", " >> ../Summary/species_prevalence.txt
   bzcat ${OUTDIR}/snps_ref_freq.txt.bz2 | head -n1 | uniq -c | wc -w >> ../Summary/species_prevalence.txt
done < ../Data/microbiome_data/snps/species_snps_list.txt

OUTDIR=../Data/micOUTDIR=../Data/microbiome_data/snps/${file}robiome_data/snps/${file}

# echo $file >> ../Summary/species_prevalence.txt
# bzcat ${OUTDIR}/snps_ref_freq.txt.bz2 | head -n1 | uniq -c | wc -w >> ../Summary/species_prevalence.txt
