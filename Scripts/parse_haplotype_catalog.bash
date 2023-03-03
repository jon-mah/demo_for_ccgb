#!/bin/bash
#$ -N MIDAS_species.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:30:00

python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_1_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/1_
python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_2_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/2_
python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_3_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/3_
python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_4_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/4_
python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_5_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/5_
