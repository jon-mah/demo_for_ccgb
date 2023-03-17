#!/bin/bash
#$ -N MIDAS_species.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:30:00

# python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_1_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/1_
# python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_2_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/2_
# python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_3_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/3_
# python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_4_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/4_
# python parse_haplotype_catalog.py ../Data/UHGG/UHGG_Bacteroides_fragilis/GUT_GENOME096063_5_haplotypes.csv ../Data/UHGG/UHGG_Bacteroides_fragilis/5_

# /u/project/ngarud/Garud_lab/UHGG
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Bacteroides_fragilis/GUT_GENOME096063_1_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Bacteroides_fragilis ../Data/UHGG/UHGG_Bacteroides_fragilis/1
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Bacteroides_fragilis/GUT_GENOME096063_2_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Bacteroides_fragilis ../Data/UHGG/UHGG_Bacteroides_fragilis/2
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Bacteroides_fragilis/GUT_GENOME096063_3_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Bacteroides_fragilis ../Data/UHGG/UHGG_Bacteroides_fragilis/3
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Bacteroides_fragilis/GUT_GENOME096063_4_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Bacteroides_fragilis ../Data/UHGG/UHGG_Bacteroides_fragilis/4
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Bacteroides_fragilis/GUT_GENOME096063_5_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Bacteroides_fragilis ../Data/UHGG/UHGG_Bacteroides_fragilis/5

python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_1_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/1
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_2_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/2
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_3_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/3
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_4_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/4
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_5_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/5
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_6_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/6
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_7_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/7
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_8_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/8
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_9_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/9
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_10_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/10
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_15_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/15
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_16_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/16
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_17_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/17
python parse_haplotype_catalog.py /u/project/ngarud/Garud_lab/UHGG/haplotypes/Alistipes_finegoldii/GUT_GENOME111543_18_haplotypes.csv /u/project/ngarud/Garud_lab/UHGG/distances/dendrograms/manual_clade_definitions.txt Alistipes_finegoldii ../Data/UHGG/UHGG_Alistipes_finegoldii/18
