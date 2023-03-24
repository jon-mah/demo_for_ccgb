#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:15:00
#$ -N create_clades

rm ../Data/UHGG/manual_clade_definitions.txt

python create_clades.py Akkermansia_muciniphila_B 0.007 ../Data/UHGG/
python create_clades.py Alistipes_finegoldii 0.006 ../Data/UHGG/
python create_clades.py Alistipes_onderdonkii 0.007 ../Data/UHGG/
python create_clades.py Alistipes_putredinis 0.007 ../Data/UHGG/
python create_clades.py Alistipes_shahii 0.005 ../Data/UHGG/
# python create_clades.py Bacteroides_fragilis 0.007 ../Data/UHGG/
python create_clades.py Bacteroides_ovatus 0.01 ../Data/UHGG/
# python create_clades.py Bacteroides_stercoris 0.007 ../Data/UHGG/
python create_clades.py Bacteroides_thetaiotaomicron 0.008 ../Data/UHGG/
python create_clades.py Bacteroides_uniformis 0.007 ../Data/UHGG/
python create_clades.py Bacteroides_xylanisolvens 0.009 ../Data/UHGG/
python create_clades.py Barnesiella_intestinihominis 0.008 ../Data/UHGG/
python create_clades.py Dialister_invisus 0.006 ../Data/UHGG/
# python create_clades.py Faecalibacterium_prausnitzii_K 0.007 ../Data/UHGG/
python create_clades.py Odoribacter_splanchnicus 0.004 ../Data/UHGG/
python create_clades.py Parabacteroides_distasonis 0.006 ../Data/UHGG/
python create_clades.py Parabacteroides_merdae 0.004 ../Data/UHGG/
# python create_clades.py Prevotella_copri_A 0.007 ../Data/UHGG/
python create_clades.py Ruminococcus_E_bromii_B 0.007 ../Data/UHGG/
