#!/bin/bash
#$ -N concat_isolate_sfs
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:20:00

python concat_isolate_sfs.py ../Data/UHGG/UHGG_Akkermansia_muciniphila_B/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Alistipes_finegoldii/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Alistipes_onderdonkii/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Alistipes_putredinis/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Alistipes_shahii/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Bacteroides_ovatus/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Bacteroides_stercoris/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Bacteroides_xylanisolvens/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Barnesiella_intestinihominis/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Dialister_invisus/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Odoribacter_splanchnicus/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Parabacteroides_distasonis/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Parabacteroides_merdae/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Prevotella_copri/
python concat_isolate_sfs.py ../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/
