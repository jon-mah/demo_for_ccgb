#!/bin/bash
#$ -N merge_snps.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -l h_data=15G
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l time=48:00:00
#$ -l highp

# module load python/2.7
export PYTHONPATH=$PYTHONPATH:/u/project/ngarud/Garud_lab/MIDAS
export PATH=$PATH:/u/project/ngarud/Garud_lab/MIDAS/scripts
export MIDAS_DB=/u/project/ngarud/Garud_lab/midas_db_v1.2

OUTDIR=../Data/oral_microbiome_data/temp_merged_data
# module load singularity
# singularity exec $H2_CONTAINER_LOC/MIDAS-mod.sif merge_midas.py snps $OUTDIR/snps -i ../Data/oral_microbiome_data/midas_output -t dir --species_id Actinomyces_viscosus_57672,Fusobacterium_periodonticum_58002,Gemella_haemolysans_61762,Granulicatella_elegans_61945,Haemophilus_parahaemolyticus_60505,Haemophilus_parainfluenzae_57123,Haemophilus_parainfluenzae_62356,Haemophilus_parainfluenzae_62468,Haemophilus_paraphrohaemolyticus_58563,Haemophilus_sputorum_53575,Neisseria_sp_61995,Porphyromonas_sp_57899,Prevotella_nanceiensis_44721,Prevotella_sp_61856,Rothia_mucilaginosa_58144,Streptococcus_parasanguinis_58487,Streptococcus_sp_57750,Streptococcus_sp_61820,Streptococcus_vestibularis_56030,Veillonella_sp_62404,Veillonella_sp_62611 --sample_depth 5 --site_depth 3 --min_samples 1 --site_prev 0.0


module load python/2.7.18
module load midas
. /u/local/apps/midas/1.3.2/python-2.7.18-MIDAS-VE/bin/activate
# merge_midas.py snps -h

merge_midas.py snps $OUTDIR/snps -i ../Data/oral_microbiome_data/midas_output -t dir --species_id Actinomyces_viscosus_57672,Fusobacterium_periodonticum_58002,Gemella_haemolysans_61762,Granulicatella_elegans_61945,Haemophilus_parahaemolyticus_60505,Haemophilus_parainfluenzae_57123,Haemophilus_parainfluenzae_62356,Haemophilus_parainfluenzae_62468,Haemophilus_paraphrohaemolyticus_58563,Haemophilus_sputorum_53575,Neisseria_sp_61995,Porphyromonas_sp_57899,Prevotella_nanceiensis_44721,Prevotella_sp_61856,Rothia_mucilaginosa_58144,Streptococcus_parasanguinis_58487,Streptococcus_sp_57750,Streptococcus_sp_61820,Streptococcus_vestibularis_56030,Veillonella_sp_62404,Veillonella_sp_62611 --sample_depth 5 --site_depth 3 --min_samples 1 --site_prev 0.0
