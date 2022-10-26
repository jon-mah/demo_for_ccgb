#!/bin/bash
#$ -N construct_crude_sfs.bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l highp
#$ -l h_data=25G
#$ -l time=00:10:00

python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Fusobacterium_periodonticum_58002 --min_depth 10 ../Data/oral_microbiome_data/Fusobacterium_periodonticum_58002_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Gemella_haemolysans_61762 --min_depth 10 ../Data/oral_microbiome_data/Gemella_haemolysans_61762/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Gemella_haemolysans_61762 --min_depth 10 ../Data/oral_microbiome_data/Gemella_haemolysans_61762_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_haemolyticus_58348 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_haemolyticus_58348_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_haemolyticus_58349 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_haemolyticus_58349_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_haemolyticus_58350 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_haemolyticus_58350_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_haemolyticus_58351 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_haemolyticus_58351_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_haemolyticus_58352 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_haemolyticus_58352_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_haemolyticus_58562 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_haemolyticus_58562_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Haemophilus_parainfluenzae_57123 --min_depth 10 ../Data/oral_microbiome_data/Haemophilus_parainfluenzae_57123_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Neisseria_sicca_58189 --min_depth 10 ../Data/oral_microbiome_data/Neisseria_sicca_58189_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Porphyromonas_sp_57899 --min_depth 10 ../Data/oral_microbiome_data/Porphyromonas_sp_57899_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Rothia_dentocariosa_57938 --min_depth 10 ../Data/oral_microbiome_data/Rothia_dentocariosa_57938_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Rothia_mucilaginosa_62109 --min_depth 10 ../Data/oral_microbiome_data/Rothia_mucilaginosa_62109_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_58288 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_58288_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_58382 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_58382_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_58556 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_58556_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_58558 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_58558_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_59739 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_59739_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_60170 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_60170_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_60171 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_60171_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_60473 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_60473_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_60474 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_60474_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_61106 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_61106_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_61110 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_61110_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_mitis_61875 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_mitis_61875_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Streptococcus_oralis_58560 --min_depth 10 ../Data/oral_microbiome_data/Streptococcus_oralis_58560_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Veillonella_parvula_57794 --min_depth 10 ../Data/oral_microbiome_data/Veillonella_parvula_57794_sfs/
python construct_crude_sfs.py ../Data/oral_microbiome_data/merged_data/snps/ Veillonella_parvula_58184 --min_depth 10 ../Data/oral_microbiome_data/Veillonella_parvula_58184_sfs/
