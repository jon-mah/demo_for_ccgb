#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=1G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

# python compute_SNP_matrix.py ../Data/microbiome_data/snps/Phascolarctobacterium_succinatutens_61948/annotated_snps.txt.bz2 ../Data/example_output/
# python compute_SNP_matrix.py ../Data/microbiome_data/snps/Bacteroides_vulgatus_57955/annotated_snps.txt.bz2 ../Analysis/Bacteroides_vulgatus_57955/
# python compute_SNP_matrix.py ../Data/microbiome_data/snps/Bacteroides_ovatus_58035/annotated_snps.txt.bz2 ../Analysis/Bacteroides_ovatus_58035/
# python compute_SNP_matrix.py ../Data/microbiome_data/snps/Alistipes_putredinis_61533/annotated_snps.txt.bz2 ../Analysis/Alistipes_putredinis_61533/
# python compute_SNP_matrix.py ../Data/microbiome_data/snps/Bacteroides_uniformis_57318/annotated_snps.txt.bz2 ../Analysis/Bacteroides_uniformis_57318/
# python compute_SNP_matrix.py ../Data/microbiome_data/snps/Eubacterium_rectale_56927/annotated_snps.txt.bz2 ../Analysis/Eubacterium_rectale_56927/
