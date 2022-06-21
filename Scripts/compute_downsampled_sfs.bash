#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.
# species=Akkermansia_muciniphila_55290
# species=Alistipes_finegoldii_56071
# species=Alistipes_onderdonkii_55464
# species=Alistipes_putredinis_61533
# species=Alistipes_shahii_62199
# species=Bacteroidales_bacterium_58650
# species=Bacteroides_caccae_53434
# species=Bacteroides_cellulosilyticus_58046
# species=Bacteroides_fragilis_54507
# species=Bacteroides_massiliensis_44749
# species=Bacteroides_ovatus_58035
# species=Bacteroides_stercoris_56735
# species=Bacteroides_thetaiotaomicron_56941
species=Bacteroides_uniformis_57318
# species=Bacteroides_vulgatus_57955
# species=Bacteroides_xylanisolvens_57185
# species=Barnesiella_intestinihominis_62208
# species=Coprococcus_sp_62244
# species=Dialister_invisus_61905
# species=Eubacterium_eligens_61678
# species=Eubacterium_rectale_56927
# species=Odoribacter_splanchnicus_62174
# species=Oscillibacter_sp_60799
# species=Parabacteroides_distasonis_56985
# species=Parabacteroides_merdae_56972
# species=Phascolarctobacterium_sp_59817
# species=Prevotella_copri_61740
# species=Ruminococcus_bicirculans_59300
# species=Ruminococcus_bromii_62047

python compute_downsampled_sfs.py ${species} ../Data/
