#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:10:00

# This script infers the demography of a given example synonymous sfs.
python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_sfs.txt two_epoch 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/
python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_sfs.txt two_epoch 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_14/
python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_sfs.txt two_epoch 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/
python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_sfs.txt two_epoch 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_14/
python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_sfs.txt two_epoch 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_sfs.txt two_epoch 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_sfs.txt two_epoch 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_sfs.txt two_epoch 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_sfs.txt two_epoch 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/empirical_sfs.txt two_epoch 0.540065 0.050769 ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_sfs.txt two_epoch 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_sfs.txt two_epoch 0.0427133 0.22371 ../Analysis/Bacteroides_stercoris_56735_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_sfs.txt two_epoch 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_sfs.txt two_epoch 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_sfs.txt two_epoch 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/
python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_sfs.txt two_epoch 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/
python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_sfs.txt two_epoch 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_14/empirical_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_14/
python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_14/empirical_sfs.txt two_epoch 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_14/
python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt two_epoch 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_14/
python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_sfs.txt two_epoch 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_14/
python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_sfs.txt two_epoch 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/
python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_sfs.txt two_epoch 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/
python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_sfs.txt two_epoch 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_14/
python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_sfs.txt two_epoch 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/
python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_sfs.txt two_epoch 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_14/
python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_sfs.txt two_epoch 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/
python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_14/empirical_sfs.txt two_epoch 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_14/
python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_sfs.txt two_epoch 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/
python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_sfs.txt two_epoch 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_14/
