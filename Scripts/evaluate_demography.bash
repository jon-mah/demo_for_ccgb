#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00

# UHGG Isolates
# python evaluate_demography.py ../Data/UHGG/UHGG_Akkermansia_muciniphila_B/downsampled_sfs.txt two_epoch 7.62438e-05 0.00059825 ../Data/UHGG/UHGG_Akkermansia_muciniphila_B/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_finegoldii/downsampled_sfs.txt two_epoch 6.87297e-05 0.000692151 ../Data/UHGG/UHGG_Alistipes_finegoldii/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_onderdonkii/downsampled_sfs.txt two_epoch 13.8009 0.0867931 ../Data/UHGG/UHGG_Alistipes_onderdonkii/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_putredinis/downsampled_sfs.txt two_epoch 2.04494 0.896315 ../Data/UHGG/UHGG_Alistipes_putredinis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_shahii/downsampled_sfs.txt two_epoch 6.34036 0.305876 ../Data/UHGG/UHGG_Alistipes_shahii/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt two_epoch 2.31451 0.810425 ../Data/UHGG/UHGG_Bacteroides_fragilis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_ovatus/downsampled_sfs.txt two_epoch 0.67543 0.309707 ../Data/UHGG/UHGG_Bacteroides_ovatus/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_stercoris/downsampled_sfs.txt two_epoch 2.88222 0.528089 ../Data/UHGG/UHGG_Bacteroides_stercoris/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/downsampled_sfs.txt two_epoch 2.86237 0.125612 ../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_xylanisolvens/downsampled_sfs.txt two_epoch 22.1446 2.4363 ../Data/UHGG/UHGG_Bacteroides_xylanisolvens/
# python evaluate_demography.py ../Data/UHGG/UHGG_Barnesiella_intestinihominis/downsampled_sfs.txt two_epoch 1.29099 0.470359 ../Data/UHGG/UHGG_Barnesiella_intestinihominis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Dialister_invisus/downsampled_sfs.txt two_epoch 0.000520986 0.00496611 ../Data/UHGG/UHGG_Dialister_invisus/
# python evaluate_demography.py ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/downsampled_sfs.txt two_epoch 3.1158 1.10706 ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/
# python evaluate_demography.py ../Data/UHGG/UHGG_Odoribacter_splanchnicus/downsampled_sfs.txt two_epoch 0.00034436 0.00282058 ../Data/UHGG/UHGG_Odoribacter_splanchnicus/
# python evaluate_demography.py ../Data/UHGG/UHGG_Parabacteroides_distasonis/downsampled_sfs.txt two_epoch 0.0 0.0 ../Data/UHGG/UHGG_Parabacteroides_distasonis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Parabacteroides_merdae/downsampled_sfs.txt two_epoch 5.38016 0.0586718 ../Data/UHGG/UHGG_Parabacteroides_merdae/
# python evaluate_demography.py ../Data/UHGG/UHGG_Prevotella_copri/downsampled_sfs.txt two_epoch 11.2043 0.0439066 ../Data/UHGG/UHGG_Prevotella_copri/
# python evaluate_demography.py ../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/downsampled_sfs.txt two_epoch 3.50459 0.0639457 ../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/

# HMP-QP Complete
python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.330587 0.289674 ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/complete
python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.590248 0.125047 ../Analysis/Alistipes_finegoldii_56071_downsampled_14/complete
python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.00111624 0.00564187 ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/complete
python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.000971892 0.00682997 ../Analysis/Alistipes_putredinis_61533_downsampled_14/complete
python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_syn_downsampled_sfs.txttxt two_epoch 34.9209 172.577 ../Analysis/Alistipes_shahii_62199_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.464414 0.066745 ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 296.16 709.811 ../Analysis/Bacteroides_caccae_53434_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 1.46129 0.475054 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 1.88251 0.195185 ../Analysis/Bacteroides_fragilis_54507_downsampled_14/complete
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.000299311 8.73668e-06 ../Analysis/Bacteroides_ovatus_58035_downsampled_14/complete
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.770457 0.0497167 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 394.013 837.865 ../Analysis/Bacteroides_uniformis_57318_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.895188 0.506591 ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/complete
python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.00115518 2.77695e-05 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/complete
python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.505931 0.0392079 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/complete
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_14/complete
python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.00272822 0.0251103 ../Analysis/Dialister_invisus_61905_downsampled_14/complete
python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.00215738 0.015655 ../Analysis/Eubacterium_eligens_61678_downsampled_14/complete
python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 1.23419 0.238825 ../Analysis/Eubacterium_rectale_56927_downsampled_14/complete
python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 184.624 578.327 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/complete
python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.607248 0.0315866 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/complete
python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.000286308 0.00292652 ../Analysis/Oscillibacter_sp_60799_downsampled_14/complete
python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 374.228 707.204 ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/complete
python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 0.778422 0.0454067 ../Analysis/Parabacteroides_merdae_56972_downsampled_14/complete
python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 3.13313 0.0997377 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/complete
python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 27.1938 0.0600029 ../Analysis/Prevotella_copri_61740_downsampled_14/complete
python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 2.15694 0.448367 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/complete
python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch 1.92731 0.279787 ../Analysis/Ruminococcus_bromii_62047_downsampled_14/complete

# Downsampled to 10
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.40622 0.166818 ../Analysis/Akkermansia_muciniphila_55290_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.0024135 0.0160841 ../Analysis/Alistipes_finegoldii_56071_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.0881793 0.227832 ../Analysis/Alistipes_onderdonkii_55464_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_10/downsampled_syn_sfs.txt two_epoch 8.43938 3.10627 ../Analysis/Alistipes_putredinis_61533_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_10/downsampled_syn_sfs.txt two_epoch 13.9005 0.035724 ../Analysis/Alistipes_shahii_62199_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.656385 0.0924681 ../Analysis/Bacteroidales_bacterium_58650_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_10/downsampled_syn_sfs.txt two_epoch 1.91278 0.0536306 ../Analysis/Bacteroides_caccae_53434_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/downsampled_syn_sfs.txt two_epoch 2.29372 0.828225 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_10/downsampled_syn_sfs.txt two_epoch 3.22129 0.248444 ../Analysis/Bacteroides_fragilis_54507_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_10/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_10/downsampled_syn_sfs.txt two_epoch 2.25642 0.0603274 ../Analysis/Bacteroides_ovatus_58035_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_10/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.0163749 0.100595 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_10/downsampled_syn_sfs.txt two_epoch 32.233 49.2075 ../Analysis/Bacteroides_uniformis_57318_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_10/downsampled_syn_sfs.txt two_epoch 4.9289 0.0037879 ../Analysis/Bacteroides_vulgatus_57955_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.0132507 0.0850143 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.149372 0.00621968 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_10/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_10/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_10/downsampled_syn_sfs.txt two_epoch 1.57348 0.0827088 ../Analysis/Dialister_invisus_61905_downsampled_10/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_10/downsampled_syn_sfs.txt two_epoch 44.8382 92.7077 ../Analysis/Eubacterium_eligens_61678_downsampled_10/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_10/downsampled_syn_sfs.txt two_epoch 1.64542 0.769789 ../Analysis/Eubacterium_rectale_56927_downsampled_10/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/downsampled_syn_sfs.txt two_epoch 1.09867 1.299 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/downsampled_syn_sfs.txt two_epoch 9.55467 31.6094 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_10/downsampled_syn_sfs.txt two_epoch 5.02523 0.0759358 ../Analysis/Oscillibacter_sp_60799_downsampled_10/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.776827 0.0916526 ../Analysis/Parabacteroides_distasonis_56985_downsampled_10/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_10/downsampled_syn_sfs.txt two_epoch 0.742039 0.379442 ../Analysis/Parabacteroides_merdae_56972_downsampled_10/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/downsampled_syn_sfs.txt two_epoch 2.58953 0.328679 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_10/downsampled_syn_sfs.txt two_epoch 3.47521 0.358743 ../Analysis/Prevotella_copri_61740_downsampled_10/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/downsampled_syn_sfs.txt two_epoch 512.46 417.649 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_10/downsampled_syn_sfs.txt two_epoch 2.99009 1.50668 ../Analysis/Ruminococcus_bromii_62047_downsampled_10/

# Downsampled to 12
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.406192 0.221325 ../Analysis/Akkermansia_muciniphila_55290_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.00141166 0.0102196 ../Analysis/Alistipes_finegoldii_56071_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.000663046 0.00503453 ../Analysis/Alistipes_onderdonkii_55464_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_12/downsampled_syn_sfs.txt two_epoch 9.19344 3.44869 ../Analysis/Alistipes_putredinis_61533_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_12/downsampled_syn_sfs.txt two_epoch 17.2738 0.0394458 ../Analysis/Alistipes_shahii_62199_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.692618 0.135731 ../Analysis/Bacteroidales_bacterium_58650_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_12/downsampled_syn_sfs.txt two_epoch 1.40648 0.120183 ../Analysis/Bacteroides_caccae_53434_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/downsampled_syn_sfs.txt two_epoch 2.75049 1.50239 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_12/downsampled_syn_sfs.txt two_epoch 2.40281 0.406655 ../Analysis/Bacteroides_fragilis_54507_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_12/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_12/downsampled_syn_sfs.txt two_epoch 7.20843 0.0339838 ../Analysis/Bacteroides_ovatus_58035_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_12/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.0143413 0.0909163 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_12/downsampled_syn_sfs.txt two_epoch 7.59117 10.6886 ../Analysis/Bacteroides_uniformis_57318_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_12/downsampled_syn_sfs.txt two_epoch 3.01055 0.00824215 ../Analysis/Bacteroides_vulgatus_57955_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.0369925 0.202084 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.437146 0.0315171 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_12/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_12/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_12/downsampled_syn_sfs.txt two_epoch 2.42975 0.0476867 ../Analysis/Dialister_invisus_61905_downsampled_12/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_12/downsampled_syn_sfs.txt two_epoch 106.953 222.988 ../Analysis/Eubacterium_eligens_61678_downsampled_12/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_12/downsampled_syn_sfs.txt two_epoch 1.58777 0.580629 ../Analysis/Eubacterium_rectale_56927_downsampled_12/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/downsampled_syn_sfs.txt two_epoch 9.21845 30.6123 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/downsampled_syn_sfs.txt two_epoch 1.5589 3.85637 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_12/downsampled_syn_sfs.txt two_epoch 8.01039 0.0744874 ../Analysis/Oscillibacter_sp_60799_downsampled_12/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.791112 0.105926 ../Analysis/Parabacteroides_distasonis_56985_downsampled_12/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_12/downsampled_syn_sfs.txt two_epoch 0.755594 0.341855 ../Analysis/Parabacteroides_merdae_56972_downsampled_12/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/downsampled_syn_sfs.txt two_epoch 2.46391 0.322072 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_12/downsampled_syn_sfs.txt two_epoch 4.11266 0.305928 ../Analysis/Prevotella_copri_61740_downsampled_12/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/downsampled_syn_sfs.txt two_epoch 310.657 252.905 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_12/downsampled_syn_sfs.txt two_epoch 3.01357 1.50539 ../Analysis/Ruminococcus_bromii_62047_downsampled_12/

# Downsampled to 14
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/downsampled_syn_sfs.txt two_epoch 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_14/downsampled_syn_sfs.txt two_epoch 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/downsampled_syn_sfs.txt two_epoch 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/downsampled_syn_sfs.txt two_epoch 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/downsampled_syn_sfs.txt two_epoch 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_14/downsampled_syn_sfs.txt two_epoch 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/downsampled_syn_sfs.txt two_epoch 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/downsampled_syn_sfs.txt two_epoch 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_14/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_14/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_14/downsampled_syn_sfs.txt two_epoch 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_14/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/downsampled_syn_sfs.txt two_epoch 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_14/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/downsampled_syn_sfs.txt two_epoch 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_14/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/downsampled_syn_sfs.txt two_epoch 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/downsampled_syn_sfs.txt two_epoch 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/downsampled_syn_sfs.txt two_epoch 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_14/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/downsampled_syn_sfs.txt two_epoch 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_14/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/downsampled_syn_sfs.txt two_epoch 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_14/downsampled_syn_sfs.txt two_epoch 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_14/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/downsampled_syn_sfs.txt two_epoch 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/downsampled_syn_sfs.txt two_epoch 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_14/

# Downsampled to 16
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.385545 0.263801 ../Analysis/Akkermansia_muciniphila_55290_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.0216463 0.098936 ../Analysis/Alistipes_finegoldii_56071_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.00874967 0.0439118 ../Analysis/Alistipes_onderdonkii_55464_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_16/downsampled_syn_sfs.txt two_epoch 10.6528 4.16344 ../Analysis/Alistipes_putredinis_61533_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_16/downsampled_syn_sfs.txt two_epoch 23.2719 0.0338066 ../Analysis/Alistipes_shahii_62199_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.72274 0.221349 ../Analysis/Bacteroidales_bacterium_58650_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_16/downsampled_syn_sfs.txt two_epoch 1.60429 0.0791623 ../Analysis/Bacteroides_caccae_53434_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/downsampled_syn_sfs.txt two_epoch 2.37427 0.975554 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_16/downsampled_syn_sfs.txt two_epoch 3.21259 0.237611 ../Analysis/Bacteroides_fragilis_54507_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_16/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_16/downsampled_syn_sfs.txt two_epoch 8.04801 0.0348558 ../Analysis/Bacteroides_ovatus_58035_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_16/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.00659293 0.0473145 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_16/downsampled_syn_sfs.txt two_epoch 1.77798 1.27066 ../Analysis/Bacteroides_uniformis_57318_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_16/downsampled_syn_sfs.txt two_epoch 6.1956 0.0100282 ../Analysis/Bacteroides_vulgatus_57955_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.0104182 0.0720721 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.653196 0.118365 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_16/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_16/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_16/downsampled_syn_sfs.txt two_epoch 3.02789 0.0450853 ../Analysis/Dialister_invisus_61905_downsampled_16/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_16/downsampled_syn_sfs.txt two_epoch 32.0696 65.638 ../Analysis/Eubacterium_eligens_61678_downsampled_16/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_16/downsampled_syn_sfs.txt two_epoch 1.65568 0.832631 ../Analysis/Eubacterium_rectale_56927_downsampled_16/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/downsampled_syn_sfs.txt two_epoch 1.06737 0.337445 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/downsampled_syn_sfs.txt two_epoch 11.5442 40.7758 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_16/downsampled_syn_sfs.txt two_epoch 4.02196 0.0964394 ../Analysis/Oscillibacter_sp_60799_downsampled_16/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.813611 0.161783 ../Analysis/Parabacteroides_distasonis_56985_downsampled_16/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_16/downsampled_syn_sfs.txt two_epoch 0.687759 0.605537 ../Analysis/Parabacteroides_merdae_56972_downsampled_16/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/downsampled_syn_sfs.txt two_epoch 2.51285 0.540036 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_16/downsampled_syn_sfs.txt two_epoch 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_16/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/downsampled_syn_sfs.txt two_epoch 286.055 233.838 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_16/downsampled_syn_sfs.txt two_epoch 2.85747 1.30462 ../Analysis/Ruminococcus_bromii_62047_downsampled_16/

# Downsampled to 18
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.391512 0.240012 ../Analysis/Akkermansia_muciniphila_55290_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.0545651 0.199532 ../Analysis/Alistipes_finegoldii_56071_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.000125171 0.00116278 ../Analysis/Alistipes_onderdonkii_55464_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_18/downsampled_syn_sfs.txt two_epoch 10.7771 4.21105 ../Analysis/Alistipes_putredinis_61533_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_18/downsampled_syn_sfs.txt two_epoch 3.48215 0.0630991 ../Analysis/Alistipes_shahii_62199_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.747965 0.26547 ../Analysis/Bacteroidales_bacterium_58650_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_18/downsampled_syn_sfs.txt two_epoch 3.27649 0.0316115 ../Analysis/Bacteroides_caccae_53434_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/downsampled_syn_sfs.txt two_epoch 3.18192 1.99256 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_18/downsampled_syn_sfs.txt two_epoch 3.0888 0.26575 ../Analysis/Bacteroides_fragilis_54507_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_18/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_18/downsampled_syn_sfs.txt two_epoch 2.12642 0.108158 ../Analysis/Bacteroides_ovatus_58035_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_18/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.13555 0.546058 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_18/downsampled_syn_sfs.txt two_epoch 1.76202 1.2601 ../Analysis/Bacteroides_uniformis_57318_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_18/downsampled_syn_sfs.txt two_epoch 1.78105 0.0311021 ../Analysis/Bacteroides_vulgatus_57955_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.125021 0.540209 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.643165 0.092557 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_18/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_18/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_18/downsampled_syn_sfs.txt two_epoch 2.21215 0.0579241 ../Analysis/Dialister_invisus_61905_downsampled_18/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_18/downsampled_syn_sfs.txt two_epoch 83.0965 170.788 ../Analysis/Eubacterium_eligens_61678_downsampled_18/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_18/downsampled_syn_sfs.txt two_epoch 3.53508 4.03943 ../Analysis/Eubacterium_rectale_56927_downsampled_18/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/downsampled_syn_sfs.txt two_epoch 1.09659 0.243501 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/downsampled_syn_sfs.txt two_epoch 1.31261 3.03154 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_18/downsampled_syn_sfs.txt two_epoch 2.93863 0.133149 ../Analysis/Oscillibacter_sp_60799_downsampled_18/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.836116 0.228785 ../Analysis/Parabacteroides_distasonis_56985_downsampled_18/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_18/downsampled_syn_sfs.txt two_epoch 0.0352227 0.176049 ../Analysis/Parabacteroides_merdae_56972_downsampled_18/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/downsampled_syn_sfs.txt two_epoch 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_18/downsampled_syn_sfs.txt two_epoch 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_18/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/downsampled_syn_sfs.txt two_epoch 258.088 210.431 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_18/downsampled_syn_sfs.txt two_epoch 3.00012 1.49253 ../Analysis/Ruminococcus_bromii_62047_downsampled_18/

# Downsampled to 20
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_20/downsampled_syn_sfs.txt two_epoch 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_20/downsampled_syn_sfs.txt two_epoch 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_20/downsampled_syn_sfs.txt two_epoch 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/downsampled_syn_sfs.txt two_epoch 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_20/downsampled_syn_sfs.txt two_epoch 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_20/
# # python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_20/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_20/downsampled_syn_sfs.txt two_epoch 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_20/
# # python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_20/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_20/downsampled_syn_sfs.txt two_epoch 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_20/downsampled_syn_sfs.txt two_epoch 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/
# # python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_20/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_20/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_20/downsampled_syn_sfs.txt two_epoch 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_20/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_20/downsampled_syn_sfs.txt two_epoch 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_20/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_20/downsampled_syn_sfs.txt two_epoch 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_20/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/downsampled_syn_sfs.txt two_epoch 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/downsampled_syn_sfs.txt two_epoch 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_20/downsampled_syn_sfs.txt two_epoch 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_20/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_20/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_20/downsampled_syn_sfs.txt two_epoch 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_20/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/downsampled_syn_sfs.txt two_epoch 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_20/downsampled_syn_sfs.txt two_epoch 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_20/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/downsampled_syn_sfs.txt two_epoch 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_20/downsampled_syn_sfs.txt two_epoch 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_20/

# Downsampled to 30
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_30/downsampled_syn_sfs.txt two_epoch 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_30/downsampled_syn_sfs.txt two_epoch 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_30/downsampled_syn_sfs.txt two_epoch 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/downsampled_syn_sfs.txt two_epoch 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_30/downsampled_syn_sfs.txt two_epoch 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_30/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_massiliensis_44749_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_30/downsampled_syn_sfs.txt two_epoch 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_30/downsampled_syn_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_30/downsampled_syn_sfs.txt two_epoch 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_30/downsampled_syn_sfs.txt two_epoch 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_30/downsampled_syn_sfs.txt two_epoch ../Analysis/Coprococcus_sp_62244_downsampled_30/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_30/downsampled_syn_sfs.txt two_epoch 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_30/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_30/downsampled_syn_sfs.txt two_epoch 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_30/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_30/downsampled_syn_sfs.txt two_epoch 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_30/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/downsampled_syn_sfs.txt two_epoch 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/downsampled_syn_sfs.txt two_epoch 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_30/downsampled_syn_sfs.txt two_epoch 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_30/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_30/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_30/downsampled_syn_sfs.txt two_epoch 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_30/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/downsampled_syn_sfs.txt two_epoch 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_30/downsampled_syn_sfs.txt two_epoch 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_30/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/downsampled_syn_sfs.txt two_epoch 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_30/downsampled_syn_sfs.txt two_epoch 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_30/
