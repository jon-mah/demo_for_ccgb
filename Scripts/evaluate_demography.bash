#!/bin/bash
#$ -cwd
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:30:00

# UHGG Isolates
# python evaluate_demography.py ../Data/UHGG/UHGG_Akkermansia_muciniphila_B/downsampled_sfs.txt two_epoch --params_list 7.62438e-05 0.00059825 ../Data/UHGG/UHGG_Akkermansia_muciniphila_B/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_finegoldii/downsampled_sfs.txt two_epoch --params_list 6.87297e-05 0.000692151 ../Data/UHGG/UHGG_Alistipes_finegoldii/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_onderdonkii/downsampled_sfs.txt two_epoch --params_list 13.8009 0.0867931 ../Data/UHGG/UHGG_Alistipes_onderdonkii/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_putredinis/downsampled_sfs.txt two_epoch --params_list 2.04494 0.896315 ../Data/UHGG/UHGG_Alistipes_putredinis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Alistipes_shahii/downsampled_sfs.txt two_epoch --params_list 6.34036 0.305876 ../Data/UHGG/UHGG_Alistipes_shahii/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt two_epoch --params_list 2.31451 0.810425 ../Data/UHGG/UHGG_Bacteroides_fragilis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_ovatus/downsampled_sfs.txt two_epoch --params_list 0.67543 0.309707 ../Data/UHGG/UHGG_Bacteroides_ovatus/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_stercoris/downsampled_sfs.txt two_epoch --params_list 2.88222 0.528089 ../Data/UHGG/UHGG_Bacteroides_stercoris/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/downsampled_sfs.txt two_epoch --params_list 2.86237 0.125612 ../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/
# python evaluate_demography.py ../Data/UHGG/UHGG_Bacteroides_xylanisolvens/downsampled_sfs.txt two_epoch --params_list 22.1446 2.4363 ../Data/UHGG/UHGG_Bacteroides_xylanisolvens/
# python evaluate_demography.py ../Data/UHGG/UHGG_Barnesiella_intestinihominis/downsampled_sfs.txt two_epoch --params_list 1.29099 0.470359 ../Data/UHGG/UHGG_Barnesiella_intestinihominis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Dialister_invisus/downsampled_sfs.txt two_epoch --params_list 0.000520986 0.00496611 ../Data/UHGG/UHGG_Dialister_invisus/
# python evaluate_demography.py ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/downsampled_sfs.txt two_epoch --params_list 3.1158 1.10706 ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/
# python evaluate_demography.py ../Data/UHGG/UHGG_Odoribacter_splanchnicus/downsampled_sfs.txt two_epoch --params_list 0.00034436 0.00282058 ../Data/UHGG/UHGG_Odoribacter_splanchnicus/
# python evaluate_demography.py ../Data/UHGG/UHGG_Parabacteroides_distasonis/downsampled_sfs.txt two_epoch --params_list 0.0 0.0 ../Data/UHGG/UHGG_Parabacteroides_distasonis/
# python evaluate_demography.py ../Data/UHGG/UHGG_Parabacteroides_merdae/downsampled_sfs.txt two_epoch --params_list 5.38016 0.0586718 ../Data/UHGG/UHGG_Parabacteroides_merdae/
# python evaluate_demography.py ../Data/UHGG/UHGG_Prevotella_copri/downsampled_sfs.txt two_epoch --params_list 11.2043 0.0439066 ../Data/UHGG/UHGG_Prevotella_copri/
# python evaluate_demography.py ../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/downsampled_sfs.txt two_epoch --params_list 3.50459 0.0639457 ../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/

# HMP-QP Complete
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/complete --params_list 0.4129750 0.1958707
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Alistipes_finegoldii_56071_downsampled_14/complete --params_list 0.09138146 0.26371637
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/complete --params_list 0.05118349 0.13302860
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/complete --params_list 0.2983 0.3886 0.1733 0.0000001245
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Alistipes_putredinis_61533_downsampled_14/complete --params_list 3.357 0.7725 0.8615 0.001956
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Alistipes_shahii_62199_downsampled_14/complete --params_list 0.5191816 1.6158950
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/complete --params_list 0.5583036 0.1366923
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Bacteroides_caccae_53434_downsampled_14/complete --params_list 1.185 1.064 0.04346 0.0000005608
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/complete --params_list 1.29 1.72 0.003456 1.065
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/complete --params_list 5.307180 6.930773
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_fragilis_54507_downsampled_14/complete --params_list 2.1107446 0.1853915
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/complete
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_ovatus_58035_downsampled_14/complete --params_list 0.00286 0.000143
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_14/complete --params_list 2.831833 4.186851
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/complete --params_list 0.07870509 0.32826127
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Bacteroides_uniformis_57318_downsampled_14/complete --params_list 0.0009072 0.001345 0.000001103 0.000002545
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/complete --params_list 0.3206005 1.0343913
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/complete --params_list 0.3802602 0.0188616
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/complete --params_list 0.50192573 0.04540289
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_14/complete
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Dialister_invisus_61905_downsampled_14/complete --params_list 90.08 0.36
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Eubacterium_eligens_61678_downsampled_14/complete --params_list 0.9608804 0.2656233
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Eubacterium_rectale_56927_downsampled_14/complete --params_list 1.557711 1.439940
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/complete --params_list 0.62309717 0.03828301
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/complete --params_list 0.7516755 0.2090903
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Oscillibacter_sp_60799_downsampled_14/complete --params_list 19.76884934 0.01692686
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/complete --params_list 0.8482857 0.1507089
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Parabacteroides_merdae_56972_downsampled_14/complete --params_list 0.2308666 0.6551347
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/complete --params_list 2.5946428 0.1907914
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Prevotella_copri_61740_downsampled_14/complete --params_list 4.6292052 0.1132925
#  python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/complete --params_list 5.703620 4.171029
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Ruminococcus_bromii_62047_downsampled_14/complete  --params_list 2.1964755 0.7034651

# Downsampled to 10
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.40622 0.166818 ../Analysis/Akkermansia_muciniphila_55290_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.0024135 0.0160841 ../Analysis/Alistipes_finegoldii_56071_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.0881793 0.227832 ../Analysis/Alistipes_onderdonkii_55464_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 8.43938 3.10627 ../Analysis/Alistipes_putredinis_61533_downsampled_10/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 13.9005 0.035724 ../Analysis/Alistipes_shahii_62199_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.656385 0.0924681 ../Analysis/Bacteroidales_bacterium_58650_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 1.91278 0.0536306 ../Analysis/Bacteroides_caccae_53434_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 2.29372 0.828225 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 3.22129 0.248444 ../Analysis/Bacteroides_fragilis_54507_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 2.25642 0.0603274 ../Analysis/Bacteroides_ovatus_58035_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.0163749 0.100595 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 32.233 49.2075 ../Analysis/Bacteroides_uniformis_57318_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 4.9289 0.0037879 ../Analysis/Bacteroides_vulgatus_57955_downsampled_10/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.0132507 0.0850143 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.149372 0.00621968 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_10/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 1.57348 0.0827088 ../Analysis/Dialister_invisus_61905_downsampled_10/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 44.8382 92.7077 ../Analysis/Eubacterium_eligens_61678_downsampled_10/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 1.64542 0.769789 ../Analysis/Eubacterium_rectale_56927_downsampled_10/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 1.09867 1.299 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 9.55467 31.6094 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 5.02523 0.0759358 ../Analysis/Oscillibacter_sp_60799_downsampled_10/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.776827 0.0916526 ../Analysis/Parabacteroides_distasonis_56985_downsampled_10/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 0.742039 0.379442 ../Analysis/Parabacteroides_merdae_56972_downsampled_10/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 2.58953 0.328679 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 3.47521 0.358743 ../Analysis/Prevotella_copri_61740_downsampled_10/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 512.46 417.649 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_10/downsampled_syn_sfs.txt two_epoch --params_list 2.99009 1.50668 ../Analysis/Ruminococcus_bromii_62047_downsampled_10/

# Downsampled to 12
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.406192 0.221325 ../Analysis/Akkermansia_muciniphila_55290_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.00141166 0.0102196 ../Analysis/Alistipes_finegoldii_56071_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.000663046 0.00503453 ../Analysis/Alistipes_onderdonkii_55464_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 9.19344 3.44869 ../Analysis/Alistipes_putredinis_61533_downsampled_12/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 17.2738 0.0394458 ../Analysis/Alistipes_shahii_62199_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.692618 0.135731 ../Analysis/Bacteroidales_bacterium_58650_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 1.40648 0.120183 ../Analysis/Bacteroides_caccae_53434_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 2.75049 1.50239 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 2.40281 0.406655 ../Analysis/Bacteroides_fragilis_54507_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 7.20843 0.0339838 ../Analysis/Bacteroides_ovatus_58035_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.0143413 0.0909163 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 7.59117 10.6886 ../Analysis/Bacteroides_uniformis_57318_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 3.01055 0.00824215 ../Analysis/Bacteroides_vulgatus_57955_downsampled_12/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.0369925 0.202084 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.437146 0.0315171 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_12/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 2.42975 0.0476867 ../Analysis/Dialister_invisus_61905_downsampled_12/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 106.953 222.988 ../Analysis/Eubacterium_eligens_61678_downsampled_12/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 1.58777 0.580629 ../Analysis/Eubacterium_rectale_56927_downsampled_12/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 9.21845 30.6123 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 1.5589 3.85637 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 8.01039 0.0744874 ../Analysis/Oscillibacter_sp_60799_downsampled_12/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.791112 0.105926 ../Analysis/Parabacteroides_distasonis_56985_downsampled_12/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 0.755594 0.341855 ../Analysis/Parabacteroides_merdae_56972_downsampled_12/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 2.46391 0.322072 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 4.11266 0.305928 ../Analysis/Prevotella_copri_61740_downsampled_12/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 310.657 252.905 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_12/downsampled_syn_sfs.txt two_epoch --params_list 3.01357 1.50539 ../Analysis/Ruminococcus_bromii_62047_downsampled_12/

# Downsampled to 14
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_14/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_14/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_14/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_14/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_14/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_14/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_14/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_14/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/downsampled_syn_sfs.txt two_epoch --params_list 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_14/

# Downsampled to 16
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.385545 0.263801 ../Analysis/Akkermansia_muciniphila_55290_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.0216463 0.098936 ../Analysis/Alistipes_finegoldii_56071_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.00874967 0.0439118 ../Analysis/Alistipes_onderdonkii_55464_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 10.6528 4.16344 ../Analysis/Alistipes_putredinis_61533_downsampled_16/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 23.2719 0.0338066 ../Analysis/Alistipes_shahii_62199_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.72274 0.221349 ../Analysis/Bacteroidales_bacterium_58650_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 1.60429 0.0791623 ../Analysis/Bacteroides_caccae_53434_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 2.37427 0.975554 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 3.21259 0.237611 ../Analysis/Bacteroides_fragilis_54507_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 8.04801 0.0348558 ../Analysis/Bacteroides_ovatus_58035_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.00659293 0.0473145 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 1.77798 1.27066 ../Analysis/Bacteroides_uniformis_57318_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 6.1956 0.0100282 ../Analysis/Bacteroides_vulgatus_57955_downsampled_16/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.0104182 0.0720721 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.653196 0.118365 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_16/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 3.02789 0.0450853 ../Analysis/Dialister_invisus_61905_downsampled_16/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 32.0696 65.638 ../Analysis/Eubacterium_eligens_61678_downsampled_16/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 1.65568 0.832631 ../Analysis/Eubacterium_rectale_56927_downsampled_16/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 1.06737 0.337445 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 11.5442 40.7758 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 4.02196 0.0964394 ../Analysis/Oscillibacter_sp_60799_downsampled_16/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.813611 0.161783 ../Analysis/Parabacteroides_distasonis_56985_downsampled_16/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 0.687759 0.605537 ../Analysis/Parabacteroides_merdae_56972_downsampled_16/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 2.51285 0.540036 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_16/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 286.055 233.838 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_16/downsampled_syn_sfs.txt two_epoch --params_list 2.85747 1.30462 ../Analysis/Ruminococcus_bromii_62047_downsampled_16/

# Downsampled to 18
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.391512 0.240012 ../Analysis/Akkermansia_muciniphila_55290_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.0545651 0.199532 ../Analysis/Alistipes_finegoldii_56071_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.000125171 0.00116278 ../Analysis/Alistipes_onderdonkii_55464_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 10.7771 4.21105 ../Analysis/Alistipes_putredinis_61533_downsampled_18/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 3.48215 0.0630991 ../Analysis/Alistipes_shahii_62199_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.747965 0.26547 ../Analysis/Bacteroidales_bacterium_58650_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 3.27649 0.0316115 ../Analysis/Bacteroides_caccae_53434_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 3.18192 1.99256 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 3.0888 0.26575 ../Analysis/Bacteroides_fragilis_54507_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 2.12642 0.108158 ../Analysis/Bacteroides_ovatus_58035_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.13555 0.546058 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 1.76202 1.2601 ../Analysis/Bacteroides_uniformis_57318_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 1.78105 0.0311021 ../Analysis/Bacteroides_vulgatus_57955_downsampled_18/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.125021 0.540209 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.643165 0.092557 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_18/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 2.21215 0.0579241 ../Analysis/Dialister_invisus_61905_downsampled_18/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 83.0965 170.788 ../Analysis/Eubacterium_eligens_61678_downsampled_18/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 3.53508 4.03943 ../Analysis/Eubacterium_rectale_56927_downsampled_18/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 1.09659 0.243501 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 1.31261 3.03154 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 2.93863 0.133149 ../Analysis/Oscillibacter_sp_60799_downsampled_18/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.836116 0.228785 ../Analysis/Parabacteroides_distasonis_56985_downsampled_18/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 0.0352227 0.176049 ../Analysis/Parabacteroides_merdae_56972_downsampled_18/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_18/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 258.088 210.431 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_18/downsampled_syn_sfs.txt two_epoch --params_list 3.00012 1.49253 ../Analysis/Ruminococcus_bromii_62047_downsampled_18/

# Downsampled to 20
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_20/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_20/
# # python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_20/
# # python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_20/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/
# # python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_20/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_20/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_20/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_20/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_20/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_20/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_20/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_20/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_20/downsampled_syn_sfs.txt two_epoch --params_list 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_20/

# Downsampled to 30
# python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.412214 0.184787 ../Analysis/Akkermansia_muciniphila_55290_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.0536736 0.192409 ../Analysis/Alistipes_finegoldii_56071_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.00077792 0.00578665 ../Analysis/Alistipes_onderdonkii_55464_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 9.61159 3.63098 ../Analysis/Alistipes_putredinis_61533_downsampled_30/
# python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 18.7669 0.0499848 ../Analysis/Alistipes_shahii_62199_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.685901 0.116485 ../Analysis/Bacteroidales_bacterium_58650_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 1.66655 0.0717302 ../Analysis/Bacteroides_caccae_53434_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 2.66512 1.39504 ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 2.92751 0.288772 ../Analysis/Bacteroides_fragilis_54507_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 3.52144 0.0446575 ../Analysis/Bacteroides_ovatus_58035_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_stercoris_56735_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.0427133 0.22371 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 1.74006 1.18761 ../Analysis/Bacteroides_uniformis_57318_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 1.13481 0.0430459 ../Analysis/Bacteroides_vulgatus_57955_downsampled_30/
# python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.265088 0.876258 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/
# python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.540065 0.050769 ../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_30/
# python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 18.4977 0.0217412 ../Analysis/Dialister_invisus_61905_downsampled_30/
# python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 55.5916 115.696 ../Analysis/Eubacterium_eligens_61678_downsampled_30/
# python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 2.16442 1.87647 ../Analysis/Eubacterium_rectale_56927_downsampled_30/
# python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 1.06848 0.243666 ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/
# python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 2.43161 7.38277 ../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/
# python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 5.09949 0.0852462 ../Analysis/Oscillibacter_sp_60799_downsampled_30/
# python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.807558 0.136155 ../Analysis/Parabacteroides_distasonis_56985_downsampled_30/
# python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 0.737274 0.391528 ../Analysis/Parabacteroides_merdae_56972_downsampled_30/
# python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 2.57614 0.355933 ../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/
# python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 4.4606 0.315347 ../Analysis/Prevotella_copri_61740_downsampled_30/
# python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 377.278 307.728 ../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/
# python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_30/downsampled_syn_sfs.txt two_epoch --params_list 2.96492 1.46003 ../Analysis/Ruminococcus_bromii_62047_downsampled_30/

# Core Genes
python evaluate_demography.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core --params_list 0.4291101 0.1593162
python evaluate_demography.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Alistipes_finegoldii_56071_downsampled_14/core --params_list 0.2278674 0.4010760
python evaluate_demography.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core --params_list 0.1702887 0.2238585
python evaluate_demography.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/core_empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Alistipes_putredinis_61533_downsampled_14/core --params_list 3.357 0.7725 0.8615 0.001956
python evaluate_demography.py ../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Alistipes_shahii_62199_downsampled_14/core --params_list 1.076841e+03 1.912691e-02
python evaluate_demography.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core --params_list 0.41052325 0.95329267 0.04484037 0.01444436
python evaluate_demography.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_caccae_53434_downsampled_14/core --params_list 1.75093668 0.06852797
python evaluate_demography.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core --params_list 2.390996 1.474717
python evaluate_demography.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_fragilis_54507_downsampled_14/core --params_list 3.6028002 0.1824545
# python evaluate_demography.py ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch --params_list ../Analysis/Bacteroides_massiliensis_44749_downsampled_14/core
python evaluate_demography.py ../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_ovatus_58035_downsampled_14/core --params_list 3.386309e-06 1.051262e-07
python evaluate_demography.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_stercoris_56735_downsampled_14/core --params_list 4.814677 6.331101
python evaluate_demography.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core --params_list 0.2702370 0.7799221
python evaluate_demography.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_uniformis_57318_downsampled_14/core --params_list 3.289016 4.222682
python evaluate_demography.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core --params_list 2.602978e+03 1.218201e-02
python evaluate_demography.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core --params_list 0.7597557 0.1988618
python evaluate_demography.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core --params_list 0.39473449 1.00083578 0.02939562 0.009063
# python evaluate_demography.py ../Analysis/Coprococcus_sp_62244_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch --params_list ../Analysis/Coprococcus_sp_62244_downsampled_14/core
python evaluate_demography.py ../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Dialister_invisus_61905_downsampled_14/core --params_list 1.440642e+03 2.057205e-02
python evaluate_demography.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Eubacterium_eligens_61678_downsampled_14/core --params_list 1.851979 4.309401
python evaluate_demography.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Eubacterium_rectale_56927_downsampled_14/core --params_list 1.6264469 0.5756775
python evaluate_demography.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core --params_list 0.45914113 0.81719497 0.00664349 0.01077599
python evaluate_demography.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core --params_list 0.74771145 1.16721839 0.07135565 0.04700593
python evaluate_demography.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Oscillibacter_sp_60799_downsampled_14/core --params_list 298.32227907 0.03815196
python evaluate_demography.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_syn_downsampled_sfs.txt three_epoch ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core --params_list 0.42029526 1.05936946 0.02598408 0.0730162
python evaluate_demography.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Parabacteroides_merdae_56972_downsampled_14/core --params_list 0.1503162 0.5010130
python evaluate_demography.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core --params_list 2.904182 0.263328
python evaluate_demography.py ../Analysis/Prevotella_copri_61740_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Prevotella_copri_61740_downsampled_14/core --params_list 3.3056753 0.3307203
python evaluate_demography.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core --params_list 8.327632 5.840767
python evaluate_demography.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_syn_downsampled_sfs.txt two_epoch ../Analysis/Ruminococcus_bromii_62047_downsampled_14/core  --params_list 2.5349463 0.7217352
