setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('useful_functions.R')

# SFS comparison

## Read in empirical downsampled SFS (folded)

a_muciniphila_original_folded = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_finegoldii_original_folded = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_onderdonkii_original_folded = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_shahii_original_folded = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_caccae_original_folded = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_cellulosilyticus_original_folded = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
# read_input_sfs('../Analysis/Bacteroides_coprocola_61586_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
# read_input_sfs('../Analysis/Bacteroides_eggerthii_54457_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_fragilis_original_folded = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_ovatus_original_folded = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_stercoris_original_folded = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_thetaiotaomicron_original_folded = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_vulgatus_original_folded = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
d_invisus_original_folded = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_intestinihominis_original_folded = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
e_rectale_original_folded = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
# read_input_sfs('../Analysis/Eubacterium_siraeum_57634_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
oscillibacter_sp_original_folded = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
p_distasonis_original_folded = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
p_merdae_original_folded = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
r_bicirculans_original_folded = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
r_bromii_original_folded = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_syn_downsampled_sfs.txt')

## Read in high recombination SFS (folded)

a_muciniphila_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_empirical_syn_14_downsampled_sfs.txt')
a_finegoldii_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_empirical_syn_14_downsampled_sfs.txt')
a_onderdonkii_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_empirical_syn_14_downsampled_sfs.txt')
a_shahii_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_caccae_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_cellulosilyticus_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_coprocola_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_eggerthii_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_fragilis_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_ovatus_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_stercoris_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_thetaiotaomicron_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_vulgatus_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_empirical_syn_14_downsampled_sfs.txt')
d_invisus_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_empirical_syn_14_downsampled_sfs.txt')
b_intestinihominis_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_empirical_syn_14_downsampled_sfs.txt')
e_rectale_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_empirical_syn_14_downsampled_sfs.txt')
e_siraeum_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_empirical_syn_14_downsampled_sfs.txt')
oscillibacter_sp_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_empirical_syn_14_downsampled_sfs.txt')
p_distasonis_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_empirical_syn_14_downsampled_sfs.txt')
p_merdae_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_empirical_syn_14_downsampled_sfs.txt')
r_bicirculans_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_empirical_syn_14_downsampled_sfs.txt')
r_bromii_HR_folded = read_input_sfs('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_empirical_syn_14_downsampled_sfs.txt')

## Read in demographic model fit for high recombination

### One-epoch
a_muciniphila_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_one_epoch_demography.txt')
a_finegoldii_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_one_epoch_demography.txt')
a_onderdonkii_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_one_epoch_demography.txt')
a_shahii_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_one_epoch_demography.txt')
b_caccae_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_one_epoch_demography.txt')
b_cellulosilyticus_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_one_epoch_demography.txt')
b_coprocola_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_one_epoch_demography.txt')
b_eggerthii_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_one_epoch_demography.txt')
b_fragilis_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_one_epoch_demography.txt')
b_ovatus_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_one_epoch_demography.txt')
b_stercoris_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_one_epoch_demography.txt')
b_thetaiotaomicron_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_one_epoch_demography.txt')
b_vulgatus_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_one_epoch_demography.txt')
d_invisus_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_one_epoch_demography.txt')
b_intestinihominis_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_one_epoch_demography.txt')
e_rectale_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_one_epoch_demography.txt')
e_siraeum_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_one_epoch_demography.txt')
oscillibacter_sp_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_one_epoch_demography.txt')
p_distasonis_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_one_epoch_demography.txt')
p_merdae_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_one_epoch_demography.txt')
r_bicirculans_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_one_epoch_demography.txt')
r_bromii_HR_one_epoch = sfs_from_demography('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_one_epoch_demography.txt')

### Two-epoch
a_muciniphila_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_two_epoch_demography.txt')
a_finegoldii_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_two_epoch_demography.txt')
a_onderdonkii_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_two_epoch_demography.txt')
a_shahii_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_two_epoch_demography.txt')
b_caccae_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_two_epoch_demography.txt')
b_cellulosilyticus_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_two_epoch_demography.txt')
b_coprocola_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_two_epoch_demography.txt')
b_eggerthii_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_two_epoch_demography.txt')
b_fragilis_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_two_epoch_demography.txt')
b_ovatus_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_two_epoch_demography.txt')
b_stercoris_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_two_epoch_demography.txt')
b_thetaiotaomicron_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_two_epoch_demography.txt')
b_vulgatus_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_two_epoch_demography.txt')
d_invisus_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_two_epoch_demography.txt')
b_intestinihominis_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_two_epoch_demography.txt')
e_rectale_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_two_epoch_demography.txt')
e_siraeum_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_two_epoch_demography.txt')
oscillibacter_sp_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_two_epoch_demography.txt')
p_distasonis_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_two_epoch_demography.txt')
p_merdae_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_two_epoch_demography.txt')
r_bicirculans_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_two_epoch_demography.txt')
r_bromii_HR_two_epoch = sfs_from_demography('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_two_epoch_demography.txt')

### Three-epoch
a_muciniphila_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_three_epoch_demography.txt')
a_finegoldii_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_three_epoch_demography.txt')
a_onderdonkii_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_three_epoch_demography.txt')
a_shahii_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_three_epoch_demography.txt')
b_caccae_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_three_epoch_demography.txt')
b_cellulosilyticus_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_three_epoch_demography.txt')
b_coprocola_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_three_epoch_demography.txt')
b_eggerthii_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_three_epoch_demography.txt')
b_fragilis_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_three_epoch_demography.txt')
b_ovatus_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_three_epoch_demography.txt')
b_stercoris_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_three_epoch_demography.txt')
b_thetaiotaomicron_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_three_epoch_demography.txt')
b_vulgatus_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_three_epoch_demography.txt')
d_invisus_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_three_epoch_demography.txt')
b_intestinihominis_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_three_epoch_demography.txt')
e_rectale_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_three_epoch_demography.txt')
e_siraeum_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_three_epoch_demography.txt')
oscillibacter_sp_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_three_epoch_demography.txt')
p_distasonis_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_three_epoch_demography.txt')
p_merdae_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_three_epoch_demography.txt')
r_bicirculans_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_three_epoch_demography.txt')
r_bromii_HR_three_epoch = sfs_from_demography('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_three_epoch_demography.txt')

## HR nonsynonoymous
a_muciniphila_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_inferred_DFE.txt')
a_finegoldii_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_inferred_DFE.txt')
a_onderdonkii_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_inferred_DFE.txt')
a_shahii_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_inferred_DFE.txt')
b_caccae_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_inferred_DFE.txt')
b_cellulosilyticus_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_inferred_DFE.txt')
b_coprocola_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_inferred_DFE.txt')
b_eggerthii_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_inferred_DFE.txt')
b_fragilis_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_inferred_DFE.txt')
b_ovatus_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_inferred_DFE.txt')
b_stercoris_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_inferred_DFE.txt')
b_thetaiotaomicron_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_inferred_DFE.txt')
b_vulgatus_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_inferred_DFE.txt')
d_invisus_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_inferred_DFE.txt')
b_intestinihominis_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_inferred_DFE.txt')
e_rectale_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_inferred_DFE.txt')
e_siraeum_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_inferred_DFE.txt')
oscillibacter_sp_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_inferred_DFE.txt')
p_distasonis_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_inferred_DFE.txt')
p_merdae_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_inferred_DFE.txt')
r_bicirculans_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_inferred_DFE.txt')
r_bromii_HR_nonsyn = empirical_sfs_from_dfe('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_inferred_DFE.txt')

# HR gamma
a_muciniphila_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_inferred_DFE.txt')
a_finegoldii_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_inferred_DFE.txt')
a_onderdonkii_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_inferred_DFE.txt')
a_shahii_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_inferred_DFE.txt')
b_caccae_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_inferred_DFE.txt')
b_cellulosilyticus_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_inferred_DFE.txt')
b_coprocola_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_inferred_DFE.txt')
b_eggerthii_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_inferred_DFE.txt')
b_fragilis_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_inferred_DFE.txt')
b_ovatus_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_inferred_DFE.txt')
b_stercoris_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_inferred_DFE.txt')
b_thetaiotaomicron_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_inferred_DFE.txt')
b_vulgatus_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_inferred_DFE.txt')
d_invisus_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_inferred_DFE.txt')
b_intestinihominis_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_inferred_DFE.txt')
e_rectale_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_inferred_DFE.txt')
e_siraeum_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_inferred_DFE.txt')
oscillibacter_sp_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_inferred_DFE.txt')
p_distasonis_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_inferred_DFE.txt')
p_merdae_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_inferred_DFE.txt')
r_bicirculans_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_inferred_DFE.txt')
r_bromii_HR_gamma = gamma_sfs_from_dfe('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_inferred_DFE.txt')

# HR neugamma
a_muciniphila_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_inferred_DFE.txt')
a_finegoldii_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_inferred_DFE.txt')
a_onderdonkii_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_inferred_DFE.txt')
a_shahii_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_inferred_DFE.txt')
b_caccae_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_inferred_DFE.txt')
b_cellulosilyticus_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_inferred_DFE.txt')
b_coprocola_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_inferred_DFE.txt')
b_eggerthii_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_inferred_DFE.txt')
b_fragilis_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_inferred_DFE.txt')
b_ovatus_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_inferred_DFE.txt')
b_stercoris_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_inferred_DFE.txt')
b_thetaiotaomicron_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_inferred_DFE.txt')
b_vulgatus_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_inferred_DFE.txt')
d_invisus_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_inferred_DFE.txt')
b_intestinihominis_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_inferred_DFE.txt')
e_rectale_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_inferred_DFE.txt')
e_siraeum_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_inferred_DFE.txt')
oscillibacter_sp_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_inferred_DFE.txt')
p_distasonis_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_inferred_DFE.txt')
p_merdae_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_inferred_DFE.txt')
r_bicirculans_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_inferred_DFE.txt')
r_bromii_HR_neugamma = neugamma_sfs_from_dfe('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_inferred_DFE.txt')

## Compare sfs high recombination (proportional)
compare_sfs_high_recombination(proportional_sfs(a_muciniphila_original_folded), 
  proportional_sfs(a_muciniphila_HR_folded)) +
  ggtitle('Akkermansia muciniphila SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(a_finegoldii_original_folded), 
  proportional_sfs(a_finegoldii_HR_folded)) +
  ggtitle('Alistipes finegoldii SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(a_onderdonkii_original_folded), 
  proportional_sfs(a_onderdonkii_HR_folded)) +
  ggtitle('Alistipes onderdonkii SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(a_shahii_original_folded), 
  proportional_sfs(a_shahii_HR_folded)) +
  ggtitle('Alistipes shahii SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_caccae_original_folded), 
  proportional_sfs(b_caccae_HR_folded)) +
  ggtitle('Bacteroides caccae SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_cellulosilyticus_original_folded), 
  proportional_sfs(b_cellulosilyticus_HR_folded)) +
  ggtitle('Bacteroides cellulosilyticus SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_fragilis_original_folded), 
  proportional_sfs(b_fragilis_HR_folded)) +
  ggtitle('Bacteroides fragilis SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_ovatus_original_folded), 
  proportional_sfs(b_ovatus_HR_folded)) +
  ggtitle('Bacteroides ovatus SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_stercoris_original_folded), 
  proportional_sfs(b_stercoris_HR_folded)) +
  ggtitle('Bacteroides stercoris SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_thetaiotaomicron_original_folded), 
  proportional_sfs(b_thetaiotaomicron_HR_folded)) +
  ggtitle('Bacteroides thetaiotaomicron SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_vulgatus_original_folded), 
  proportional_sfs(b_vulgatus_HR_folded)) +
  ggtitle('Bacteroides vulgatus SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(d_invisus_original_folded), 
  proportional_sfs(d_invisus_HR_folded)) +
  ggtitle('Dialister invisus SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(b_intestinihominis_original_folded), 
  proportional_sfs(b_intestinihominis_HR_folded)) +
  ggtitle('Barnesiella intestinihominis SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(e_rectale_original_folded), 
  proportional_sfs(e_rectale_HR_folded)) +
  ggtitle('Eubacterium rectale SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(oscillibacter_sp_original_folded), 
  proportional_sfs(oscillibacter_sp_HR_folded)) +
  ggtitle('Oscillibacter sp. SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(p_distasonis_original_folded), 
  proportional_sfs(p_distasonis_HR_folded)) +
  ggtitle('Parabacteroides distasonis SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(p_merdae_original_folded), 
  proportional_sfs(p_merdae_HR_folded)) +
  ggtitle('Parabacteroides merdae SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(r_bicirculans_original_folded), 
  proportional_sfs(r_bicirculans_HR_folded)) +
  ggtitle('Ruminococcus bicirculans SFS comparison (proportional)')
compare_sfs_high_recombination(proportional_sfs(r_bromii_original_folded), 
  proportional_sfs(r_bromii_HR_folded)) +
  ggtitle('Ruminococcus bromii SFS comparison (proportional)')

## Compare sfs high recombination (count)
compare_sfs_high_recombination((a_muciniphila_original_folded), 
  (a_muciniphila_HR_folded)) +
  ggtitle('Akkermansia muciniphila SFS comparison (count)')
compare_sfs_high_recombination((a_finegoldii_original_folded), 
  (a_finegoldii_HR_folded)) +
  ggtitle('Alistipes finegoldii SFS comparison (count)')
compare_sfs_high_recombination((a_onderdonkii_original_folded), 
  (a_onderdonkii_HR_folded)) +
  ggtitle('Alistipes onderdonkii SFS comparison (count)')
compare_sfs_high_recombination((a_shahii_original_folded), 
  (a_shahii_HR_folded)) +
  ggtitle('Alistipes shahii SFS comparison (count)')
compare_sfs_high_recombination((b_caccae_original_folded), 
  (b_caccae_HR_folded)) +
  ggtitle('Bacteroides caccae SFS comparison (count)')
compare_sfs_high_recombination((b_cellulosilyticus_original_folded), 
  (b_cellulosilyticus_HR_folded)) +
  ggtitle('Bacteroides cellulosilyticus SFS comparison (count)')
compare_sfs_high_recombination((b_fragilis_original_folded), 
  (b_fragilis_HR_folded)) +
  ggtitle('Bacteroides fragilis SFS comparison (count)')
compare_sfs_high_recombination((b_ovatus_original_folded), 
  (b_ovatus_HR_folded)) +
  ggtitle('Bacteroides ovatus SFS comparison (count)')
compare_sfs_high_recombination((b_stercoris_original_folded), 
  (b_stercoris_HR_folded)) +
  ggtitle('Bacteroides stercoris SFS comparison (count)')
compare_sfs_high_recombination((b_thetaiotaomicron_original_folded), 
  (b_thetaiotaomicron_HR_folded)) +
  ggtitle('Bacteroides thetaiotaomicron SFS comparison (count)')
compare_sfs_high_recombination((b_vulgatus_original_folded), 
  (b_vulgatus_HR_folded)) +
  ggtitle('Bacteroides vulgatus SFS comparison (count)')
compare_sfs_high_recombination((d_invisus_original_folded), 
  (d_invisus_HR_folded)) +
  ggtitle('Dialister invisus SFS comparison (count)')
compare_sfs_high_recombination((b_intestinihominis_original_folded), 
  (b_intestinihominis_HR_folded)) +
  ggtitle('Barnesiella intestinihominis SFS comparison (count)')
compare_sfs_high_recombination((e_rectale_original_folded), 
  (e_rectale_HR_folded)) +
  ggtitle('Eubacterium rectale SFS comparison (count)')
compare_sfs_high_recombination((oscillibacter_sp_original_folded), 
  (oscillibacter_sp_HR_folded)) +
  ggtitle('Oscillibacter sp. SFS comparison (count)')
compare_sfs_high_recombination((p_distasonis_original_folded), 
  (p_distasonis_HR_folded)) +
  ggtitle('Parabacteroides distasonis SFS comparison (count)')
compare_sfs_high_recombination((p_merdae_original_folded), 
  (p_merdae_HR_folded)) +
  ggtitle('Parabacteroides merdae SFS comparison (count)')
compare_sfs_high_recombination((r_bicirculans_original_folded), 
  (r_bicirculans_HR_folded)) +
  ggtitle('Ruminococcus bicirculans SFS comparison (count)')
compare_sfs_high_recombination((r_bromii_original_folded), 
  (r_bromii_HR_folded)) +
  ggtitle('Ruminococcus bromii SFS comparison (count)')

## High recombination model fit
a_muciniphila_HR_demography_sfs = compare_sfs(proportional_sfs(a_muciniphila_HR_folded), 
  proportional_sfs(a_muciniphila_HR_one_epoch), 
  proportional_sfs(a_muciniphila_HR_two_epoch), 
  proportional_sfs(a_muciniphila_HR_three_epoch)) +
  ggtitle('Akkermansia muciniphila (HR) model fit')

a_finegoldii_HR_demography_sfs = compare_sfs(proportional_sfs(a_finegoldii_HR_folded), 
  proportional_sfs(a_finegoldii_HR_one_epoch), 
  proportional_sfs(a_finegoldii_HR_two_epoch), 
  proportional_sfs(a_finegoldii_HR_three_epoch)) +
  ggtitle('Alistipes finegoldii (HR) model fit')

a_onderdonkii_HR_demography_sfs = compare_sfs(proportional_sfs(a_onderdonkii_HR_folded), 
  proportional_sfs(a_onderdonkii_HR_one_epoch), 
  proportional_sfs(a_onderdonkii_HR_two_epoch), 
  proportional_sfs(a_onderdonkii_HR_three_epoch)) +
  ggtitle('Alistipes onderdonkii (HR) model fit')

a_shahii_HR_demography_sfs = compare_sfs(proportional_sfs(a_shahii_HR_folded), 
  proportional_sfs(a_shahii_HR_one_epoch), 
  proportional_sfs(a_shahii_HR_two_epoch), 
  proportional_sfs(a_shahii_HR_three_epoch)) +
  ggtitle('Alistipes shahii (HR) model fit')

b_caccae_HR_demography_sfs = compare_sfs(proportional_sfs(b_caccae_HR_folded), 
  proportional_sfs(b_caccae_HR_one_epoch), 
  proportional_sfs(b_caccae_HR_two_epoch), 
  proportional_sfs(b_caccae_HR_three_epoch)) +
  ggtitle('Bacteroides caccae (HR) model fit')

b_cellulosilyticus_HR_demography_sfs = compare_sfs(proportional_sfs(b_cellulosilyticus_HR_folded), 
  proportional_sfs(b_cellulosilyticus_HR_one_epoch), 
  proportional_sfs(b_cellulosilyticus_HR_two_epoch), 
  proportional_sfs(b_cellulosilyticus_HR_three_epoch)) +
  ggtitle('Bacteroides cellulosilyticus (HR) model fit')

b_coprocola_HR_demography_sfs = compare_sfs(proportional_sfs(b_coprocola_HR_folded), 
  proportional_sfs(b_coprocola_HR_one_epoch), 
  proportional_sfs(b_coprocola_HR_two_epoch), 
  proportional_sfs(b_coprocola_HR_three_epoch)) +
  ggtitle('Bacteroides copracola (HR) model fit')

b_eggerthii_HR_demography_sfs = compare_sfs(proportional_sfs(b_eggerthii_HR_folded), 
  proportional_sfs(b_eggerthii_HR_one_epoch), 
  proportional_sfs(b_eggerthii_HR_two_epoch), 
  proportional_sfs(b_eggerthii_HR_three_epoch)) +
  ggtitle('Bacteroides eggerthii (HR) model fit')

b_fragilis_HR_demography_sfs = compare_sfs(proportional_sfs(b_fragilis_HR_folded), 
  proportional_sfs(b_fragilis_HR_one_epoch), 
  proportional_sfs(b_fragilis_HR_two_epoch), 
  proportional_sfs(b_fragilis_HR_three_epoch)) +
  ggtitle('Bacteroides fragilis (HR) model fit')

b_ovatus_HR_demography_sfs = compare_sfs(proportional_sfs(b_ovatus_HR_folded), 
  proportional_sfs(b_ovatus_HR_one_epoch), 
  proportional_sfs(b_ovatus_HR_two_epoch), 
  proportional_sfs(b_ovatus_HR_three_epoch)) +
  ggtitle('Bacteroides ovatus (HR) model fit')

b_stercoris_HR_demography_sfs = compare_sfs(proportional_sfs(b_stercoris_HR_folded), 
  proportional_sfs(b_stercoris_HR_one_epoch), 
  proportional_sfs(b_stercoris_HR_two_epoch), 
  proportional_sfs(b_stercoris_HR_three_epoch)) +
  ggtitle('Bacteroides stercoris (HR) model fit')

b_thetaiotaomicron_HR_demography_sfs = compare_sfs(proportional_sfs(b_thetaiotaomicron_HR_folded), 
  proportional_sfs(b_thetaiotaomicron_HR_one_epoch), 
  proportional_sfs(b_thetaiotaomicron_HR_two_epoch), 
  proportional_sfs(b_thetaiotaomicron_HR_three_epoch)) +
  ggtitle('Bacteroides thetaiotaomicron (HR) model fit')

b_vulgatus_HR_demography_sfs = compare_sfs(proportional_sfs(b_vulgatus_HR_folded), 
  proportional_sfs(b_vulgatus_HR_one_epoch), 
  proportional_sfs(b_vulgatus_HR_two_epoch), 
  proportional_sfs(b_vulgatus_HR_three_epoch)) +
  ggtitle('Bacteroides vulgatus (HR) model fit')

d_invisus_HR_demography_sfs = compare_sfs(proportional_sfs(d_invisus_HR_folded), 
  proportional_sfs(d_invisus_HR_one_epoch), 
  proportional_sfs(d_invisus_HR_two_epoch), 
  proportional_sfs(d_invisus_HR_three_epoch)) +
  ggtitle('Dialister invisus (HR) model fit')

b_intestinihominis_HR_demography_sfs = compare_sfs(proportional_sfs(b_intestinihominis_HR_folded), 
  proportional_sfs(b_intestinihominis_HR_one_epoch), 
  proportional_sfs(b_intestinihominis_HR_two_epoch), 
  proportional_sfs(b_intestinihominis_HR_three_epoch)) +
  ggtitle('Barnesiella intestinihominis (HR) model fit')

e_rectale_HR_demography_sfs = compare_sfs(proportional_sfs(e_rectale_HR_folded), 
  proportional_sfs(e_rectale_HR_one_epoch), 
  proportional_sfs(e_rectale_HR_two_epoch), 
  proportional_sfs(e_rectale_HR_three_epoch)) +
  ggtitle('Eubacterium rectale (HR) model fit')

e_siraeum_HR_demography_sfs = compare_sfs(proportional_sfs(e_siraeum_HR_folded), 
  proportional_sfs(e_siraeum_HR_one_epoch), 
  proportional_sfs(e_siraeum_HR_two_epoch), 
  proportional_sfs(e_siraeum_HR_three_epoch)) +
  ggtitle('Eubacterium siraeum (HR) model fit')

oscillibacter_sp_HR_demography_sfs = compare_sfs(proportional_sfs(oscillibacter_sp_HR_folded), 
  proportional_sfs(oscillibacter_sp_HR_one_epoch), 
  proportional_sfs(oscillibacter_sp_HR_two_epoch), 
  proportional_sfs(oscillibacter_sp_HR_three_epoch)) +
  ggtitle('Oscillibacter sp. (HR) model fit')

p_distasonis_HR_demography_sfs = compare_sfs(proportional_sfs(p_distasonis_HR_folded), 
  proportional_sfs(p_distasonis_HR_one_epoch), 
  proportional_sfs(p_distasonis_HR_two_epoch), 
  proportional_sfs(p_distasonis_HR_three_epoch)) +
  ggtitle('Parabacteroides distasonis (HR) model fit')

p_merdae_HR_demography_sfs = compare_sfs(proportional_sfs(p_merdae_HR_folded), 
  proportional_sfs(p_merdae_HR_one_epoch), 
  proportional_sfs(p_merdae_HR_two_epoch), 
  proportional_sfs(p_merdae_HR_three_epoch)) +
  ggtitle('Parabacteroides merdae (HR) model fit')

r_bicirculans_HR_demography_sfs = compare_sfs(proportional_sfs(r_bicirculans_HR_folded), 
  proportional_sfs(r_bicirculans_HR_one_epoch), 
  proportional_sfs(r_bicirculans_HR_two_epoch), 
  proportional_sfs(r_bicirculans_HR_three_epoch)) +
  ggtitle('Ruminococcus bicirculans (HR) model fit')

r_bromii_HR_demography_sfs = compare_sfs(proportional_sfs(r_bromii_HR_folded), 
  proportional_sfs(r_bromii_HR_one_epoch), 
  proportional_sfs(r_bromii_HR_two_epoch), 
  proportional_sfs(r_bromii_HR_three_epoch)) +
  ggtitle('Ruminococcus bromii (HR) model fit')

a_muciniphila_HR_demography_sfs +
  a_finegoldii_HR_demography_sfs +
  a_onderdonkii_HR_demography_sfs +
  a_shahii_HR_demography_sfs +
  plot_layout(ncol=1)


## Likelihood surfaces
p1_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/likelihood_surface.csv')
p1_l + ggtitle('A. muciniphila (High recombination)')

p2_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/likelihood_surface.csv')
p2_l + ggtitle('A. finegoldii (High recombination)')

p3_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/likelihood_surface.csv')
p3_l + ggtitle('A. onderdonkii (High recombination)')

p4_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Alistipes_shahii_62199/likelihood_surface.csv')
p4_l + ggtitle('A. shahii (High recombination)')

p5_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_caccae_53434/likelihood_surface.csv')
p5_l + ggtitle('B. caccae (High recombination)')

p6_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/likelihood_surface.csv')
p6_l + ggtitle('B. cellulosilyticus (High recombination)')

p7_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/likelihood_surface.csv')
p7_l + ggtitle('B. coprocola (High recombination)')

p8_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/likelihood_surface.csv')
p8_l + ggtitle('B. eggerthii (High recombination)')

p9_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/likelihood_surface.csv')
p9_l + ggtitle('B. fragilis (High recombination)')

p10_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/likelihood_surface.csv')
p10_l + ggtitle('B. ovatus (High recombination)')

p11_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/likelihood_surface.csv')
p11_l + ggtitle('B. stercoris (High recombination)')

p12_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/likelihood_surface.csv')
p12_l + ggtitle('B. thetaiotaomicron (High recombination)')

p13_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/likelihood_surface.csv')
p13_l + ggtitle('B. vulgatus (High recombination)')

p14_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/likelihood_surface.csv')
p14_l + ggtitle('B. intestinihominis (High recombination)')

p15_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Dialister_invisus_61905/likelihood_surface.csv')
p15_l + ggtitle('D. invisus (High recombination)')

p16_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Eubacterium_rectale_56927/likelihood_surface.csv')
p16_l + ggtitle('E. rectale (High recombination)')

p17_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/likelihood_surface.csv')
p17_l + ggtitle('E. siraeum (High recombination)')

p18_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Oscillibacter_sp_60799/likelihood_surface.csv')
p18_l + ggtitle('Oscillibacter sp. (High recombination)')

p19_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/likelihood_surface.csv')
p19_l + ggtitle('P. distasonis (High recombination)')

p20_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/likelihood_surface.csv')
p20_l + ggtitle('P. merdae (High recombination)')

p21_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/likelihood_surface.csv')
p21_l + ggtitle('R. bicirculans (High recombination)')

p22_l = plot_likelihood_surface_contour('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/likelihood_surface.csv')
p22_l + ggtitle('R. bromii (High recombination)')

## Survival curve
a_muciniphila_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/survival_curve.csv')
plot_survival_curve(a_muciniphila_HR_survival_curve) + 
  ggtitle('A. muciniphila survival curve')

a_finegoldii_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/survival_curve.csv')
plot_survival_curve(a_finegoldii_HR_survival_curve) +
  ggtitle('A. finegoldii survival curve')

a_onderdonkii_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/survival_curve.csv')
plot_survival_curve(a_onderdonkii_HR_survival_curve) +
  ggtitle('A. onderdonkii survival curve')

a_shahii_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Alistipes_shahii_62199/survival_curve.csv')
plot_survival_curve(a_shahii_HR_survival_curve) +
  ggtitle('A. shahii survival curve')

b_caccae_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_caccae_53434/survival_curve.csv')
plot_survival_curve(b_caccae_HR_survival_curve) +
  ggtitle('B. caccae survival curve')

b_cellulosilyticus_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/survival_curve.csv')
plot_survival_curve(b_cellulosilyticus_HR_survival_curve) +
  ggtitle('B. cellulosilyticus survival curve')

b_coprocola_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/survival_curve.csv')
plot_survival_curve(b_coprocola_HR_survival_curve) +
  ggtitle('B. coprocola survival curve')

b_eggerthii_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/survival_curve.csv')
plot_survival_curve(b_eggerthii_HR_survival_curve) +
  ggtitle('B. eggerthii survival curve')

b_fragilis_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/survival_curve.csv')
plot_survival_curve(b_fragilis_HR_survival_curve) +
  ggtitle('B. fragilis survival curve')

b_ovatus_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/survival_curve.csv')
plot_survival_curve(b_ovatus_HR_survival_curve) +
  ggtitle('B. ovatus survival curve')

b_stercoris_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/survival_curve.csv')
plot_survival_curve(b_stercoris_HR_survival_curve) +
  ggtitle('B. stercoris survival curve')

b_thetaiotaomicron_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/survival_curve.csv')
plot_survival_curve(b_thetaiotaomicron_HR_survival_curve) +
  ggtitle('B. thetaiotaomicron survival curve')

b_vulgatus_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/survival_curve.csv')
plot_survival_curve(b_vulgatus_HR_survival_curve) +
  ggtitle('B. vulgatus survival curve')

d_invisus_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Dialister_invisus_61905/survival_curve.csv')
plot_survival_curve(d_invisus_HR_survival_curve) +
  ggtitle('D. invisus survival curve')

b_intestinihominis_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/survival_curve.csv')
plot_survival_curve(b_intestinihominis_HR_survival_curve) +
  ggtitle('B. intestinihominis survival curve')

# e_rectale_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Eubacterium_rectale_56927/survival_curve.csv')
# plot_survival_curve(e_rectale_HR_survival_curve) +
#   ggtitle('A. onderdonkii survival curve')

e_siraeum_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/survival_curve.csv')
plot_survival_curve(e_siraeum_HR_survival_curve) +
  ggtitle('E. siraeum survival curve')

oscillibacter_sp_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Oscillibacter_sp_60799/survival_curve.csv')
plot_survival_curve(oscillibacter_sp_HR_survival_curve) +
  ggtitle('Oscillibacter sp. survival curve')

p_distasonis_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/survival_curve.csv')
plot_survival_curve(p_distasonis_HR_survival_curve) +
  ggtitle('P. distasonis survival curve')

p_merdae_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/survival_curve.csv')
plot_survival_curve(p_merdae_HR_survival_curve) +
  ggtitle('P. merdae survival curve')

r_bicirculans_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/survival_curve.csv')
plot_survival_curve(r_bicirculans_HR_survival_curve) +
  ggtitle('R. bicirculans survival curve')

r_bromii_HR_survival_curve = read.csv('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/survival_curve.csv')
plot_survival_curve(r_bromii_HR_survival_curve) +
  ggtitle('R. bromii survival curve')

## Demography comparison
shared_species_list = c(
  'Akkermansia muciniphila',
  'Alistipes finegoldii',
  'Alistipes onderdonkii',
  'Alistipes shahii',
  'Bacteroides caccae',
  'Bacteroides cellulosilyticus',
  'Bacteroides fragilis',
  'Bacteroides stercoris',
  'Bacteroides thetaiotaomicron',
  'Bacteroides vulgatus',
  'Barnesiella intestinihominis',
  'Dialister invisus',
  'Eubacterium rectale',
  'Oscillibacter sp.',
  'Parabacteroides distasonis',
  'Parabacteroides merdae',
  'Ruminococcus bicirculans',
  'Ruminococcus bromii'
)

original_demography_file_list = c(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_shahii_62199_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Dialister_invisus_61905_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_two_epoch_demography.txt'
)

original_sfs_list = c(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_syn_downsampled_sfs.txt',
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_syn_downsampled_sfs.txt'
)

original_likelihood_surface_list = c(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Alistipes_shahii_62199_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Dialister_invisus_61905_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_likelihood_surface.csv',
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_likelihood_surface.csv'
)

hr_demography_file_list = c(
  '../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_two_epoch_demography.txt',
  '../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_two_epoch_demography.txt'
)

hr_sfs_list = c(
  '../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_empirical_syn_sfs.txt',
  '../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_empirical_syn_sfs.txt'
)

hr_likelihood_surface_list = c(
  '../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Alistipes_finegoldii_56071/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Alistipes_shahii_62199/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Bacteroides_caccae_53434/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Bacteroides_fragilis_54507/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Bacteroides_stercoris_56735/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Dialister_invisus_61905/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Eubacterium_rectale_56927/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Oscillibacter_sp_60799/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Parabacteroides_merdae_56972/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/likelihood_surface.csv',
  '../HighRecombinationAnalysis/Ruminococcus_bromii_62047/likelihood_surface.csv'
)

hr_dfe_file_list = c(
  '../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_inferred_DFE.txt',
  '../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_inferred_DFE.txt'
)

fd_demography_file_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Dialister_invisus_61905/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/two_epoch_demography.txt'
)

fd_sfs_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Dialister_invisus_61905/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_empirical_syn_downsampled_sfs.txt'
)


fd_likelihood_surface_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/likelihood_surface.csv',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/likelihood_surface.csv',
  '../SupplementaryAnalysis/Dialister_invisus_61905/likelihood_surface.csv',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/likelihood_surface.csv',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/likelihood_surface.csv',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/likelihood_surface.csv',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/likelihood_surface.csv',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/likelihood_surface.csv',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/likelihood_surface.csv'
)

fd_dfe_file_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Dialister_invisus_61905/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_inferred_DFE.txt',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_inferred_DFE.txt'
)


original_demography_df = data.frame(species=shared_species_list, 
  nu_mle = numeric(18),
  time_mle = numeric(18)
)

for (i in 1:length(shared_species_list)) {
  # nu_mle
  original_demography_df[i, 2] = return_nu_mle(original_likelihood_surface_list[i])
  # tau_mle
  original_demography_df[i, 3] = return_time_mle(original_likelihood_surface_list[i], 
    original_sfs_list[i], 
    original_demography_file_list[i])
}

original_demography_df

hr_demography_df = data.frame(species=shared_species_list,
  sample_size = numeric(18))

for (i in 1:length(shared_species_list)) {
  hr_demography_df[i, 2] = get_species_prevalence(hr_sfs_list[i])
}

hr_demography_df

ggplot(hr_demography_df, aes(x = reorder(species, sample_size), y = sample_size)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sample Size by Species (HR Data)",
       x = "Species",
       y = "Sample Size") +
  theme_minimal()

hr_demography_df = data.frame(species=shared_species_list, 
  nu_mle = numeric(18),
  time_mle = numeric(18)
)

for (i in 1:length(shared_species_list)) {
  # nu_mle
  hr_demography_df[i, 2] = return_nu_mle(hr_likelihood_surface_list[i])
  # tau_mle
  hr_demography_df[i, 3] = return_time_mle(hr_likelihood_surface_list[i], 
    hr_sfs_list[i], 
    hr_demography_file_list[i])
}

hr_demography_df

x_label_text = expression(nu == frac(N[current], N[ancestral]))

original_demography_scatter = ggscatter(original_demography_df, x="nu_mle", y="time_mle", color='species', size=3) +
  ylab('Estimated time in years since most recent demographic event') +
  xlab(x_label_text) +
  geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
  scale_shape_manual(name = "Best-Fit Demographic Model",
                     labels = c("Three Epoch", "Two Epoch"),
                     values = c(17, 19)) +
  # geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=all_genes_typeface) +
  guides(color=guide_legend(title="Species")) +
  scale_x_log10(limits=c(1e-2, 2e5)) +
  scale_y_log10(limits=c(2e2, 1e7)) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none')  +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16))

original_demography_scatter

plot_build <- ggplot_build(original_demography_scatter)
color_mapping <- plot_build$data[[1]]$colour
print(color_mapping)

difference_plot =
  original_demography_scatter +
  geom_segment(aes(x=original_demography_df$nu_mle, y=original_demography_df$time_mle,
    xend=hr_demography_df$nu_mle, yend=hr_demography_df$time_mle),
    linejoin='round',
    lineend='round',
    linetype=1,
    color=color_mapping,
    arrow = arrow(length=unit(0.2, 'cm'))) +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  ggtitle('Change in demography before and after taking top 50th % of recombination')

difference_plot

## Supplemental SFSs

supplementary_demography_file_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_putredinis_61533/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Alistipes_sp_60764/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroidales_bacterium_58650/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_coprocola_61586/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_eggerthii_54457/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_massiliensis_44749/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_ovatus_58035/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_plebeius_61623/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_uniformis_57318/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Coprococcus_sp_62244/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Dialister_invisus_61905/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Eubacterium_eligens_61678/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Eubacterium_siraeum_57634/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Prevotella_copri_61740/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Roseburia_intestinalis_56239/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Roseburia_inulinivorans_61943/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/two_epoch_demography.txt',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/two_epoch_demography.txt'
)

supplementary_likelihood_surface_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_putredinis_61533/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/likelihood_surface.csv',
  '../SupplementaryAnalysis/Alistipes_sp_60764/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroidales_bacterium_58650/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_coprocola_61586/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_eggerthii_54457/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_massiliensis_44749/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_ovatus_58035/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_plebeius_61623/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_uniformis_57318/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/likelihood_surface.csv',
  '../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/likelihood_surface.csv',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/likelihood_surface.csv',
  '../SupplementaryAnalysis/Coprococcus_sp_62244/likelihood_surface.csv',
  '../SupplementaryAnalysis/Dialister_invisus_61905/likelihood_surface.csv',
  '../SupplementaryAnalysis/Eubacterium_eligens_61678/likelihood_surface.csv',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/likelihood_surface.csv',
  '../SupplementaryAnalysis/Eubacterium_siraeum_57634/likelihood_surface.csv',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/likelihood_surface.csv',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/likelihood_surface.csv',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/likelihood_surface.csv',
  '../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/likelihood_surface.csv',
  '../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/likelihood_surface.csv',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/likelihood_surface.csv',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/likelihood_surface.csv',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/likelihood_surface.csv',
  '../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/likelihood_surface.csv',
  '../SupplementaryAnalysis/Prevotella_copri_61740/likelihood_surface.csv',
  '../SupplementaryAnalysis/Roseburia_intestinalis_56239/likelihood_surface.csv',
  '../SupplementaryAnalysis/Roseburia_inulinivorans_61943/likelihood_surface.csv',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/likelihood_surface.csv',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/likelihood_surface.csv'
)

supplementary_downsampled_sfs_file_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_putredinis_61533/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_sp_60764/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Coprococcus_sp_62244/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Dialister_invisus_61905/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_eligens_61678/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Prevotella_copri_61740/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_empirical_syn_downsampled_sfs.txt',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_empirical_syn_downsampled_sfs.txt'
)


supplementary_full_sfs_file_list = c(
  '../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_putredinis_61533/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_shahii_62199/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Alistipes_sp_60764/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_caccae_53434/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Coprococcus_sp_62244/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Dialister_invisus_61905/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_eligens_61678/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_rectale_56927/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Oscillibacter_sp_60799/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Prevotella_copri_61740/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_empirical_syn_sfs.txt',
  '../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_empirical_syn_sfs.txt'
)

supplementary_species_list = c(
  'Akkermansia muciniphila',
  'Alistipes finegoldii',
  'Alistipes onderdonkii',
  'Alistipes putredinis',
  'Alistipes shahii',
  'Alistipes sp.',
  'Bacteroidale bacterium',
  'Bacteroides caccae',
  'Bacteroides cellulosilyticus',
  'Bacteroides coprocola',
  'Bacteroides eggerthii',
  'Bacteroides fragilis',
  'Bacteroides massiliensis',
  'Bacteroides ovatus',
  'Bacteroides plebeius',
  'Bacteroides stercoris',
  'Bacteroides thetaiotaomicron',
  'Bacteroides uniformis',
  'Bacteroides vulgatus',
  'Bacteroides xylanisolvens',
  'Barnesiella intestinihominis',
  'Coprococcus sp.',
  'Dialister invisus',
  'Eubacterium eligens',
  'Eubacterium rectale',
  'Eubacterium siraeum',
  'Faecalibacterium prausnitzii (57453)',
  'Faecalibacterium prausnitzii (61481)',
  'Faecalibacterium prausnitzii (62201)',
  'Lachnospiraceae bacterium',
  'Odoribacter splanchnicus',
  'Oscillibacter sp.',
  'Parabacteroides distasonis',
  'Parabacteroides merdae',
  'Phascolarctobacterium sp.',
  'Prevotella copri',
  'Roseburia intestinalis',
  'Roseburia inulinivorans',
  'Ruminococcus bicirculans',
  'Ruminococcus bromii'
)

supplementary_demography_df = data.frame(species=supplementary_species_list, 
  sample_size=numeric(40)
)

for (i in 1:length(supplementary_species_list)) {
  supplementary_demography_df[i, 2] = get_species_prevalence(supplementary_full_sfs_file_list[i])
}

supplementary_demography_df

ggplot(supplementary_demography_df, aes(x = reorder(species, sample_size), y = sample_size)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sample Size by Species (Full Data)",
       x = "Species",
       y = "Sample Size") +
  theme_minimal()

supplementary_demography_df = data.frame(species=supplementary_species_list, 
  nu_mle = numeric(40),
  time_mle = numeric(40)
)

for (i in 1:length(supplementary_species_list)) {
  # nu_mle
  supplementary_demography_df[i, 2] = return_nu_mle(supplementary_likelihood_surface_list[i])
  # tau_mle
  supplementary_demography_df[i, 3] = return_time_mle(supplementary_likelihood_surface_list[i], 
    supplementary_downsampled_sfs_file_list[i], 
    supplementary_demography_file_list[i])
}

supplementary_demography_df

options(ggrepel.max.overlaps = Inf)

supplementary_demography_scatter = ggscatter(supplementary_demography_df, x="nu_mle", y="time_mle", color='species', size=3) +
  ylab('Estimated time in years since most recent demographic event') +
  xlab(x_label_text) +
  geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
  scale_shape_manual(name = "Best-Fit Demographic Model",
                     labels = c("Three Epoch", "Two Epoch"),
                     values = c(17, 19)) +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=3) +
  guides(color=guide_legend(title="Species")) +
  scale_x_log10(limits=c(1e-2, 2e5)) +
  scale_y_log10(limits=c(2e2, 1e7)) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none')  +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16))

supplementary_demography_scatter

# Supplementary species model fit
## Read in full data SFS (folded)

a_muciniphila_FD_folded = read_input_sfs('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_empirical_syn_downsampled_sfs.txt')
a_finegoldii_FD_folded = read_input_sfs('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_empirical_syn_downsampled_sfs.txt')
a_onderdonkii_FD_folded = read_input_sfs('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_empirical_syn_downsampled_sfs.txt')
a_putredinis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Alistipes_putredinis_61533/core_empirical_syn_downsampled_sfs.txt')
a_shahii_FD_folded = read_input_sfs('../SupplementaryAnalysis/Alistipes_shahii_62199/core_empirical_syn_downsampled_sfs.txt')
alistipes_sp_FD_folded = read_input_sfs('../SupplementaryAnalysis/Alistipes_sp_60764/core_empirical_syn_downsampled_sfs.txt')
b_bacterium_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_empirical_syn_downsampled_sfs.txt')
b_caccae_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_caccae_53434/core_empirical_syn_downsampled_sfs.txt')
b_cellulosilyticus_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_empirical_syn_downsampled_sfs.txt')
b_coprocola_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_empirical_syn_downsampled_sfs.txt')
b_eggerthii_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_empirical_syn_downsampled_sfs.txt')
b_fragilis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_empirical_syn_downsampled_sfs.txt')
b_massiliensis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_empirical_syn_downsampled_sfs.txt')
b_ovatus_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_empirical_syn_downsampled_sfs.txt')
b_plebeius_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_empirical_syn_downsampled_sfs.txt')
b_stercoris_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_empirical_syn_downsampled_sfs.txt')
b_thetaiotaomicron_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_empirical_syn_downsampled_sfs.txt')
b_uniformis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_empirical_syn_downsampled_sfs.txt')
b_vulgatus_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_empirical_syn_downsampled_sfs.txt')
b_xylanisolvens_FD_folded = read_input_sfs('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_empirical_syn_downsampled_sfs.txt')
b_intestinihominis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_empirical_syn_downsampled_sfs.txt')
coprococcus_sp_FD_folded = read_input_sfs('../SupplementaryAnalysis/Coprococcus_sp_62244/core_empirical_syn_downsampled_sfs.txt')
d_invisus_FD_folded = read_input_sfs('../SupplementaryAnalysis/Dialister_invisus_61905/core_empirical_syn_downsampled_sfs.txt')
e_eligens_FD_folded = read_input_sfs('../SupplementaryAnalysis/Eubacterium_eligens_61678/core_empirical_syn_downsampled_sfs.txt')
e_rectale_FD_folded = read_input_sfs('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_empirical_syn_downsampled_sfs.txt')
e_siraeum_FD_folded = read_input_sfs('../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_empirical_syn_downsampled_sfs.txt')
f_prausnitzii_57453_FD_folded = read_input_sfs('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_empirical_syn_downsampled_sfs.txt')
f_prausnitzii_61481_FD_folded = read_input_sfs('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_empirical_syn_downsampled_sfs.txt')
f_prausnitzii_62201_FD_folded = read_input_sfs('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_empirical_syn_downsampled_sfs.txt')
l_bacterium_FD_folded = read_input_sfs('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_empirical_syn_downsampled_sfs.txt')
o_splanchnicus_FD_folded = read_input_sfs('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_empirical_syn_downsampled_sfs.txt')
oscillibacter_sp_FD_folded = read_input_sfs('../SupplementaryAnalysis/Oscillibacter_sp_60799/core_empirical_syn_downsampled_sfs.txt')
p_distasonis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_empirical_syn_downsampled_sfs.txt')
p_merdae_FD_folded = read_input_sfs('../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_empirical_syn_downsampled_sfs.txt')
phascolarctobacterium_sp_FD_folded = read_input_sfs('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_empirical_syn_downsampled_sfs.txt')
p_copri_FD_folded = read_input_sfs('../SupplementaryAnalysis/Prevotella_copri_61740/core_empirical_syn_downsampled_sfs.txt')
r_intestinalis_FD_folded = read_input_sfs('../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_empirical_syn_downsampled_sfs.txt')
r_inulinivorans_FD_folded = read_input_sfs('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_empirical_syn_downsampled_sfs.txt')
r_bicirculans_FD_folded = read_input_sfs('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_empirical_syn_downsampled_sfs.txt')
r_bromii_FD_folded = read_input_sfs('../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_empirical_syn_downsampled_sfs.txt')

## Read in demographic model fit for high recombination

### One-epoch
a_muciniphila_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/one_epoch_demography.txt')
a_finegoldii_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_finegoldii_56071/one_epoch_demography.txt')
a_onderdonkii_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/one_epoch_demography.txt')
a_putredinis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_putredinis_61533/one_epoch_demography.txt')
a_shahii_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_shahii_62199/one_epoch_demography.txt')
alistipes_sp_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_sp_60764/one_epoch_demography.txt')
b_bacterium_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/one_epoch_demography.txt')
b_caccae_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_caccae_53434/one_epoch_demography.txt')
b_cellulosilyticus_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/one_epoch_demography.txt')
b_coprocola_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_coprocola_61586/one_epoch_demography.txt')
b_eggerthii_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/one_epoch_demography.txt')
b_fragilis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_fragilis_54507/one_epoch_demography.txt')
b_massiliensis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/one_epoch_demography.txt')
b_ovatus_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_ovatus_58035/one_epoch_demography.txt')
b_plebeius_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_plebeius_61623/one_epoch_demography.txt')
b_stercoris_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_stercoris_56735/one_epoch_demography.txt')
b_thetaiotaomicron_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/one_epoch_demography.txt')
b_uniformis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_uniformis_57318/one_epoch_demography.txt')
b_vulgatus_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/one_epoch_demography.txt')
b_xylanisolvens_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/one_epoch_demography.txt')
b_intestinihominis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/one_epoch_demography.txt')
coprococcus_sp_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Coprococcus_sp_62244/one_epoch_demography.txt')
d_invisus_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Dialister_invisus_61905/one_epoch_demography.txt')
e_eligens_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_eligens_61678/one_epoch_demography.txt')
e_rectale_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_rectale_56927/one_epoch_demography.txt')
e_siraeum_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_siraeum_57634/one_epoch_demography.txt')
f_prausnitzii_57453_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/one_epoch_demography.txt')
f_prausnitzii_61481_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/one_epoch_demography.txt')
f_prausnitzii_62201_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/one_epoch_demography.txt')
l_bacterium_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/one_epoch_demography.txt')
o_splanchnicus_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/one_epoch_demography.txt')
oscillibacter_sp_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Oscillibacter_sp_60799/one_epoch_demography.txt')
p_distasonis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/one_epoch_demography.txt')
p_merdae_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Parabacteroides_merdae_56972/one_epoch_demography.txt')
phascolarctobacterium_sp_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/one_epoch_demography.txt')
p_copri_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Prevotella_copri_61740/one_epoch_demography.txt')
r_intestinalis_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Roseburia_intestinalis_56239/one_epoch_demography.txt')
r_inulinivorans_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/one_epoch_demography.txt')
r_bicirculans_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/one_epoch_demography.txt')
r_bromii_FD_one_epoch = sfs_from_demography('../SupplementaryAnalysis/Ruminococcus_bromii_62047/one_epoch_demography.txt')

### Two-epoch
a_muciniphila_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/two_epoch_demography.txt')
a_finegoldii_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_finegoldii_56071/two_epoch_demography.txt')
a_onderdonkii_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/two_epoch_demography.txt')
a_putredinis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_putredinis_61533/two_epoch_demography.txt')
a_shahii_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_shahii_62199/two_epoch_demography.txt')
alistipes_sp_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_sp_60764/two_epoch_demography.txt')
b_bacterium_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/two_epoch_demography.txt')
b_caccae_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_caccae_53434/two_epoch_demography.txt')
b_cellulosilyticus_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/two_epoch_demography.txt')
b_coprocola_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_coprocola_61586/two_epoch_demography.txt')
b_eggerthii_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/two_epoch_demography.txt')
b_fragilis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_fragilis_54507/two_epoch_demography.txt')
b_massiliensis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/two_epoch_demography.txt')
b_ovatus_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_ovatus_58035/two_epoch_demography.txt')
b_plebeius_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_plebeius_61623/two_epoch_demography.txt')
b_stercoris_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_stercoris_56735/two_epoch_demography.txt')
b_thetaiotaomicron_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/two_epoch_demography.txt')
b_uniformis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_uniformis_57318/two_epoch_demography.txt')
b_vulgatus_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/two_epoch_demography.txt')
b_xylanisolvens_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/two_epoch_demography.txt')
b_intestinihominis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/two_epoch_demography.txt')
coprococcus_sp_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Coprococcus_sp_62244/two_epoch_demography.txt')
d_invisus_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Dialister_invisus_61905/two_epoch_demography.txt')
e_eligens_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_eligens_61678/two_epoch_demography.txt')
e_rectale_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_rectale_56927/two_epoch_demography.txt')
e_siraeum_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_siraeum_57634/two_epoch_demography.txt')
f_prausnitzii_57453_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/two_epoch_demography.txt')
f_prausnitzii_61481_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/two_epoch_demography.txt')
f_prausnitzii_62201_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/two_epoch_demography.txt')
l_bacterium_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/two_epoch_demography.txt')
o_splanchnicus_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/two_epoch_demography.txt')
oscillibacter_sp_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Oscillibacter_sp_60799/two_epoch_demography.txt')
p_distasonis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/two_epoch_demography.txt')
p_merdae_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Parabacteroides_merdae_56972/two_epoch_demography.txt')
phascolarctobacterium_sp_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/two_epoch_demography.txt')
p_copri_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Prevotella_copri_61740/two_epoch_demography.txt')
r_intestinalis_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Roseburia_intestinalis_56239/two_epoch_demography.txt')
r_inulinivorans_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/two_epoch_demography.txt')
r_bicirculans_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/two_epoch_demography.txt')
r_bromii_FD_two_epoch = sfs_from_demography('../SupplementaryAnalysis/Ruminococcus_bromii_62047/two_epoch_demography.txt')

### Three-epoch
a_muciniphila_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/three_epoch_demography.txt')
a_finegoldii_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_finegoldii_56071/three_epoch_demography.txt')
a_onderdonkii_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/three_epoch_demography.txt')
a_putredinis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_putredinis_61533/three_epoch_demography.txt')
a_shahii_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_shahii_62199/three_epoch_demography.txt')
alistipes_sp_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Alistipes_sp_60764/three_epoch_demography.txt')
b_bacterium_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/three_epoch_demography.txt')
b_caccae_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_caccae_53434/three_epoch_demography.txt')
b_cellulosilyticus_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/three_epoch_demography.txt')
b_coprocola_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_coprocola_61586/three_epoch_demography.txt')
b_eggerthii_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/three_epoch_demography.txt')
b_fragilis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_fragilis_54507/three_epoch_demography.txt')
b_massiliensis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/three_epoch_demography.txt')
b_ovatus_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_ovatus_58035/three_epoch_demography.txt')
b_plebeius_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_plebeius_61623/three_epoch_demography.txt')
b_stercoris_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_stercoris_56735/three_epoch_demography.txt')
b_thetaiotaomicron_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/three_epoch_demography.txt')
b_uniformis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_uniformis_57318/three_epoch_demography.txt')
b_vulgatus_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/three_epoch_demography.txt')
b_xylanisolvens_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/three_epoch_demography.txt')
b_intestinihominis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/three_epoch_demography.txt')
coprococcus_sp_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Coprococcus_sp_62244/three_epoch_demography.txt')
d_invisus_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Dialister_invisus_61905/three_epoch_demography.txt')
e_eligens_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_eligens_61678/three_epoch_demography.txt')
e_rectale_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_rectale_56927/three_epoch_demography.txt')
e_siraeum_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Eubacterium_siraeum_57634/three_epoch_demography.txt')
f_prausnitzii_57453_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/three_epoch_demography.txt')
f_prausnitzii_61481_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/three_epoch_demography.txt')
f_prausnitzii_62201_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/three_epoch_demography.txt')
l_bacterium_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/three_epoch_demography.txt')
o_splanchnicus_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/three_epoch_demography.txt')
oscillibacter_sp_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Oscillibacter_sp_60799/three_epoch_demography.txt')
p_distasonis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/three_epoch_demography.txt')
p_merdae_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Parabacteroides_merdae_56972/three_epoch_demography.txt')
phascolarctobacterium_sp_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/three_epoch_demography.txt')
p_copri_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Prevotella_copri_61740/three_epoch_demography.txt')
r_intestinalis_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Roseburia_intestinalis_56239/three_epoch_demography.txt')
r_inulinivorans_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/three_epoch_demography.txt')
r_bicirculans_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/three_epoch_demography.txt')
r_bromii_FD_three_epoch = sfs_from_demography('../SupplementaryAnalysis/Ruminococcus_bromii_62047/three_epoch_demography.txt')

# Nonsyn empirical

a_muciniphila_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_inferred_DFE.txt')
a_finegoldii_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_inferred_DFE.txt')
a_onderdonkii_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_inferred_DFE.txt')
a_putredinis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_putredinis_61533/core_inferred_DFE.txt')
a_shahii_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_shahii_62199/core_inferred_DFE.txt')
alistipes_sp_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_sp_60764/core_inferred_DFE.txt')
b_bacterium_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_inferred_DFE.txt')
b_caccae_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_caccae_53434/core_inferred_DFE.txt')
b_cellulosilyticus_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_inferred_DFE.txt')
b_coprocola_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_inferred_DFE.txt')
b_eggerthii_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_inferred_DFE.txt')
b_fragilis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_inferred_DFE.txt')
b_massiliensis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_inferred_DFE.txt')
b_ovatus_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_inferred_DFE.txt')
b_plebeius_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_inferred_DFE.txt')
b_stercoris_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_inferred_DFE.txt')
b_thetaiotaomicron_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_inferred_DFE.txt')
b_uniformis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_inferred_DFE.txt')
b_vulgatus_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_inferred_DFE.txt')
b_xylanisolvens_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_inferred_DFE.txt')
b_intestinihominis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_inferred_DFE.txt')
coprococcus_sp_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Coprococcus_sp_62244/core_inferred_DFE.txt')
d_invisus_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Dialister_invisus_61905/core_inferred_DFE.txt')
e_eligens_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_eligens_61678/core_inferred_DFE.txt')
e_rectale_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_inferred_DFE.txt')
e_siraeum_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_inferred_DFE.txt')
f_prausnitzii_57453_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_inferred_DFE.txt')
f_prausnitzii_61481_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_inferred_DFE.txt')
f_prausnitzii_62201_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_inferred_DFE.txt')
l_bacterium_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_inferred_DFE.txt')
o_splanchnicus_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_inferred_DFE.txt')
oscillibacter_sp_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Oscillibacter_sp_60799/core_inferred_DFE.txt')
p_distasonis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_inferred_DFE.txt')
p_merdae_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_inferred_DFE.txt')
phascolarctobacterium_sp_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_inferred_DFE.txt')
p_copri_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Prevotella_copri_61740/core_inferred_DFE.txt')
r_intestinalis_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_inferred_DFE.txt')
r_inulinivorans_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_inferred_DFE.txt')
r_bicirculans_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_inferred_DFE.txt')
r_bromii_FD_nonsyn = empirical_sfs_from_dfe('../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_inferred_DFE.txt')

# Gamma

a_muciniphila_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_inferred_DFE.txt')
a_finegoldii_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_inferred_DFE.txt')
a_onderdonkii_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_inferred_DFE.txt')
a_putredinis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_putredinis_61533/core_inferred_DFE.txt')
a_shahii_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_shahii_62199/core_inferred_DFE.txt')
alistipes_sp_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_sp_60764/core_inferred_DFE.txt')
b_bacterium_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_inferred_DFE.txt')
b_caccae_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_caccae_53434/core_inferred_DFE.txt')
b_cellulosilyticus_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_inferred_DFE.txt')
b_coprocola_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_inferred_DFE.txt')
b_eggerthii_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_inferred_DFE.txt')
b_fragilis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_inferred_DFE.txt')
b_massiliensis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_inferred_DFE.txt')
b_ovatus_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_inferred_DFE.txt')
b_plebeius_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_inferred_DFE.txt')
b_stercoris_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_inferred_DFE.txt')
b_thetaiotaomicron_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_inferred_DFE.txt')
b_uniformis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_inferred_DFE.txt')
b_vulgatus_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_inferred_DFE.txt')
b_xylanisolvens_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_inferred_DFE.txt')
b_intestinihominis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_inferred_DFE.txt')
coprococcus_sp_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Coprococcus_sp_62244/core_inferred_DFE.txt')
d_invisus_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Dialister_invisus_61905/core_inferred_DFE.txt')
e_eligens_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_eligens_61678/core_inferred_DFE.txt')
e_rectale_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_inferred_DFE.txt')
e_siraeum_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_inferred_DFE.txt')
f_prausnitzii_57453_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_inferred_DFE.txt')
f_prausnitzii_61481_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_inferred_DFE.txt')
f_prausnitzii_62201_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_inferred_DFE.txt')
l_bacterium_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_inferred_DFE.txt')
o_splanchnicus_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_inferred_DFE.txt')
oscillibacter_sp_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Oscillibacter_sp_60799/core_inferred_DFE.txt')
p_distasonis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_inferred_DFE.txt')
p_merdae_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_inferred_DFE.txt')
phascolarctobacterium_sp_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_inferred_DFE.txt')
p_copri_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Prevotella_copri_61740/core_inferred_DFE.txt')
r_intestinalis_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_inferred_DFE.txt')
r_inulinivorans_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_inferred_DFE.txt')
r_bicirculans_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_inferred_DFE.txt')
r_bromii_FD_gamma = gamma_sfs_from_dfe('../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_inferred_DFE.txt')

# Neugamma

a_muciniphila_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_inferred_DFE.txt')
a_finegoldii_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_inferred_DFE.txt')
a_onderdonkii_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_inferred_DFE.txt')
a_putredinis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_putredinis_61533/core_inferred_DFE.txt')
a_shahii_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_shahii_62199/core_inferred_DFE.txt')
alistipes_sp_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Alistipes_sp_60764/core_inferred_DFE.txt')
b_bacterium_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_inferred_DFE.txt')
b_caccae_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_caccae_53434/core_inferred_DFE.txt')
b_cellulosilyticus_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_inferred_DFE.txt')
b_coprocola_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_inferred_DFE.txt')
b_eggerthii_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_inferred_DFE.txt')
b_fragilis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_inferred_DFE.txt')
b_massiliensis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_inferred_DFE.txt')
b_ovatus_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_inferred_DFE.txt')
b_plebeius_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_inferred_DFE.txt')
b_stercoris_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_inferred_DFE.txt')
b_thetaiotaomicron_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_inferred_DFE.txt')
b_uniformis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_inferred_DFE.txt')
b_vulgatus_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_inferred_DFE.txt')
b_xylanisolvens_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_inferred_DFE.txt')
b_intestinihominis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_inferred_DFE.txt')
coprococcus_sp_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Coprococcus_sp_62244/core_inferred_DFE.txt')
d_invisus_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Dialister_invisus_61905/core_inferred_DFE.txt')
e_eligens_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_eligens_61678/core_inferred_DFE.txt')
e_rectale_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_inferred_DFE.txt')
e_siraeum_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_inferred_DFE.txt')
f_prausnitzii_57453_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_inferred_DFE.txt')
f_prausnitzii_61481_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_inferred_DFE.txt')
f_prausnitzii_62201_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_inferred_DFE.txt')
l_bacterium_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_inferred_DFE.txt')
o_splanchnicus_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_inferred_DFE.txt')
oscillibacter_sp_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Oscillibacter_sp_60799/core_inferred_DFE.txt')
p_distasonis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_inferred_DFE.txt')
p_merdae_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_inferred_DFE.txt')
phascolarctobacterium_sp_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_inferred_DFE.txt')
p_copri_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Prevotella_copri_61740/core_inferred_DFE.txt')
r_intestinalis_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_inferred_DFE.txt')
r_inulinivorans_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_inferred_DFE.txt')
r_bicirculans_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_inferred_DFE.txt')
r_bromii_FD_neugamma = neugamma_sfs_from_dfe('../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_inferred_DFE.txt')


# SFS comparison
compare_sfs(proportional_sfs(a_muciniphila_FD_folded), 
  proportional_sfs(a_muciniphila_FD_one_epoch), 
  proportional_sfs(a_muciniphila_FD_two_epoch), 
  proportional_sfs(a_muciniphila_FD_three_epoch)) +
  ggtitle('Akkermansia muciniphila (FD) model fit')

compare_sfs(proportional_sfs(a_finegoldii_FD_folded), 
  proportional_sfs(a_finegoldii_FD_one_epoch), 
  proportional_sfs(a_finegoldii_FD_two_epoch), 
  proportional_sfs(a_finegoldii_FD_three_epoch)) +
  ggtitle('Alistipes finegoldii (FD) model fit')

compare_sfs(proportional_sfs(a_onderdonkii_FD_folded), 
  proportional_sfs(a_onderdonkii_FD_one_epoch), 
  proportional_sfs(a_onderdonkii_FD_two_epoch), 
  proportional_sfs(a_onderdonkii_FD_three_epoch)) +
  ggtitle('Alistipes onderdonkii (FD) model fit')

compare_sfs(proportional_sfs(a_putredinis_FD_folded), 
  proportional_sfs(a_putredinis_FD_one_epoch), 
  proportional_sfs(a_putredinis_FD_two_epoch), 
  proportional_sfs(a_putredinis_FD_three_epoch)) +
  ggtitle('Alistipes putredinis (FD) model fit')

compare_sfs(proportional_sfs(a_shahii_FD_folded), 
  proportional_sfs(a_shahii_FD_one_epoch), 
  proportional_sfs(a_shahii_FD_two_epoch), 
  proportional_sfs(a_shahii_FD_three_epoch)) +
  ggtitle('Alistipes shahii (FD) model fit')

compare_sfs(proportional_sfs(alistipes_sp_FD_folded), 
  proportional_sfs(alistipes_sp_FD_one_epoch), 
  proportional_sfs(alistipes_sp_FD_two_epoch), 
  proportional_sfs(alistipes_sp_FD_three_epoch)) +
  ggtitle('Alistipes sp. (FD) model fit')

compare_sfs(proportional_sfs(b_bacterium_FD_folded), 
  proportional_sfs(b_bacterium_FD_one_epoch), 
  proportional_sfs(b_bacterium_FD_two_epoch), 
  proportional_sfs(b_bacterium_FD_three_epoch)) +
  ggtitle('Bacteroidales bacterium (FD) model fit')

compare_sfs(proportional_sfs(b_caccae_FD_folded), 
  proportional_sfs(b_caccae_FD_one_epoch), 
  proportional_sfs(b_caccae_FD_two_epoch), 
  proportional_sfs(b_caccae_FD_three_epoch)) +
  ggtitle('Bacteroides caccae (FD) model fit')

compare_sfs(proportional_sfs(b_cellulosilyticus_FD_folded), 
  proportional_sfs(b_cellulosilyticus_FD_one_epoch), 
  proportional_sfs(b_cellulosilyticus_FD_two_epoch), 
  proportional_sfs(b_cellulosilyticus_FD_three_epoch)) +
  ggtitle('Bacteroides cellulosilyticus (FD) model fit')

compare_sfs(proportional_sfs(b_coprocola_FD_folded), 
  proportional_sfs(b_coprocola_FD_one_epoch), 
  proportional_sfs(b_coprocola_FD_two_epoch), 
  proportional_sfs(b_coprocola_FD_three_epoch)) +
  ggtitle('Bacteroides coprocola (FD) model fit')

compare_sfs(proportional_sfs(b_eggerthii_FD_folded), 
  proportional_sfs(b_eggerthii_FD_one_epoch), 
  proportional_sfs(b_eggerthii_FD_two_epoch), 
  proportional_sfs(b_eggerthii_FD_three_epoch)) +
  ggtitle('Bacteroides eggerthii (FD) model fit')

compare_sfs(proportional_sfs(b_fragilis_FD_folded), 
  proportional_sfs(b_fragilis_FD_one_epoch), 
  proportional_sfs(b_fragilis_FD_two_epoch), 
  proportional_sfs(b_fragilis_FD_three_epoch)) +
  ggtitle('Bacteroides fragilis (FD) model fit')

compare_sfs(proportional_sfs(b_massiliensis_FD_folded), 
  proportional_sfs(b_massiliensis_FD_one_epoch), 
  proportional_sfs(b_massiliensis_FD_two_epoch), 
  proportional_sfs(b_massiliensis_FD_three_epoch)) +
  ggtitle('Bacteroides massiliensis (FD) model fit')

compare_sfs(proportional_sfs(b_ovatus_FD_folded), 
  proportional_sfs(b_ovatus_FD_one_epoch), 
  proportional_sfs(b_ovatus_FD_two_epoch), 
  proportional_sfs(b_ovatus_FD_three_epoch)) +
  ggtitle('Bacteroides ovatus (FD) model fit')

compare_sfs(proportional_sfs(b_plebeius_FD_folded), 
  proportional_sfs(b_plebeius_FD_one_epoch), 
  proportional_sfs(b_plebeius_FD_two_epoch), 
  proportional_sfs(b_plebeius_FD_three_epoch)) +
  ggtitle('Bacteroides plebeius (FD) model fit')

compare_sfs(proportional_sfs(b_stercoris_FD_folded), 
  proportional_sfs(b_stercoris_FD_one_epoch), 
  proportional_sfs(b_stercoris_FD_two_epoch), 
  proportional_sfs(b_stercoris_FD_three_epoch)) +
  ggtitle('Bacteroides stercoris (FD) model fit')

compare_sfs(proportional_sfs(b_thetaiotaomicron_FD_folded), 
  proportional_sfs(b_thetaiotaomicron_FD_one_epoch), 
  proportional_sfs(b_thetaiotaomicron_FD_two_epoch), 
  proportional_sfs(b_thetaiotaomicron_FD_three_epoch)) +
  ggtitle('Bacteroides thetaiotaomicron (FD) model fit')

compare_sfs(proportional_sfs(b_uniformis_FD_folded), 
  proportional_sfs(b_uniformis_FD_one_epoch), 
  proportional_sfs(b_uniformis_FD_two_epoch), 
  proportional_sfs(b_uniformis_FD_three_epoch)) +
  ggtitle('Bacteroides uniformis (FD) model fit')

compare_sfs(proportional_sfs(b_vulgatus_FD_folded), 
  proportional_sfs(b_vulgatus_FD_one_epoch), 
  proportional_sfs(b_vulgatus_FD_two_epoch), 
  proportional_sfs(b_vulgatus_FD_three_epoch)) +
  ggtitle('Bacteroides vulgatus (FD) model fit')

compare_sfs(proportional_sfs(b_xylanisolvens_FD_folded), 
  proportional_sfs(b_xylanisolvens_FD_one_epoch), 
  proportional_sfs(b_xylanisolvens_FD_two_epoch), 
  proportional_sfs(b_xylanisolvens_FD_three_epoch)) +
  ggtitle('Bacteroides xylanisolvens (FD) model fit')

compare_sfs(proportional_sfs(b_intestinihominis_FD_folded), 
  proportional_sfs(b_intestinihominis_FD_one_epoch), 
  proportional_sfs(b_intestinihominis_FD_two_epoch), 
  proportional_sfs(b_intestinihominis_FD_three_epoch)) +
  ggtitle('Barnesiella intestinihominis (FD) model fit')

compare_sfs(proportional_sfs(coprococcus_sp_FD_folded), 
  proportional_sfs(coprococcus_sp_FD_one_epoch), 
  proportional_sfs(coprococcus_sp_FD_two_epoch), 
  proportional_sfs(coprococcus_sp_FD_three_epoch)) +
  ggtitle('Coprococcus sp. (FD) model fit')

compare_sfs(proportional_sfs(d_invisus_FD_folded), 
  proportional_sfs(d_invisus_FD_one_epoch), 
  proportional_sfs(d_invisus_FD_two_epoch), 
  proportional_sfs(d_invisus_FD_three_epoch)) +
  ggtitle('Dialister invisus (FD) model fit')

compare_sfs(proportional_sfs(e_eligens_FD_folded), 
  proportional_sfs(e_eligens_FD_one_epoch), 
  proportional_sfs(e_eligens_FD_two_epoch), 
  proportional_sfs(e_eligens_FD_three_epoch)) +
  ggtitle('Eubacterium eligens (FD) model fit')

compare_sfs(proportional_sfs(e_rectale_FD_folded), 
  proportional_sfs(e_rectale_FD_one_epoch), 
  proportional_sfs(e_rectale_FD_two_epoch), 
  proportional_sfs(e_rectale_FD_three_epoch)) +
  ggtitle('Eubacterium rectale (FD) model fit')

compare_sfs(proportional_sfs(e_siraeum_FD_folded), 
  proportional_sfs(e_siraeum_FD_one_epoch), 
  proportional_sfs(e_siraeum_FD_two_epoch), 
  proportional_sfs(e_siraeum_FD_three_epoch)) +
  ggtitle('Eubacterium siraeum (FD) model fit')

compare_sfs(proportional_sfs(f_prausnitzii_57453_FD_folded), 
  proportional_sfs(f_prausnitzii_57453_FD_one_epoch), 
  proportional_sfs(f_prausnitzii_57453_FD_two_epoch), 
  proportional_sfs(f_prausnitzii_57453_FD_three_epoch)) +
  ggtitle('Faecalibacterium prausnitzii 57453 (FD) model fit')

compare_sfs(proportional_sfs(f_prausnitzii_61481_FD_folded), 
  proportional_sfs(f_prausnitzii_61481_FD_one_epoch), 
  proportional_sfs(f_prausnitzii_61481_FD_two_epoch), 
  proportional_sfs(f_prausnitzii_61481_FD_three_epoch)) +
  ggtitle('Faecalibacterium prausnitzii 61481 (FD) model fit')

compare_sfs(proportional_sfs(f_prausnitzii_62201_FD_folded), 
  proportional_sfs(f_prausnitzii_62201_FD_one_epoch), 
  proportional_sfs(f_prausnitzii_62201_FD_two_epoch), 
  proportional_sfs(f_prausnitzii_62201_FD_three_epoch)) +
  ggtitle('Faecalibacterium prausnitzii 62201 (FD) model fit')

compare_sfs(proportional_sfs(l_bacterium_FD_folded), 
  proportional_sfs(l_bacterium_FD_one_epoch), 
  proportional_sfs(l_bacterium_FD_two_epoch), 
  proportional_sfs(l_bacterium_FD_three_epoch)) +
  ggtitle('Lachnospiraceae bacterium (FD) model fit')

compare_sfs(proportional_sfs(o_splanchnicus_FD_folded), 
  proportional_sfs(o_splanchnicus_FD_one_epoch), 
  proportional_sfs(o_splanchnicus_FD_two_epoch), 
  proportional_sfs(o_splanchnicus_FD_three_epoch)) +
  ggtitle('Odoribacter splanchnicus (FD) model fit')

compare_sfs(proportional_sfs(oscillibacter_sp_FD_folded), 
  proportional_sfs(oscillibacter_sp_FD_one_epoch), 
  proportional_sfs(oscillibacter_sp_FD_two_epoch), 
  proportional_sfs(oscillibacter_sp_FD_three_epoch)) +
  ggtitle('Oscillibacter sp. (FD) model fit')

compare_sfs(proportional_sfs(p_distasonis_FD_folded), 
  proportional_sfs(p_distasonis_FD_one_epoch), 
  proportional_sfs(p_distasonis_FD_two_epoch), 
  proportional_sfs(p_distasonis_FD_three_epoch)) +
  ggtitle('Parabacteroides distasonis (FD) model fit')

compare_sfs(proportional_sfs(p_merdae_FD_folded), 
  proportional_sfs(p_merdae_FD_one_epoch), 
  proportional_sfs(p_merdae_FD_two_epoch), 
  proportional_sfs(p_merdae_FD_three_epoch)) +
  ggtitle('Parabacteroides merdae (FD) model fit')

compare_sfs(proportional_sfs(phascolarctobacterium_sp_FD_folded), 
  proportional_sfs(phascolarctobacterium_sp_FD_one_epoch), 
  proportional_sfs(phascolarctobacterium_sp_FD_two_epoch), 
  proportional_sfs(phascolarctobacterium_sp_FD_three_epoch)) +
  ggtitle('Phascolarctobacterium sp. (FD) model fit')

compare_sfs(proportional_sfs(p_copri_FD_folded), 
  proportional_sfs(p_copri_FD_one_epoch), 
  proportional_sfs(p_copri_FD_two_epoch), 
  proportional_sfs(p_copri_FD_three_epoch)) +
  ggtitle('Prevotella copri (FD) model fit')

compare_sfs(proportional_sfs(r_intestinalis_FD_folded), 
  proportional_sfs(r_intestinalis_FD_one_epoch), 
  proportional_sfs(r_intestinalis_FD_two_epoch), 
  proportional_sfs(r_intestinalis_FD_three_epoch)) +
  ggtitle('Roseburia intestinalis (FD) model fit')

compare_sfs(proportional_sfs(r_inulinivorans_FD_folded), 
  proportional_sfs(r_inulinivorans_FD_one_epoch), 
  proportional_sfs(r_inulinivorans_FD_two_epoch), 
  proportional_sfs(r_inulinivorans_FD_three_epoch)) +
  ggtitle('Roseburia inulinivorans (FD) model fit')

compare_sfs(proportional_sfs(r_bicirculans_FD_folded), 
  proportional_sfs(r_bicirculans_FD_one_epoch), 
  proportional_sfs(r_bicirculans_FD_two_epoch), 
  proportional_sfs(r_bicirculans_FD_three_epoch)) +
  ggtitle('Ruminococcus bicirculans (FD) model fit')

compare_sfs(proportional_sfs(r_bromii_FD_folded), 
  proportional_sfs(r_bromii_FD_one_epoch), 
  proportional_sfs(r_bromii_FD_two_epoch), 
  proportional_sfs(r_bromii_FD_three_epoch)) +
  ggtitle('Ruminococcus bromii (FD) model fit')

a_muciniphila_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/likelihood_surface.csv')
a_muciniphila_FD_likelihood_surface + ggtitle('Akkermansia muciniphila (FD) likelihood surface')

a_finegoldii_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_finegoldii_56071/likelihood_surface.csv')
a_finegoldii_FD_likelihood_surface + ggtitle('Alistipes finegoldi (FD) likelihood surface')

a_onderdonkii_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/likelihood_surface.csv')
a_onderdonkii_FD_likelihood_surface + ggtitle('Alistipes onderdonkii (FD) likelihood surface')

a_putredinis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_putredinis_61533/likelihood_surface.csv')
a_putredinis_FD_likelihood_surface + ggtitle('Alistipes putredinis (FD) likelihood surface')

a_shahii_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_shahii_62199/likelihood_surface.csv')
a_shahii_FD_likelihood_surface + ggtitle('Alistipes shahii (FD) likelihood surface')

alistipes_sp_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_sp_60764/likelihood_surface.csv')
alistipes_sp_FD_likelihood_surface + ggtitle('Alistipes sp. (FD) likelihood surface')

b_bacterium_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/likelihood_surface.csv')
b_bacterium_FD_likelihood_surface + ggtitle('Bacteroides bacterium (FD) likelihood surface')

b_caccae_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_caccae_53434/likelihood_surface.csv')
b_caccae_FD_likelihood_surface + ggtitle('Bacteroides caccae (FD) likelihood surface')

b_cellulosilyticus_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/likelihood_surface.csv')
b_cellulosilyticus_FD_likelihood_surface + ggtitle('Bacteroides cellulosilyticus (FD) likelihood surface')

b_coprocola_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_coprocola_61586/likelihood_surface.csv')
b_coprocola_FD_likelihood_surface + ggtitle('Bacteroides coprocola (FD) likelihood surface')

b_eggerthii_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/likelihood_surface.csv')
b_eggerthii_FD_likelihood_surface + ggtitle('Bacteroides eggerthii (FD) likelihood surface')

b_fragilis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_fragilis_54507/likelihood_surface.csv')
b_fragilis_FD_likelihood_surface + ggtitle('Bacteroides fragilis (FD) likelihood surface')

b_massiliensis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/likelihood_surface.csv')
b_massiliensis_FD_likelihood_surface + ggtitle('Bacteroides massiliensis (FD) likelihood surface')

b_ovatus_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_ovatus_58035/likelihood_surface.csv')
b_ovatus_FD_likelihood_surface + ggtitle('Bacteroides ovatus (FD) likelihood surface')

b_plebeius_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_plebeius_61623/likelihood_surface.csv')
b_plebeius_FD_likelihood_surface + ggtitle('Bacteroides plebeius (FD) likelihood surface')

b_stercoris_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_stercoris_56735/likelihood_surface.csv')
b_stercoris_FD_likelihood_surface + ggtitle('Bacteroides stercoris (FD) likelihood surface')

b_thetaiotaomicron_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/likelihood_surface.csv')
b_thetaiotaomicron_FD_likelihood_surface + ggtitle('Bacteroides thetaiotaomicron (FD) likelihood surface')

b_uniformis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_uniformis_57318/likelihood_surface.csv')
b_uniformis_FD_likelihood_surface + ggtitle('Bacteroides uniformis (FD) likelihood surface')

b_vulgatus_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/likelihood_surface.csv')
b_vulgatus_FD_likelihood_surface + ggtitle('Bacteroides vulgatus (FD) likelihood surface')

b_xylanisolvens_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/likelihood_surface.csv')
b_xylanisolvens_FD_likelihood_surface + ggtitle('Bacteroides xylanisolvens (FD) likelihood surface')

b_intestinihominis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/likelihood_surface.csv')
b_intestinihominis_FD_likelihood_surface + ggtitle('Barnesiella intestinihominis (FD) likelihood surface')

coprococcus_sp_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Coprococcus_sp_62244/likelihood_surface.csv')
coprococcus_sp_FD_likelihood_surface + ggtitle('Coprococcus sp. (FD) likelihood surface')

d_invisus_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Dialister_invisus_61905/likelihood_surface.csv')
d_invisus_FD_likelihood_surface + ggtitle('Dialister invisus (FD) likelihood surface')

e_eligens_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Eubacterium_eligens_61678/likelihood_surface.csv')
e_eligens_FD_likelihood_surface + ggtitle('Eubacterium eligens (FD) likelihood surface')

e_rectale_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Eubacterium_rectale_56927/likelihood_surface.csv')
e_rectale_FD_likelihood_surface + ggtitle('Eubacterium rectale (FD) likelihood surface')

e_siraeum_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Eubacterium_siraeum_57634/likelihood_surface.csv')
e_siraeum_FD_likelihood_surface + ggtitle('Eubacterium siraeum (FD) likelihood surface')

f_prausnitzii_57453_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/likelihood_surface.csv')
f_prausnitzii_57453_FD_likelihood_surface + ggtitle('Faecalibacterium prausnitzii 57453 (FD) likelihood surface')

f_prausnitzii_61481_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/likelihood_surface.csv')
f_prausnitzii_61481_FD_likelihood_surface + ggtitle('Faecalibacterium prausnitzii 61481 (FD) likelihood surface')

f_prausnitzii_62201_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/likelihood_surface.csv')
f_prausnitzii_62201_FD_likelihood_surface + ggtitle('Faecalibacterium prausnitzii 62201 (FD) likelihood surface')

l_bacterium_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/likelihood_surface.csv')
l_bacterium_FD_likelihood_surface + ggtitle('Lachnospiraceae bacterium (FD) likelihood surface')

o_splanchnicus_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/likelihood_surface.csv')
o_splanchnicus_FD_likelihood_surface + ggtitle('Odoribacter splanchnicus (FD) likelihood surface')

oscillibacter_sp_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Oscillibacter_sp_60799/likelihood_surface.csv')
oscillibacter_sp_FD_likelihood_surface + ggtitle('Oscillibacter sp. (FD) likelihood surface')

p_distasonis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/likelihood_surface.csv')
p_distasonis_FD_likelihood_surface + ggtitle('Parabacteroides distasonis (FD) likelihood surface')

p_merdae_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Parabacteroides_merdae_56972/likelihood_surface.csv')
p_merdae_FD_likelihood_surface + ggtitle('Parabacteroides merdae (FD) likelihood surface')

phascolarctobacterium_sp_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/likelihood_surface.csv')
phascolarctobacterium_sp_FD_likelihood_surface + ggtitle('Phascolarctobacterium sp. (FD) likelihood surface')

p_copri_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Prevotella_copri_61740/likelihood_surface.csv')
p_copri_FD_likelihood_surface + ggtitle('Prevotella copri (FD) likelihood surface')

r_intestinalis_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Roseburia_intestinalis_56239/likelihood_surface.csv')
r_intestinalis_FD_likelihood_surface + ggtitle('Roseburia intestinalis (FD) likelihood surface')

r_inulinivorans_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/likelihood_surface.csv')
r_inulinivorans_FD_likelihood_surface + ggtitle('Roseburia inulinovrans (FD) likelihood surface')

r_bicirculans_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/likelihood_surface.csv')
r_bicirculans_FD_likelihood_surface + ggtitle('Ruminococcus bicirculans (FD) likelihood surface')

r_bromii_FD_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Ruminococcus_bromii_62047/likelihood_surface.csv')
r_bromii_FD_likelihood_surface + ggtitle('Ruminococcus bromii (FD) likelihood surface')

## Accessory genes

a_muciniphila_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/accessory_likelihood_surface.csv')
a_muciniphila_FD_accessory_likelihood_surface + ggtitle('Akkermansia muciniphila (FD, accessory genome) likelihood surface')

a_finegoldii_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_finegoldii_56071/accessory_likelihood_surface.csv')
a_finegoldii_FD_accessory_likelihood_surface + ggtitle('Alistipes finegoldi (FD, accessory genome) likelihood surface')

a_onderdonkii_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/accessory_likelihood_surface.csv')
a_onderdonkii_FD_accessory_likelihood_surface + ggtitle('Alistipes onderdonkii (FD, accessory genome) likelihood surface')

a_putredinis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_putredinis_61533/accessory_likelihood_surface.csv')
a_putredinis_FD_accessory_likelihood_surface + ggtitle('Alistipes putredinis (FD, accessory genome) likelihood surface')

a_shahii_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_shahii_62199/accessory_likelihood_surface.csv')
a_shahii_FD_accessory_likelihood_surface + ggtitle('Alistipes shahii (FD, accessory genome) likelihood surface')

alistipes_sp_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Alistipes_sp_60764/accessory_likelihood_surface.csv')
alistipes_sp_FD_accessory_likelihood_surface + ggtitle('Alistipes sp. (FD, accessory genome) likelihood surface')

b_bacterium_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/accessory_likelihood_surface.csv')
b_bacterium_FD_accessory_likelihood_surface + ggtitle('Bacteroides bacterium (FD, accessory genome) likelihood surface')

b_caccae_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_caccae_53434/accessory_likelihood_surface.csv')
b_caccae_FD_accessory_likelihood_surface + ggtitle('Bacteroides caccae (FD, accessory genome) likelihood surface')

b_cellulosilyticus_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/accessory_likelihood_surface.csv')
b_cellulosilyticus_FD_accessory_likelihood_surface + ggtitle('Bacteroides cellulosilyticus (FD, accessory genome) likelihood surface')

b_coprocola_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_coprocola_61586/accessory_likelihood_surface.csv')
b_coprocola_FD_accessory_likelihood_surface + ggtitle('Bacteroides coprocola (FD, accessory genome) likelihood surface')

b_eggerthii_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/accessory_likelihood_surface.csv')
b_eggerthii_FD_accessory_likelihood_surface + ggtitle('Bacteroides eggerthii (FD, accessory genome) likelihood surface')

b_fragilis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_fragilis_54507/accessory_likelihood_surface.csv')
b_fragilis_FD_accessory_likelihood_surface + ggtitle('Bacteroides fragilis (FD, accessory genome) likelihood surface')

b_massiliensis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/accessory_likelihood_surface.csv')
b_massiliensis_FD_accessory_likelihood_surface + ggtitle('Bacteroides massiliensis (FD, accessory genome) likelihood surface')

b_ovatus_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_ovatus_58035/accessory_likelihood_surface.csv')
b_ovatus_FD_accessory_likelihood_surface + ggtitle('Bacteroides ovatus (FD, accessory genome) likelihood surface')

b_plebeius_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_plebeius_61623/accessory_likelihood_surface.csv')
b_plebeius_FD_accessory_likelihood_surface + ggtitle('Bacteroides plebeius (FD, accessory genome) likelihood surface')

b_stercoris_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_stercoris_56735/accessory_likelihood_surface.csv')
b_stercoris_FD_accessory_likelihood_surface + ggtitle('Bacteroides stercoris (FD, accessory genome) likelihood surface')

b_thetaiotaomicron_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/accessory_likelihood_surface.csv')
b_thetaiotaomicron_FD_accessory_likelihood_surface + ggtitle('Bacteroides thetaiotaomicron (FD, accessory genome) likelihood surface')

b_uniformis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_uniformis_57318/accessory_likelihood_surface.csv')
b_uniformis_FD_accessory_likelihood_surface + ggtitle('Bacteroides uniformis (FD, accessory genome) likelihood surface')

b_vulgatus_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/accessory_likelihood_surface.csv')
b_vulgatus_FD_accessory_likelihood_surface + ggtitle('Bacteroides vulgatus (FD, accessory genome) likelihood surface')

b_xylanisolvens_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/accessory_likelihood_surface.csv')
b_xylanisolvens_FD_accessory_likelihood_surface + ggtitle('Bacteroides xylanisolvens (FD, accessory genome) likelihood surface')

b_intestinihominis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/accessory_likelihood_surface.csv')
b_intestinihominis_FD_accessory_likelihood_surface + ggtitle('Barnesiella intestinihominis (FD, accessory genome) likelihood surface')

coprococcus_sp_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Coprococcus_sp_62244/accessory_likelihood_surface.csv')
coprococcus_sp_FD_accessory_likelihood_surface + ggtitle('Coprococcus sp. (FD, accessory genome) likelihood surface')

d_invisus_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Dialister_invisus_61905/accessory_likelihood_surface.csv')
d_invisus_FD_accessory_likelihood_surface + ggtitle('Dialister invisus (FD, accessory genome) likelihood surface')

e_eligens_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Eubacterium_eligens_61678/accessory_likelihood_surface.csv')
e_eligens_FD_accessory_likelihood_surface + ggtitle('Eubacterium eligens (FD, accessory genome) likelihood surface')

e_rectale_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Eubacterium_rectale_56927/accessory_likelihood_surface.csv')
e_rectale_FD_accessory_likelihood_surface + ggtitle('Eubacterium rectale (FD, accessory genome) likelihood surface')

e_siraeum_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Eubacterium_siraeum_57634/accessory_likelihood_surface.csv')
e_siraeum_FD_accessory_likelihood_surface + ggtitle('Eubacterium siraeum (FD, accessory genome) likelihood surface')

f_prausnitzii_57453_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/accessory_likelihood_surface.csv')
f_prausnitzii_57453_FD_accessory_likelihood_surface + ggtitle('Faecalibacterium prausnitzii 57453 (FD, accessory genome) likelihood surface')

f_prausnitzii_61481_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/accessory_likelihood_surface.csv')
f_prausnitzii_61481_FD_accessory_likelihood_surface + ggtitle('Faecalibacterium prausnitzii 61481 (FD, accessory genome) likelihood surface')

f_prausnitzii_62201_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/accessory_likelihood_surface.csv')
f_prausnitzii_62201_FD_accessory_likelihood_surface + ggtitle('Faecalibacterium prausnitzii 62201 (FD, accessory genome) likelihood surface')

l_bacterium_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/accessory_likelihood_surface.csv')
l_bacterium_FD_accessory_likelihood_surface + ggtitle('Lachnospiraceae bacterium (FD, accessory genome) likelihood surface')

o_splanchnicus_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/accessory_likelihood_surface.csv')
o_splanchnicus_FD_accessory_likelihood_surface + ggtitle('Odoribacter splanchnicus (FD, accessory genome) likelihood surface')

oscillibacter_sp_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Oscillibacter_sp_60799/accessory_likelihood_surface.csv')
oscillibacter_sp_FD_accessory_likelihood_surface + ggtitle('Oscillibacter sp. (FD, accessory genome) likelihood surface')

p_distasonis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/accessory_likelihood_surface.csv')
p_distasonis_FD_accessory_likelihood_surface + ggtitle('Parabacteroides distasonis (FD, accessory genome) likelihood surface')

p_merdae_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Parabacteroides_merdae_56972/accessory_likelihood_surface.csv')
p_merdae_FD_accessory_likelihood_surface + ggtitle('Parabacteroides merdae (FD, accessory genome) likelihood surface')

phascolarctobacterium_sp_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/accessory_likelihood_surface.csv')
phascolarctobacterium_sp_FD_accessory_likelihood_surface + ggtitle('Phascolarctobacterium sp. (FD, accessory genome) likelihood surface')

p_copri_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Prevotella_copri_61740/accessory_likelihood_surface.csv')
p_copri_FD_accessory_likelihood_surface + ggtitle('Prevotella copri (FD, accessory genome) likelihood surface')

r_intestinalis_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Roseburia_intestinalis_56239/accessory_likelihood_surface.csv')
r_intestinalis_FD_accessory_likelihood_surface + ggtitle('Roseburia intestinalis (FD, accessory genome) likelihood surface')

r_inulinivorans_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/accessory_likelihood_surface.csv')
r_inulinivorans_FD_accessory_likelihood_surface + ggtitle('Roseburia inulinovrans (FD, accessory genome) likelihood surface')

r_bicirculans_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/accessory_likelihood_surface.csv')
r_bicirculans_FD_accessory_likelihood_surface + ggtitle('Ruminococcus bicirculans (FD, accessory genome) likelihood surface')

r_bromii_FD_accessory_likelihood_surface = plot_likelihood_surface_contour('../SupplementaryAnalysis/Ruminococcus_bromii_62047/accessory_likelihood_surface.csv')
r_bromii_FD_accessory_likelihood_surface + ggtitle('Ruminococcus bromii (FD, accessory genome) likelihood surface')


# HR DFE Comparison
a_muciniphila_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Akkermansia_muciniphila_55290/core_0.5_inferred_DFE.txt')
a_muciniphila_HR_dfe_params$species = 'Akkermansia muciniphila'

a_finegoldii_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Alistipes_finegoldii_56071/core_0.5_inferred_DFE.txt')
a_finegoldii_HR_dfe_params$species = 'Alistipes finegoldii'

a_onderdonkii_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Alistipes_onderdonkii_55464/core_0.5_inferred_DFE.txt')
a_onderdonkii_HR_dfe_params$species = 'Alistipes onderdonkii'

a_shahii_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Alistipes_shahii_62199/core_0.5_inferred_DFE.txt')
a_shahii_HR_dfe_params$species = 'Alistipes shahii'

b_caccae_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_caccae_53434/core_0.5_inferred_DFE.txt')
b_caccae_HR_dfe_params$species = 'Bacteroides caccae'

b_cellulosilyticus_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_cellulosilyticus_58046/core_0.5_inferred_DFE.txt')
b_cellulosilyticus_HR_dfe_params$species = 'Bacteroides cellulosilyticus'

b_coprocola_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_coprocola_61586/core_0.5_inferred_DFE.txt')
b_coprocola_HR_dfe_params$species = 'Bacteroides coprocola'

b_eggerthii_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_eggerthii_54457/core_0.5_inferred_DFE.txt')
b_eggerthii_HR_dfe_params$species = 'Bacteroides eggerthii'

b_fragilis_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_fragilis_54507/core_0.5_inferred_DFE.txt')
b_fragilis_HR_dfe_params$species = 'Bacteroides fragilis'

b_ovatus_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_ovatus_58035/core_0.5_inferred_DFE.txt')
b_ovatus_HR_dfe_params$species = 'Bacteroides ovatus'

b_stercoris_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_stercoris_56735/core_0.5_inferred_DFE.txt')
b_stercoris_HR_dfe_params$species = 'Bacteroides stercoris'

b_thetaiotaomicron_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_thetaiotaomicron_56941/core_0.5_inferred_DFE.txt')
b_thetaiotaomicron_HR_dfe_params$species = 'Bacteroides thetaiotaomicron'

b_vulgatus_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Bacteroides_vulgatus_57955/core_0.5_inferred_DFE.txt')
b_vulgatus_HR_dfe_params$species = 'Bacteroides vulgatus'

b_intestinihominis_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Barnesiella_intestinihominis_62208/core_0.5_inferred_DFE.txt')
b_intestinihominis_HR_dfe_params$species = 'Barnesiella intestinihominis'

d_invisus_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Dialister_invisus_61905/core_0.5_inferred_DFE.txt')
d_invisus_HR_dfe_params$species = 'Dialiester invisus'

e_rectale_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Eubacterium_rectale_56927/core_0.5_inferred_DFE.txt')
e_rectale_HR_dfe_params$species = 'Eubacterium rectale'

e_siraeum_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Eubacterium_siraeum_57634/core_0.5_inferred_DFE.txt')
e_siraeum_HR_dfe_params$species = 'Eubacterium siraeum'

oscillibacter_sp_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Oscillibacter_sp_60799/core_0.5_inferred_DFE.txt')
oscillibacter_sp_HR_dfe_params$species = 'Oscillibacter sp.'

p_distasonis_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Parabacteroides_distasonis_56985/core_0.5_inferred_DFE.txt')
p_distasonis_HR_dfe_params$species = 'Parabacteroides distasonis'

p_merdae_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Parabacteroides_merdae_56972/core_0.5_inferred_DFE.txt')
p_merdae_HR_dfe_params$species = 'Parabacteroides merdae'

r_bicirculans_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Ruminococcus_bicirculans_59300/core_0.5_inferred_DFE.txt')
r_bicirculans_HR_dfe_params$species = 'Ruminococcus bicirculans'

r_bromii_HR_dfe_params = read_dfe_params('../HighRecombinationAnalysis/Ruminococcus_bromii_62047/core_0.5_inferred_DFE.txt')
r_bromii_HR_dfe_params$species = 'Ruminococcus bromii'

HR_dfe_df = rbind(
  melt(a_muciniphila_HR_dfe_params),
  melt(a_finegoldii_HR_dfe_params),
  melt(a_onderdonkii_HR_dfe_params),
  melt(a_shahii_HR_dfe_params),
  melt(b_caccae_HR_dfe_params),
  melt(b_cellulosilyticus_HR_dfe_params),
  melt(b_coprocola_HR_dfe_params),
  melt(b_eggerthii_HR_dfe_params),
  melt(b_fragilis_HR_dfe_params),
  melt(b_ovatus_HR_dfe_params),
  melt(b_stercoris_HR_dfe_params),
  melt(b_thetaiotaomicron_HR_dfe_params),
  melt(b_vulgatus_HR_dfe_params),
  melt(b_intestinihominis_HR_dfe_params),
  melt(d_invisus_HR_dfe_params),
  melt(e_rectale_HR_dfe_params),
  melt(e_siraeum_HR_dfe_params),
  melt(oscillibacter_sp_HR_dfe_params),
  melt(p_distasonis_HR_dfe_params),
  melt(p_merdae_HR_dfe_params),
  melt(r_bicirculans_HR_dfe_params),
  melt(r_bromii_HR_dfe_params)
)

# dfe_df

## Due to Dadi internal scaling which considers genotype
## fitness as (1, 1 + 2sh, 1 + 2s), and the fact that
## we are working with a haploid sample, we
## wish to multiply the _inferred_ s by 2 to get the true
## selective coefficient of the haploid bacteria.
## However, s is for the heterozygote case.

# dfe_df$value = dfe_df$value * 2
HR_dfe_df$value[HR_dfe_df$value <= 1e-11] = 1e-11
HR_dfe_df$value[HR_dfe_df$value >= 0.5] = 0.5

# DFE Comparison

ggplot(HR_dfe_df[HR_dfe_df$variable == 'gamma_dfe_dist_low', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 1) +
  #labs(
  #  title = 'Gamma-Distributed DFE',
  #  subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  #) +  
  theme_ridges() +
  scale_x_log10(limits=c(1e-12, 1e1)) +
  ylab('Proportion of Sites') +
  theme(axis.text.y = element_text(face='italic')) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  xlab('Selection Coefficient')

# FD DFE comparison

a_muciniphila_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Akkermansia_muciniphila_55290/core_inferred_DFE.txt')
a_muciniphila_FD_dfe_params$species = 'Akkermansia muciniphila'

a_finegoldii_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_inferred_DFE.txt')
a_finegoldii_FD_dfe_params$species = 'Alistipes finegoldii'

a_onderdonkii_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Alistipes_onderdonkii_55464/core_inferred_DFE.txt')
a_onderdonkii_FD_dfe_params$species = 'Alistipes onderdonkii'

a_putredinis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Alistipes_putredinis_61533/core_inferred_DFE.txt')
a_putredinis_FD_dfe_params$species = 'Alistipes putredinis'

a_shahii_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Alistipes_shahii_62199/core_inferred_DFE.txt')
a_shahii_FD_dfe_params$species = 'Alistipes shahii'

alistipes_sp_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Alistipes_sp_60764/core_inferred_DFE.txt')
alistipes_sp_FD_dfe_params$species = 'Alistipes sp.'

b_bacterium_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroidales_bacterium_58650/core_inferred_DFE.txt')
b_bacterium_FD_dfe_params$species = 'Bacteroidales bacterium'

b_caccae_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_caccae_53434/core_inferred_DFE.txt')
b_caccae_FD_dfe_params$species = 'Bacteroides caccae'

b_cellulosilyticus_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_cellulosilyticus_58046/core_inferred_DFE.txt')
b_cellulosilyticus_FD_dfe_params$species = 'Bacteroides cellulosilyticus'

b_coprocola_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_coprocola_61586/core_inferred_DFE.txt')
b_coprocola_FD_dfe_params$species = 'Bacteroides coprocola'

b_eggerthii_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_eggerthii_54457/core_inferred_DFE.txt')
b_eggerthii_FD_dfe_params$species = 'Bacteroides eggerthii'

b_fragilis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_fragilis_54507/core_inferred_DFE.txt')
b_fragilis_FD_dfe_params$species = 'Bacteroides fragilis'

b_massiliensis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_massiliensis_44749/core_inferred_DFE.txt')
b_massiliensis_FD_dfe_params$species = 'Bacteroides massiliensis'

b_ovatus_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_ovatus_58035/core_inferred_DFE.txt')
b_ovatus_FD_dfe_params$species = 'Bacteroides ovatus'

b_plebeius_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_plebeius_61623/core_inferred_DFE.txt')
b_plebeius_FD_dfe_params$species = 'Bacteroides plebeius'

b_stercoris_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_stercoris_56735/core_inferred_DFE.txt')
b_stercoris_FD_dfe_params$species = 'Bacteroides stercoris'

b_thetaiotaomicron_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_thetaiotaomicron_56941/core_inferred_DFE.txt')
b_thetaiotaomicron_FD_dfe_params$species = 'Bacteroides thetaiotaomicron'

b_uniformis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_uniformis_57318/core_inferred_DFE.txt')
b_uniformis_FD_dfe_params$species = 'Bacteroides uniformis'

b_vulgatus_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_vulgatus_57955/core_inferred_DFE.txt')
b_vulgatus_FD_dfe_params$species = 'Bacteroides vulgatus'

b_xylanisolvens_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Bacteroides_xylanisolvens_57185/core_inferred_DFE.txt')
b_xylanisolvens_FD_dfe_params$species = 'Bacteroides xylanisolvens'

b_intestinihominis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Barnesiella_intestinihominis_62208/core_inferred_DFE.txt')
b_intestinihominis_FD_dfe_params$species = 'Barnesiella intestinihominis'

coprococcus_sp_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Coprococcus_sp_62244/core_inferred_DFE.txt')
coprococcus_sp_FD_dfe_params$species = 'Coprococcus sp.'

d_invisus_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Dialister_invisus_61905/core_inferred_DFE.txt')
d_invisus_FD_dfe_params$species = 'Dialiester invisus'

e_eligens_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Eubacterium_eligens_61678/core_inferred_DFE.txt')
e_eligens_FD_dfe_params$species = 'Eubacterium eligens'

e_rectale_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_inferred_DFE.txt')
e_rectale_FD_dfe_params$species = 'Eubacterium rectale'

e_siraeum_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Eubacterium_siraeum_57634/core_inferred_DFE.txt')
e_siraeum_FD_dfe_params$species = 'Eubacterium siraeum'

f_prausnitzii_57453_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_57453/core_inferred_DFE.txt')
f_prausnitzii_57453_FD_dfe_params$species = 'Faecalibacter prausnitzii (57453)'

f_prausnitzii_61481_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_61481/core_inferred_DFE.txt')
f_prausnitzii_61481_FD_dfe_params$species = 'Faecalibacter prausnitzii (61481)'

f_prausnitzii_62201_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Faecalibacterium_prausnitzii_62201/core_inferred_DFE.txt')
f_prausnitzii_62201_FD_dfe_params$species = 'Faecalibacter prausnitzii (62201)'

l_bacterium_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Lachnospiraceae_bacterium_51870/core_inferred_DFE.txt')
l_bacterium_FD_dfe_params$species = 'Lachnospiraceae bacterium'

o_splanchnicus_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Odoribacter_splanchnicus_62174/core_inferred_DFE.txt')
o_splanchnicus_FD_dfe_params$species = 'Odoribacter splanchnicus'

oscillibacter_sp_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Oscillibacter_sp_60799/core_inferred_DFE.txt')
oscillibacter_sp_FD_dfe_params$species = 'Oscillibacter sp.'

p_distasonis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Parabacteroides_distasonis_56985/core_inferred_DFE.txt')
p_distasonis_FD_dfe_params$species = 'Parabacteroides distasonis'

p_merdae_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Parabacteroides_merdae_56972/core_inferred_DFE.txt')
p_merdae_FD_dfe_params$species = 'Parabacteroides merdae'

phascolarctobacterium_sp_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Phascolarctobacterium_sp_59817/core_inferred_DFE.txt')
phascolarctobacterium_sp_FD_dfe_params$species = 'Phascolarctobacterium sp.'

p_copri_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Prevotella_copri_61740/core_inferred_DFE.txt')
p_copri_FD_dfe_params$species = 'Prevotella copri'

r_intestinalis_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Roseburia_intestinalis_56239/core_inferred_DFE.txt')
r_intestinalis_FD_dfe_params$species = 'Roseburia intestinalis'

r_inulinivorans_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Roseburia_inulinivorans_61943/core_inferred_DFE.txt')
r_inulinivorans_FD_dfe_params$species = 'Roseburia inulinivorans'

r_bicirculans_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Ruminococcus_bicirculans_59300/core_inferred_DFE.txt')
r_bicirculans_FD_dfe_params$species = 'Ruminococcus bicirculans'

r_bromii_FD_dfe_params = read_dfe_params('../SupplementaryAnalysis/Ruminococcus_bromii_62047/core_inferred_DFE.txt')
r_bromii_FD_dfe_params$species = 'Ruminococcus bromii'

FD_dfe_df = rbind(
  melt(a_muciniphila_FD_dfe_params),
  melt(a_finegoldii_FD_dfe_params),
  melt(a_onderdonkii_FD_dfe_params),
  melt(a_putredinis_FD_dfe_params),
  melt(a_shahii_FD_dfe_params),
  melt(alistipes_sp_FD_dfe_params),
  melt(b_bacterium_FD_dfe_params),
  melt(b_bacterium_FD_dfe_params),
  melt(b_caccae_FD_dfe_params),
  melt(b_cellulosilyticus_FD_dfe_params),
  melt(b_coprocola_FD_dfe_params),
  melt(b_eggerthii_FD_dfe_params),
  melt(b_fragilis_FD_dfe_params),
  melt(b_massiliensis_FD_dfe_params),
  melt(b_ovatus_FD_dfe_params),
  melt(b_plebeius_FD_dfe_params),
  melt(b_stercoris_FD_dfe_params),
  melt(b_thetaiotaomicron_FD_dfe_params),
  melt(b_uniformis_FD_dfe_params),
  melt(b_vulgatus_FD_dfe_params),
  melt(b_xylanisolvens_FD_dfe_params),
  melt(b_intestinihominis_FD_dfe_params),
  melt(coprococcus_sp_FD_dfe_params),
  melt(d_invisus_FD_dfe_params),
  melt(e_eligens_FD_dfe_params),
  melt(e_rectale_FD_dfe_params),
  melt(e_siraeum_FD_dfe_params),
  melt(f_prausnitzii_57453_FD_dfe_params),
  melt(f_prausnitzii_61481_FD_dfe_params),
  melt(f_prausnitzii_62201_FD_dfe_params),
  melt(l_bacterium_FD_dfe_params),
  melt(o_splanchnicus_FD_dfe_params),
  melt(oscillibacter_sp_FD_dfe_params),
  melt(p_distasonis_FD_dfe_params),
  melt(p_merdae_FD_dfe_params),
  melt(phascolarctobacterium_sp_FD_dfe_params),
  melt(p_copri_FD_dfe_params),
  melt(r_intestinalis_FD_dfe_params),
  melt(r_inulinivorans_FD_dfe_params),
  melt(r_bicirculans_FD_dfe_params),
  melt(r_bromii_FD_dfe_params)
)

# dfe_df

## Due to Dadi internal scaling which considers genotype
## fitness as (1, 1 + 2sh, 1 + 2s), and the fact that
## we are working with a haploid sample, we
## wish to multiply the _inferred_ s by 2 to get the true
## selective coefficient of the haploid bacteria.
## However, s is for the heterozygote case.

# dfe_df$value = dfe_df$value * 2
FD_dfe_df$value[FD_dfe_df$value <= 1e-11] = 1e-11
FD_dfe_df$value[FD_dfe_df$value >= 0.5] = 0.5

# DFE Comparison

ggplot(FD_dfe_df[FD_dfe_df$variable == 'gamma_dfe_dist_low', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 1) +
  #labs(
  #  title = 'Gamma-Distributed DFE',
  #  subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  #) +  
  theme_ridges() +
  scale_x_log10(limits=c(1e-12, 1e1)) +
  ylab('Proportion of Sites') +
  theme(axis.text.y = element_text(face='italic')) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  xlab('Selection Coefficient')

# HR SFS DFE comparison
compare_dfe_sfs(a_muciniphila_HR_nonsyn, 
  a_muciniphila_HR_gamma, 
  a_muciniphila_HR_neugamma) +
  ggtitle('Akkermansia muciniphila (HR) DFE model fit')

compare_dfe_sfs(a_finegoldii_HR_nonsyn, 
  a_finegoldii_HR_gamma, 
  a_finegoldii_HR_neugamma) +
  ggtitle('Alistipes finegoldii (HR) DFE model fit')

compare_dfe_sfs(a_onderdonkii_HR_nonsyn, 
  a_onderdonkii_HR_gamma, 
  a_onderdonkii_HR_neugamma) +
  ggtitle('Alistipes onderdonkii (HR) DFE model fit')

compare_dfe_sfs(a_shahii_HR_nonsyn, 
  a_shahii_HR_gamma, 
  a_shahii_HR_neugamma) +
  ggtitle('Alistipes shahii (HR) DFE model fit')

compare_dfe_sfs(b_caccae_HR_nonsyn, 
  b_caccae_HR_gamma, 
  b_caccae_HR_neugamma) +
  ggtitle('Bacteroides caccae (HR) DFE model fit')

compare_dfe_sfs(b_cellulosilyticus_HR_nonsyn, 
  b_cellulosilyticus_HR_gamma, 
  b_cellulosilyticus_HR_neugamma) +
  ggtitle('Bacteroides cellulosilyticus (HR) DFE model fit')

compare_dfe_sfs(b_coprocola_HR_nonsyn, 
  b_coprocola_HR_gamma, 
  b_coprocola_HR_neugamma) +
  ggtitle('Bacteroides copracola (HR) DFE model fit')

compare_dfe_sfs(b_eggerthii_HR_nonsyn, 
  b_eggerthii_HR_gamma, 
  b_eggerthii_HR_neugamma) +
  ggtitle('Bacteroides eggerthii (HR) DFE model fit')

compare_dfe_sfs(b_fragilis_HR_nonsyn, 
  b_fragilis_HR_gamma, 
  b_fragilis_HR_neugamma) +
  ggtitle('Bacteroides fragilis (HR) DFE model fit')

compare_dfe_sfs(b_ovatus_HR_nonsyn, 
  b_ovatus_HR_gamma, 
  b_ovatus_HR_neugamma) +
  ggtitle('Bacteroides ovatus (HR) DFE model fit')

compare_dfe_sfs(b_stercoris_HR_nonsyn, 
  b_stercoris_HR_gamma, 
  b_stercoris_HR_neugamma) +
  ggtitle('Bacteroides stercoris (HR) DFE model fit')

compare_dfe_sfs(b_thetaiotaomicron_HR_nonsyn, 
  b_thetaiotaomicron_HR_gamma, 
  b_thetaiotaomicron_HR_neugamma) +
  ggtitle('Bacteroides thetaiotaomicron (HR) DFE model fit')

compare_dfe_sfs(b_vulgatus_HR_nonsyn, 
  b_vulgatus_HR_gamma, 
  b_vulgatus_HR_neugamma) +
  ggtitle('Bacteroides vulgatus (HR) DFE model fit')

compare_dfe_sfs(d_invisus_HR_nonsyn, 
  d_invisus_HR_gamma, 
  d_invisus_HR_neugamma) +
  ggtitle('Dialister invisus (HR) DFE model fit')

compare_dfe_sfs(b_intestinihominis_HR_nonsyn, 
  b_intestinihominis_HR_gamma, 
  b_intestinihominis_HR_neugamma) +
  ggtitle('Barnesiella intestinihominis (HR) DFE model fit')

compare_dfe_sfs(e_rectale_HR_nonsyn, 
  e_rectale_HR_gamma, 
  e_rectale_HR_neugamma) +
  ggtitle('Eubacterium rectale (HR) DFE model fit')

compare_dfe_sfs(e_siraeum_HR_nonsyn, 
  e_siraeum_HR_gamma, 
  e_siraeum_HR_neugamma) +
  ggtitle('Eubacterium siraeum (HR) DFE model fit')

compare_dfe_sfs(oscillibacter_sp_HR_nonsyn, 
  oscillibacter_sp_HR_gamma, 
  oscillibacter_sp_HR_neugamma) +
  ggtitle('Oscillibacter sp. (HR) DFE model fit')

compare_dfe_sfs(p_distasonis_HR_nonsyn, 
  p_distasonis_HR_gamma, 
  p_distasonis_HR_neugamma) +
  ggtitle('Parabacteroides distasonis (HR) DFE model fit')

compare_dfe_sfs(p_merdae_HR_nonsyn, 
  p_merdae_HR_gamma, 
  p_merdae_HR_neugamma) +
  ggtitle('Parabacteroides merdae (HR) DFE model fit')

compare_dfe_sfs(r_bicirculans_HR_nonsyn, 
  r_bicirculans_HR_gamma, 
  r_bicirculans_HR_neugamma) +
  ggtitle('Ruminococcus bicirculans (HR) DFE model fit')

compare_dfe_sfs(r_bromii_HR_nonsyn, 
  r_bromii_HR_gamma, 
  r_bromii_HR_neugamma) +
  ggtitle('Ruminococcus bromii (HR) DFE model fit')

# FD SFS DFE comparison

compare_dfe_sfs(a_muciniphila_FD_nonsyn, 
  a_muciniphila_FD_gamma, 
  a_muciniphila_FD_neugamma) + 
  ggtitle('Akkermansia muciniphila (FD) DFE model fit')

compare_dfe_sfs(a_finegoldii_FD_nonsyn, 
  a_finegoldii_FD_gamma, 
  a_finegoldii_FD_neugamma) + 
  ggtitle('Alistipes finegoldii (FD) DFE model fit')

compare_dfe_sfs(a_onderdonkii_FD_nonsyn, 
  a_onderdonkii_FD_gamma, 
  a_onderdonkii_FD_neugamma) + 
  ggtitle('Alistipes onderdonkii (FD) DFE model fit')

compare_dfe_sfs(a_putredinis_FD_nonsyn, 
  a_putredinis_FD_gamma, 
  a_putredinis_FD_neugamma) + 
  ggtitle('Alistipes putredinis (FD) DFE model fit')

compare_dfe_sfs(a_shahii_FD_nonsyn, 
  a_shahii_FD_gamma, 
  a_shahii_FD_neugamma) + 
  ggtitle('Alistipes shahii (FD) DFE model fit')

compare_dfe_sfs(alistipes_sp_FD_nonsyn, 
  alistipes_sp_FD_gamma, 
  alistipes_sp_FD_neugamma) + 
  ggtitle('Alistipes sp. (FD) DFE model fit')

compare_dfe_sfs(b_bacterium_FD_nonsyn, 
  b_bacterium_FD_gamma, 
  b_bacterium_FD_neugamma) + 
  ggtitle('Bacteroidales bacterium (FD) DFE model fit')

compare_dfe_sfs(b_caccae_FD_nonsyn, 
  b_caccae_FD_gamma, 
  b_caccae_FD_neugamma) + 
  ggtitle('Bacteroides caccae (FD) DFE model fit')

compare_dfe_sfs(b_cellulosilyticus_FD_nonsyn, 
  b_cellulosilyticus_FD_gamma, 
  b_cellulosilyticus_FD_neugamma) + 
  ggtitle('Bacteroides cellulosilyticus (FD) DFE model fit')

compare_dfe_sfs(b_coprocola_FD_nonsyn, 
  b_coprocola_FD_gamma, 
  b_coprocola_FD_neugamma) + 
  ggtitle('Bacteroides coprocola (FD) DFE model fit')

compare_dfe_sfs(b_eggerthii_FD_nonsyn, 
  b_eggerthii_FD_gamma, 
  b_eggerthii_FD_neugamma) + 
  ggtitle('Bacteroides eggerthii (FD) DFE model fit')

compare_dfe_sfs(b_fragilis_FD_nonsyn, 
  b_fragilis_FD_gamma, 
  b_fragilis_FD_neugamma) + 
  ggtitle('Bacteroides fragilis (FD) DFE model fit')

compare_dfe_sfs(b_massiliensis_FD_nonsyn, 
  b_massiliensis_FD_gamma, 
  b_massiliensis_FD_neugamma) + 
  ggtitle('Bacteroides massiliensis (FD) DFE model fit')

compare_dfe_sfs(b_ovatus_FD_nonsyn, 
  b_ovatus_FD_gamma, 
  b_ovatus_FD_neugamma) + 
  ggtitle('Bacteroides ovatus (FD) DFE model fit')

compare_dfe_sfs(b_plebeius_FD_nonsyn, 
  b_plebeius_FD_gamma, 
  b_plebeius_FD_neugamma) + 
  ggtitle('Bacteroides plebeius (FD) DFE model fit')

compare_dfe_sfs(b_stercoris_FD_nonsyn, 
  b_stercoris_FD_gamma, 
  b_stercoris_FD_neugamma) + 
  ggtitle('Bacteroides stercoris (FD) DFE model fit')

compare_dfe_sfs(b_thetaiotaomicron_FD_nonsyn, 
  b_thetaiotaomicron_FD_gamma, 
  b_thetaiotaomicron_FD_neugamma) + 
  ggtitle('Bacteroides thetaiotaomicron (FD) DFE model fit')

compare_dfe_sfs(b_uniformis_FD_nonsyn, 
  b_uniformis_FD_gamma, 
  b_uniformis_FD_neugamma) + 
  ggtitle('Bacteroides uniformis (FD) DFE model fit')

compare_dfe_sfs(b_vulgatus_FD_nonsyn, 
  b_vulgatus_FD_gamma, 
  b_vulgatus_FD_neugamma) + 
  ggtitle('Bacteroides vulgatus (FD) DFE model fit')

compare_dfe_sfs(b_xylanisolvens_FD_nonsyn, 
  b_xylanisolvens_FD_gamma, 
  b_xylanisolvens_FD_neugamma) + 
  ggtitle('Bacteroides xylanisolvens (FD) DFE model fit')

compare_dfe_sfs(b_intestinihominis_FD_nonsyn, 
  b_intestinihominis_FD_gamma, 
  b_intestinihominis_FD_neugamma) + 
  ggtitle('Barnesiella intestinihominis (FD) DFE model fit')

compare_dfe_sfs(coprococcus_sp_FD_nonsyn, 
  coprococcus_sp_FD_gamma, 
  coprococcus_sp_FD_neugamma) + 
  ggtitle('Coprococcus sp. (FD) DFE model fit')

compare_dfe_sfs(d_invisus_FD_nonsyn, 
  d_invisus_FD_gamma, 
  d_invisus_FD_neugamma) + 
  ggtitle('Dialister invisus (FD) DFE model fit')

compare_dfe_sfs(e_eligens_FD_nonsyn, 
  e_eligens_FD_gamma, 
  e_eligens_FD_neugamma) + 
  ggtitle('Eubacterium eligens (FD) DFE model fit')

compare_dfe_sfs(e_rectale_FD_nonsyn, 
  e_rectale_FD_gamma, 
  e_rectale_FD_neugamma) + 
  ggtitle('Eubacterium rectale (FD) DFE model fit')

compare_dfe_sfs(e_siraeum_FD_nonsyn, 
  e_siraeum_FD_gamma, 
  e_siraeum_FD_neugamma) + 
  ggtitle('Eubacterium siraeum (FD) DFE model fit')

compare_dfe_sfs(f_prausnitzii_57453_FD_nonsyn, 
  f_prausnitzii_57453_FD_gamma, 
  f_prausnitzii_57453_FD_neugamma) + 
  ggtitle('Faecalibacterium prausnitzii 57453 (FD) DFE model fit')

compare_dfe_sfs(f_prausnitzii_61481_FD_nonsyn, 
  f_prausnitzii_61481_FD_gamma, 
  f_prausnitzii_61481_FD_neugamma) + 
  ggtitle('Faecalibacterium prausnitzii 61481 (FD) DFE model fit')

compare_dfe_sfs(f_prausnitzii_62201_FD_nonsyn, 
  f_prausnitzii_62201_FD_gamma, 
  f_prausnitzii_62201_FD_neugamma) + 
  ggtitle('Faecalibacterium prausnitzii 62201 (FD) DFE model fit')

compare_dfe_sfs(l_bacterium_FD_nonsyn, 
  l_bacterium_FD_gamma, 
  l_bacterium_FD_neugamma) + 
  ggtitle('Lachnospiraceae bacterium (FD) DFE model fit')

compare_dfe_sfs(o_splanchnicus_FD_nonsyn, 
  o_splanchnicus_FD_gamma, 
  o_splanchnicus_FD_neugamma) + 
  ggtitle('Odoribacter splanchnicus (FD) DFE model fit')

compare_dfe_sfs(oscillibacter_sp_FD_nonsyn, 
  oscillibacter_sp_FD_gamma, 
  oscillibacter_sp_FD_neugamma) + 
  ggtitle('Oscillibacter sp. (FD) DFE model fit')

compare_dfe_sfs(p_distasonis_FD_nonsyn, 
  p_distasonis_FD_gamma, 
  p_distasonis_FD_neugamma) + 
  ggtitle('Parabacteroides distasonis (FD) DFE model fit')

compare_dfe_sfs(p_merdae_FD_nonsyn, 
  p_merdae_FD_gamma, 
  p_merdae_FD_neugamma) + 
  ggtitle('Parabacteroides merdae (FD) DFE model fit')

compare_dfe_sfs(phascolarctobacterium_sp_FD_nonsyn, 
  phascolarctobacterium_sp_FD_gamma, 
  phascolarctobacterium_sp_FD_neugamma) +
  ggtitle('Phascolarctobacterium sp. (FD) DFE model fit')

compare_dfe_sfs(p_copri_FD_nonsyn, 
  p_copri_FD_gamma, 
  p_copri_FD_neugamma) + 
  ggtitle('Prevotella copri (FD) DFE model fit')

compare_dfe_sfs(r_intestinalis_FD_nonsyn, 
  r_intestinalis_FD_gamma, 
  r_intestinalis_FD_neugamma) + 
  ggtitle('Roseburia intestinalis (FD) DFE model fit')

compare_dfe_sfs(r_inulinivorans_FD_nonsyn, 
  r_inulinivorans_FD_gamma, 
  r_inulinivorans_FD_neugamma) + 
  ggtitle('Roseburia inulinivorans (FD) DFE model fit')

compare_dfe_sfs(r_bicirculans_FD_nonsyn, 
  r_bicirculans_FD_gamma, 
  r_bicirculans_FD_neugamma) + 
  ggtitle('Ruminococcus bicirculans (FD) DFE model fit')

compare_dfe_sfs(r_bromii_FD_nonsyn, 
  r_bromii_FD_gamma, 
  r_bromii_FD_neugamma) + 
  ggtitle('Ruminococcus bromii (FD) DFE model fit')

### FD vs. HR

temp_supp_df = supplementary_demography_df[which(supplementary_demography_df$species %in% shared_species_list), ]

temp_supplementary_demography_scatter = ggscatter(temp_supp_df, x="nu_mle", y="time_mle", color='species', size=3) +
  ylab('Estimated time in years since most recent demographic event') +
  xlab(x_label_text) +
  geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
  scale_shape_manual(name = "Best-Fit Demographic Model",
                     labels = c("Three Epoch", "Two Epoch"),
                     values = c(17, 19)) +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  guides(color=guide_legend(title="Species")) +
  scale_x_log10(limits=c(1e-2, 2e5)) +
  scale_y_log10(limits=c(2e2, 1e7)) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none')  +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16))

temp_supplementary_demography_scatter

plot_build <- ggplot_build(temp_supplementary_demography_scatter)
color_mapping <- plot_build$data[[1]]$colour
print(color_mapping)

difference_plot =
  temp_supplementary_demography_scatter +
  geom_segment(aes(x=temp_supp_df$nu_mle, y=temp_supp_df$time_mle,
    xend=hr_demography_df$nu_mle, yend=hr_demography_df$time_mle),
    linejoin='round',
    lineend='round',
    linetype=1,
    color=color_mapping,
    arrow = arrow(length=unit(0.2, 'cm'))) +
  # geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  ggtitle('Change in demography from full data to high recombination')

difference_plot

shared_species_df = data.frame(species=shared_species_list, 
  HR_nu_mle = numeric(18),
  FD_nu_mle = numeric(18),
  HR_time_mle = numeric(18),
  FD_time_mle = numeric(18),
  HR_nanc = numeric(18),
  FD_nanc = numeric(18),
  HR_shape = numeric(18),
  FD_shape = numeric(18),
  HR_scale = numeric(18),
  FD_scale = numeric(18),
  HR_mean_s = numeric(18),
  FD_mean_s = numeric(18)
)

for (i in 1:length(shared_species_list)) {
  # nu_mle
  ## HR
  shared_species_df[i, 2] = return_nu_mle(hr_likelihood_surface_list[i])
  ## FD
  shared_species_df[i, 3] = return_nu_mle(fd_likelihood_surface_list[i])
  # tau_mle
  ## HR
  shared_species_df[i, 4] = return_time_mle(hr_likelihood_surface_list[i], 
    hr_sfs_list[i], 
    hr_demography_file_list[i])
  ## FD
  shared_species_df[i, 5] = return_time_mle(fd_likelihood_surface_list[i], 
    fd_sfs_list[i], 
    fd_demography_file_list[i])
  # Nanc
  ## HR
  shared_species_df[i, 6] = nanc_from_demography(hr_demography_file_list[i])
  ## FD
  shared_species_df[i, 7] = nanc_from_demography(fd_demography_file_list[i])
  # shape
  ## HR
  shared_species_df[i, 8] = return_shape_from_dfe(hr_dfe_file_list[i])
  ## FD
  shared_species_df[i, 9] = return_shape_from_dfe(fd_dfe_file_list[i])
  # scale
  ## HR
  shared_species_df[i, 10] = return_scale_from_dfe(hr_dfe_file_list[i])
  ## FD
  shared_species_df[i, 11] = return_scale_from_dfe(fd_dfe_file_list[i])
  # Mean s
  ## HR
  shared_species_df[i, 12] = mean(read_dfe_params(hr_dfe_file_list[i])$gamma_dfe_dist_high)
  ## FD
  shared_species_df[i, 13] = mean(read_dfe_params(fd_dfe_file_list[i])$gamma_dfe_dist_high)
}

shared_species_df

nu_mle_scatter = ggscatter(shared_species_df, x="FD_nu_mle", y="HR_nu_mle", color='species', size=3) +
  ylab('High recombination data') +
  xlab('Full data') +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color=guide_legend(title="species")) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none') +
  geom_abline(slope=1, intercept=0) +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16)) +
  ggtitle('Maximum likelihood estimate of Nu')

nu_mle_scatter

tau_mle_scatter = ggscatter(shared_species_df, x="FD_time_mle", y="HR_time_mle", color='species', size=3) +
  ylab('High recombination data') +
  xlab('Full data') +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color=guide_legend(title="species")) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none') +
  geom_abline(slope=1, intercept=0) +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16)) +
  ggtitle('Maximum likelihood estimate of Time (years)')

tau_mle_scatter

nanc_scatter = ggscatter(shared_species_df, x="FD_nanc", y="HR_nanc", color='species', size=3) +
  ylab('High recombination data') +
  xlab('Full data') +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color=guide_legend(title="species")) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none') +
  geom_abline(slope=1, intercept=0) +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16)) +
  ggtitle('Effective ancestral population size')

nanc_scatter

shape_scatter = ggscatter(shared_species_df, x="FD_shape", y="HR_shape", color='species', size=3) +
  ylab('High recombination data') +
  xlab('Full data') +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color=guide_legend(title="species")) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none') +
  geom_abline(slope=1, intercept=0) +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16)) +
  ggtitle('Gamma-distributed DFE shape parameter')

shape_scatter

scale_scatter = ggscatter(shared_species_df, x="FD_scale", y="HR_scale", color='species', size=3) +
  ylab('High recombination data') +
  xlab('Full data') +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color=guide_legend(title="species")) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none') +
  geom_abline(slope=1, intercept=0) +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16)) +
  ggtitle('Gamma-distributed DFE scale parameter')

scale_scatter

mean_s_scatter = ggscatter(shared_species_df, x="FD_mean_s", y="HR_mean_s", color='species', size=3) +
  ylab('High recombination data') +
  xlab('Full data') +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=4) +
  scale_x_log10() +
  scale_y_log10() +
  guides(color=guide_legend(title="species")) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none') +
  geom_abline(slope=1, intercept=0) +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16)) +
  ggtitle('Gamma-distributed DFE mean selection coefficient')

mean_s_scatter

