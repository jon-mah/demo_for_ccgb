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
compare_sfs(proportional_sfs(a_muciniphila_HR_folded), 
  proportional_sfs(a_muciniphila_HR_one_epoch), 
  proportional_sfs(a_muciniphila_HR_two_epoch), 
  proportional_sfs(a_muciniphila_HR_three_epoch)) +
  ggtitle('Akkermansia muciniphila (HR) model fit')

compare_sfs(proportional_sfs(a_finegoldii_HR_folded), 
  proportional_sfs(a_finegoldii_HR_one_epoch), 
  proportional_sfs(a_finegoldii_HR_two_epoch), 
  proportional_sfs(a_finegoldii_HR_three_epoch)) +
  ggtitle('Alistipes finegoldii (HR) model fit')

compare_sfs(proportional_sfs(a_onderdonkii_HR_folded), 
  proportional_sfs(a_onderdonkii_HR_one_epoch), 
  proportional_sfs(a_onderdonkii_HR_two_epoch), 
  proportional_sfs(a_onderdonkii_HR_three_epoch)) +
  ggtitle('Alistipes onderdonkii (HR) model fit')

compare_sfs(proportional_sfs(a_shahii_HR_folded), 
  proportional_sfs(a_shahii_HR_one_epoch), 
  proportional_sfs(a_shahii_HR_two_epoch), 
  proportional_sfs(a_shahii_HR_three_epoch)) +
  ggtitle('Alistipes shahii (HR) model fit')

compare_sfs(proportional_sfs(b_caccae_HR_folded), 
  proportional_sfs(b_caccae_HR_one_epoch), 
  proportional_sfs(b_caccae_HR_two_epoch), 
  proportional_sfs(b_caccae_HR_three_epoch)) +
  ggtitle('Bacteroides caccae (HR) model fit')

compare_sfs(proportional_sfs(b_cellulosilyticus_HR_folded), 
  proportional_sfs(b_cellulosilyticus_HR_one_epoch), 
  proportional_sfs(b_cellulosilyticus_HR_two_epoch), 
  proportional_sfs(b_cellulosilyticus_HR_three_epoch)) +
  ggtitle('Bacteroides cellulosilyticus (HR) model fit')

compare_sfs(proportional_sfs(b_coprocola_HR_folded), 
  proportional_sfs(b_coprocola_HR_one_epoch), 
  proportional_sfs(b_coprocola_HR_two_epoch), 
  proportional_sfs(b_coprocola_HR_three_epoch)) +
  ggtitle('Bacteroides copracola (HR) model fit')

compare_sfs(proportional_sfs(b_eggerthii_HR_folded), 
  proportional_sfs(b_eggerthii_HR_one_epoch), 
  proportional_sfs(b_eggerthii_HR_two_epoch), 
  proportional_sfs(b_eggerthii_HR_three_epoch)) +
  ggtitle('Bacteroides eggerthii (HR) model fit')

compare_sfs(proportional_sfs(b_fragilis_HR_folded), 
  proportional_sfs(b_fragilis_HR_one_epoch), 
  proportional_sfs(b_fragilis_HR_two_epoch), 
  proportional_sfs(b_fragilis_HR_three_epoch)) +
  ggtitle('Bacteroides fragilis (HR) model fit')

compare_sfs(proportional_sfs(b_ovatus_HR_folded), 
  proportional_sfs(b_ovatus_HR_one_epoch), 
  proportional_sfs(b_ovatus_HR_two_epoch), 
  proportional_sfs(b_ovatus_HR_three_epoch)) +
  ggtitle('Bacteroides ovatus (HR) model fit')

compare_sfs(proportional_sfs(b_stercoris_HR_folded), 
  proportional_sfs(b_stercoris_HR_one_epoch), 
  proportional_sfs(b_stercoris_HR_two_epoch), 
  proportional_sfs(b_stercoris_HR_three_epoch)) +
  ggtitle('Bacteroides stercoris (HR) model fit')

compare_sfs(proportional_sfs(b_thetaiotaomicron_HR_folded), 
  proportional_sfs(b_thetaiotaomicron_HR_one_epoch), 
  proportional_sfs(b_thetaiotaomicron_HR_two_epoch), 
  proportional_sfs(b_thetaiotaomicron_HR_three_epoch)) +
  ggtitle('Bacteroides thetaiotaomicron (HR) model fit')

compare_sfs(proportional_sfs(b_vulgatus_HR_folded), 
  proportional_sfs(b_vulgatus_HR_one_epoch), 
  proportional_sfs(b_vulgatus_HR_two_epoch), 
  proportional_sfs(b_vulgatus_HR_three_epoch)) +
  ggtitle('Bacteroides vulgatus (HR) model fit')

compare_sfs(proportional_sfs(d_invisus_HR_folded), 
  proportional_sfs(d_invisus_HR_one_epoch), 
  proportional_sfs(d_invisus_HR_two_epoch), 
  proportional_sfs(d_invisus_HR_three_epoch)) +
  ggtitle('Dialister invisus (HR) model fit')

compare_sfs(proportional_sfs(b_intestinihominis_HR_folded), 
  proportional_sfs(b_intestinihominis_HR_one_epoch), 
  proportional_sfs(b_intestinihominis_HR_two_epoch), 
  proportional_sfs(b_intestinihominis_HR_three_epoch)) +
  ggtitle('Barnesiella intestinihominis (HR) model fit')

compare_sfs(proportional_sfs(e_rectale_HR_folded), 
  proportional_sfs(e_rectale_HR_one_epoch), 
  proportional_sfs(e_rectale_HR_two_epoch), 
  proportional_sfs(e_rectale_HR_three_epoch)) +
  ggtitle('Eubacterium rectale (HR) model fit')

compare_sfs(proportional_sfs(e_siraeum_HR_folded), 
  proportional_sfs(e_siraeum_HR_one_epoch), 
  proportional_sfs(e_siraeum_HR_two_epoch), 
  proportional_sfs(e_siraeum_HR_three_epoch)) +
  ggtitle('Eubacterium siraeum (HR) model fit')

compare_sfs(proportional_sfs(oscillibacter_sp_HR_folded), 
  proportional_sfs(oscillibacter_sp_HR_one_epoch), 
  proportional_sfs(oscillibacter_sp_HR_two_epoch), 
  proportional_sfs(oscillibacter_sp_HR_three_epoch)) +
  ggtitle('Oscillibacter sp. (HR) model fit')

compare_sfs(proportional_sfs(p_distasonis_HR_folded), 
  proportional_sfs(p_distasonis_HR_one_epoch), 
  proportional_sfs(p_distasonis_HR_two_epoch), 
  proportional_sfs(p_distasonis_HR_three_epoch)) +
  ggtitle('Parabacteroides distasonis (HR) model fit')

compare_sfs(proportional_sfs(p_merdae_HR_folded), 
  proportional_sfs(p_merdae_HR_one_epoch), 
  proportional_sfs(p_merdae_HR_two_epoch), 
  proportional_sfs(p_merdae_HR_three_epoch)) +
  ggtitle('Parabacteroides merdae (HR) model fit')

compare_sfs(proportional_sfs(r_bicirculans_HR_folded), 
  proportional_sfs(r_bicirculans_HR_one_epoch), 
  proportional_sfs(r_bicirculans_HR_two_epoch), 
  proportional_sfs(r_bicirculans_HR_three_epoch)) +
  ggtitle('Ruminococcus bicirculans (HR) model fit')

compare_sfs(proportional_sfs(r_bromii_HR_folded), 
  proportional_sfs(r_bromii_HR_one_epoch), 
  proportional_sfs(r_bromii_HR_two_epoch), 
  proportional_sfs(r_bromii_HR_three_epoch)) +
  ggtitle('Ruminococcus bromii (HR) model fit')

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
  'Oscillibacter sp',
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
