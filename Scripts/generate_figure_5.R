setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('useful_functions.R')

a_finegoldii_core = read_input_sfs('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_empirical_syn_downsampled_sfs.txt') 
a_finegoldii_core_ns = read_input_sfs('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_empirical_nonsyn_downsampled_sfs.txt') 
a_finegoldii_accessory = read_input_sfs('../SupplementaryAnalysis/Alistipes_finegoldii_56071/accessory_empirical_syn_downsampled_sfs.txt') 
a_finegoldii_accessory_ns = read_input_sfs('../SupplementaryAnalysis/Alistipes_finegoldii_56071/accessory_empirical_nonsyn_downsampled_sfs.txt') 

e_rectale_core = read_input_sfs('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_empirical_syn_downsampled_sfs.txt') 
e_rectale_core_ns = read_input_sfs('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_empirical_nonsyn_downsampled_sfs.txt') 
e_rectale_accessory = read_input_sfs('../SupplementaryAnalysis/Eubacterium_rectale_56927/accessory_empirical_syn_downsampled_sfs.txt') 
e_rectale_accessory_ns = read_input_sfs('../SupplementaryAnalysis/Eubacterium_rectale_56927/accessory_empirical_nonsyn_downsampled_sfs.txt') 

fig_5A = compare_core_accessory_sfs_syn_ns_5A(a_finegoldii_core,
  a_finegoldii_core_ns,
  a_finegoldii_accessory,
  a_finegoldii_accessory_ns) + ggtitle('A. finegoldii')

fig_5B = compare_core_accessory_sfs_syn_ns_5B(e_rectale_core,
  e_rectale_core_ns,
  e_rectale_accessory,
  e_rectale_accessory_ns) + ggtitle('E. rectale')

a_finegoldii_core_dfe = plot_core_accessory_dfe('../SupplementaryAnalysis/Alistipes_finegoldii_56071/core_inferred_DFE.txt') + ggtitle('Core Genes')
e_rectale_core_dfe = plot_core_accessory_dfe('../SupplementaryAnalysis/Eubacterium_rectale_56927/core_inferred_DFE.txt') + ggtitle('Core Genes')
a_finegoldii_acc_dfe = plot_core_accessory_dfe('../SupplementaryAnalysis/Alistipes_finegoldii_56071/accessory_inferred_DFE.txt') + ggtitle('Accessory Genes')
e_rectale_acc_dfe = plot_core_accessory_dfe('../SupplementaryAnalysis/Eubacterium_rectale_56927/accessory_inferred_DFE.txt') + ggtitle('Accessory Genes')


design = "
ABB
ACC
DEE
DFF
"

### Figure 5
# 1000 x 800

png("../Summary/figure_5_output.png", width = 1000, height = 800)
fig_5A + a_finegoldii_core_dfe + a_finegoldii_acc_dfe +
  fig_5B + e_rectale_core_dfe + e_rectale_acc_dfe +
  plot_layout(design=design)
dev.off()
