# compute_and_plot_PCA.R
# The purpose of this script is to compute and plot
# the principal components of a given SNP matrix.
# Author: Jonathan Mah
# Date: 20210205


library(ggfortify)
setwd("C:/Users/jonat/Desktop/GitHub/demo_for_ccgb")
# Phascolarctobacterium_succinatutens
data = read.csv('./Data/example_output/snp_matrix.csv')
pca = prcomp(data, scale = TRUE)
autoplot(pca) + ggtitle('Phascolarctobacterium succinatutens')

# Bacteroides_vulgatus
data = read.csv('./Analysis/Bacteroides_vulgatus_57955/snp_matrix.csv')
pca = prcomp(data, scale = TRUE)
autoplot(pca) + ggtitle('Bacteroides vulgatus')

# Bacteroides_ovatus
data = read.csv('./Analysis/Bacteroides_ovatus_58035/snp_matrix.csv')
pca = prcomp(data, scale = TRUE)
autoplot(pca) + ggtitle('Bacteroides ovatus')

# Alistipes_putredeinis
data = read.csv('./Analysis/Alistipes_putredinis_61533/snp_matrix.csv')
pca = prcomp(data, scale = TRUE)
autoplot(pca) + ggtitle('Alistipes_putredinis')

# Bacteroides_uniformis
data = read.csv('./Analysis/Bacteroides_uniformis_57318/snp_matrix.csv')
pca = prcomp(data, scale = TRUE)
autoplot(pca) + ggtitle('Bacteroides uniformis')

# Eubacterium_rectale
data = read.csv('./Analysis/Eubacterium_rectale_56927/snp_matrix.csv')
pca = prcomp(data, scale = TRUE)
autoplot(pca) + ggtitle('Eubacterium rectale')

