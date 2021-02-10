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
autoplot(pca)
