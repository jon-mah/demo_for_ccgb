# DFE_visual_comparison.R
# Author: Jonathan Mah
# Date: 20210316
# This script is used to visually compare different DFE's.

library(ggplot2)
library(reshape2)

set.seed(1)

# B_vulgatus

b_vulgatus_gamma_no_s_shape = 0.0280127
b_vulgatus_gamma_no_s_scale = 0.1547839

b_vulgatus_gamma_no_s_dist = rgamma(10000, shape=b_vulgatus_gamma_no_s_shape, scale=b_vulgatus_gamma_no_s_scale)

b_vulgatus_neugamma_no_s_shape = 0.3280127
b_vulgatus_neugamma_no_s_scale = 0.2547839

b_vulgatus_neugamma_no_s_dist = rgamma(10000, shape=b_vulgatus_neugamma_no_s_shape, scale=b_vulgatus_neugamma_no_s_scale)

b_vulgatus_neugamma_no_s_dist[1:3000] = 0.0002

b_vulgatus_no_s_df = data.frame(gamma=b_vulgatus_gamma_no_s_dist,
                                neugamma=b_vulgatus_neugamma_no_s_dist)

ggplot(melt(b_vulgatus_no_s_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  ggtitle('B. vulgatus, no singletons')

b_vulgatus_gamma_no_d_shape = 0.3562186
b_vulgatus_gamma_no_d_scale = 0.2356798

b_vulgatus_gamma_no_d_dist = rgamma(10000, shape=b_vulgatus_gamma_no_d_shape, scale=b_vulgatus_gamma_no_d_scale)

b_vulgatus_neugamma_no_d_shape = 0.6357227
b_vulgatus_neugamma_no_d_scale = 0.4673892

b_vulgatus_neugamma_no_d_dist = rgamma(10000, shape=b_vulgatus_neugamma_no_d_shape, scale=b_vulgatus_neugamma_no_d_scale)

b_vulgatus_neugamma_no_d_dist[1:3000] = 0.0002

b_vulgatus_no_d_df = data.frame(gamma=b_vulgatus_gamma_no_d_dist,
                                neugamma=b_vulgatus_neugamma_no_d_dist)

ggplot(melt(b_vulgatus_no_d_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  ggtitle('B. vulgatus, no doubletons')

# B_ovatus

b_ovatus_gamma_no_s_shape = 0.0253678
b_ovatus_gamma_no_s_scale = 0.1463287

b_ovatus_gamma_no_s_dist = rgamma(10000, shape=b_ovatus_gamma_no_s_shape, scale=b_ovatus_gamma_no_s_scale)

b_ovatus_neugamma_no_s_shape = 0.3063287
b_ovatus_neugamma_no_s_scale = 0.2239760

b_ovatus_neugamma_no_s_dist = rgamma(10000, shape=b_ovatus_neugamma_no_s_shape, scale=b_ovatus_neugamma_no_s_scale)

b_ovatus_neugamma_no_s_dist[1:3000] = 0.0002

b_ovatus_no_s_df = data.frame(gamma=b_ovatus_gamma_no_s_dist,
                                neugamma=b_ovatus_neugamma_no_s_dist)

ggplot(melt(b_ovatus_no_s_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  ggtitle('B. ovatus, no singletons')

b_ovatus_gamma_no_d_shape = 0.3961328
b_ovatus_gamma_no_d_scale = 0.2673281

b_ovatus_gamma_no_d_dist = rgamma(10000, shape=b_ovatus_gamma_no_d_shape, scale=b_ovatus_gamma_no_d_scale)

b_ovatus_neugamma_no_d_shape = 0.8367219
b_ovatus_neugamma_no_d_scale = 0.56783939

b_ovatus_neugamma_no_d_dist = rgamma(10000, shape=b_ovatus_neugamma_no_d_shape, scale=b_ovatus_neugamma_no_d_scale)

b_ovatus_neugamma_no_d_dist[1:3000] = 0.0002

b_ovatus_no_d_df = data.frame(gamma=b_ovatus_gamma_no_d_dist,
                                neugamma=b_ovatus_neugamma_no_d_dist)

ggplot(melt(b_ovatus_no_d_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  ggtitle('B. ovatus, no doubletons')

# B_uniformis

b_uniformis_gamma_no_s_shape = 0.0237283
b_uniformis_gamma_no_s_scale = 0.1453879

b_uniformis_gamma_no_s_dist = rgamma(10000, shape=b_uniformis_gamma_no_s_shape, scale=b_uniformis_gamma_no_s_scale)

b_uniformis_neugamma_no_s_shape = 0.5361287
b_uniformis_neugamma_no_s_scale = 0.1643872

b_uniformis_neugamma_no_s_dist = rgamma(10000, shape=b_uniformis_neugamma_no_s_shape, scale=b_uniformis_neugamma_no_s_scale)

b_uniformis_neugamma_no_s_dist[1:3000] = 0.0002

b_uniformis_no_s_df = data.frame(gamma=b_uniformis_gamma_no_s_dist,
                                 neugamma=b_uniformis_neugamma_no_s_dist)

ggplot(melt(b_uniformis_no_s_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  ggtitle('B. uniformis, no singletons')

b_uniformis_gamma_no_d_shape = 0.3863287
b_uniformis_gamma_no_d_scale = 0.2036871

b_uniformis_gamma_no_d_dist = rgamma(10000, shape=b_uniformis_gamma_no_d_shape, scale=b_uniformis_gamma_no_d_scale)

b_uniformis_neugamma_no_d_shape = 0.5849327
b_uniformis_neugamma_no_d_scale = 0.5789321

b_uniformis_neugamma_no_d_dist = rgamma(10000, shape=b_uniformis_neugamma_no_d_shape, scale=b_uniformis_neugamma_no_d_scale)

b_uniformis_neugamma_no_d_dist[1:3000] = 0.0002

b_uniformis_no_d_df = data.frame(gamma=b_uniformis_gamma_no_d_dist,
                                neugamma=b_uniformis_neugamma_no_d_dist)

ggplot(melt(b_uniformis_no_d_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  ggtitle('B. uniformis, no doubletons')


# 

ggplot(melt(b_vulgatus_no_s_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  xlab('Magnitude of selection coefficient') +
  ylab('Density of sites') +
  ggtitle('A. onderdonkii, no singletons')

ggplot(melt(b_ovatus_no_s_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  xlab('Magnitude of selection coefficient') +
  ylab('Density of sites') +
  ggtitle('B. thetaiotaomicron, no singletons')

ggplot(melt(b_uniformis_no_s_df), aes(x=value, y=..density.., fill = variable)) + 
  geom_histogram(position = "dodge",
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  xlab('Magnitude of selection coefficient') +
  ylab('Density of sites') +
  ggtitle('P. distasonis, no singletons')

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

gamma_dfe_data = data.frame(b_vulgatus_gamma_no_s_dist,
                            b_ovatus_gamma_no_s_dist,
                            b_uniformis_gamma_no_s_dist)
colnames(gamma_dfe_data) = c('A. onderdonkii', 'B. thetaiotaomicron', 'P. distasonis')

ggplot(melt(gamma_dfe_data), aes(x=value, y=..density.., fill=variable)) + 
  geom_histogram(position='dodge',
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  xlab('Magnitude of selection coefficient') +
  ylab('Density of sites') +
  ggtitle('Gamma-distributed DFE, no singletons')

neugamma_dfe_data = data.frame(b_vulgatus_neugamma_no_s_dist,
                               b_ovatus_neugamma_no_s_dist,
                               b_uniformis_neugamma_no_s_dist)
colnames(neugamma_dfe_data) = c('A. onderdonkii', 'B. thetaiotaomicron', 'P. distasonis')

ggplot(melt(neugamma_dfe_data), aes(x=value, y=..density.., fill=variable)) + 
  geom_histogram(position='dodge',
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  xlab('Magnitude of selection coefficient') +
  ylab('Density of sites') +
  ggtitle('Neutral + Gamma-distributed DFE, no singletons')

# A. finegoldii

a_finegoldii_gamma_no_s_shape = 0.0273179
a_finegoldii_gamma_no_s_scale = 0.1723789

a_finegoldii_gamma_no_s_dist = rgamma(10000, shape=a_finegoldii_gamma_no_s_shape, scale=a_finegoldii_gamma_no_s_scale)

# A. onderdonkii
# A. muciniphila

a_muciniphila_gamma_no_s_shape = 0.0258129
a_muciniphila_gamma_no_s_scale = 0.1487210

a_muciniphila_gamma_no_s_dist = rgamma(10000, shape=a_muciniphila_gamma_no_s_shape, scale=a_muciniphila_gamma_no_s_scale)

# B. bacterium

b_bacterium_gamma_no_s_shape = 0.0317382
b_bacterium_gamma_no_s_scale = 0.1712389

b_bacterium_gamma_no_s_dist = rgamma(10000, shape=b_bacterium_gamma_no_s_shape, scale=b_bacterium_gamma_no_s_scale)

# B. intestinihominis

b_intestinihominis_gamma_no_s_shape = 0.0247721
b_intestinihominis_gamma_no_s_scale = 0.1932167

b_intestinihominis_gamma_no_s_dist = rgamma(10000, shape=b_intestinihominis_gamma_no_s_shape, scale=b_intestinihominis_gamma_no_s_scale)

# B. thetaiotaomicron
# P. distasonis
# P. merdae

p_merdae_gamma_no_s_shape = 0.0222617
p_merdae_gamma_no_s_scale = 0.2183712

p_merdae_gamma_no_s_dist = rgamma(10000, shape=p_merdae_gamma_no_s_shape, scale=p_merdae_gamma_no_s_scale)

# P. sp

p_sp_gamma_no_s_shape = 0.0237321
p_sp_gamma_no_s_scale = 0.1551782

p_sp_gamma_no_s_dist = rgamma(10000, shape=p_sp_gamma_no_s_shape, scale=p_sp_gamma_no_s_scale)


gamma_dfe_data = data.frame(a_finegoldii_gamma_no_s_dist, b_vulgatus_gamma_no_s_dist, a_muciniphila_gamma_no_s_dist, 
                            b_bacterium_gamma_no_s_dist, b_intestinihominis_gamma_no_s_dist, b_ovatus_gamma_no_s_dist,
                            b_uniformis_gamma_no_s_dist, p_merdae_gamma_no_s_dist, p_sp_gamma_no_s_dist)
colnames(gamma_dfe_data) = c('A. finegoldii', 'A. onderdonkii', 'A. muciniphila', 
                             'B. bacterium', 'B. intestinihominis', 'B. thetaiotaomicron', 
                             'P. distasonis', 'P. merdae', 'P. sp')

ggplot(melt(gamma_dfe_data), aes(x=value, y=..density.., fill=variable)) + 
  geom_histogram(position='dodge',
                 breaks=c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_x_log10() +
  xlab('Magnitude of selection coefficient') +
  ylab('Density of sites') +
  geom_vline(xintercept=1e-03) +
  geom_vline(xintercept=1e-02) +
  geom_vline(xintercept=1e-01) +
  ggtitle('Gamma-distributed DFE, no singletons')
