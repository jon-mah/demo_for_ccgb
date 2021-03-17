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

