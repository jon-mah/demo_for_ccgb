library(ggplot2)
library(ggrepel)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(fitdistrplus)
library(scales)
library(reshape2)
library(stringr)

fold_sfs = function(input_sfs) {
  input_length = length(input_sfs)
  folded_length = length(input_sfs) / 2
  if (input_length %% 2 == 1) {
    folded_length = folded_length + 1
  }
  output_sfs = c()
  for (i in 1:folded_length) {
    if (input_sfs[i] == input_sfs[input_length - i + 1]) {
      output_sfs[i] = input_sfs[i]
    } else {
      output_sfs[i] = input_sfs[i] + input_sfs[input_length - i + 1]
    }
  }
  return(output_sfs)
}

proportional_sfs = function(input_sfs) {
  return (input_sfs / sum(input_sfs))
}

read_input_sfs = function(input_file)  {
  ## Reads input SFS in Dadi Format
  this_file = file(input_file)
  on.exit(close(this_file))
  sfs_string = readLines(this_file)[2]
  output_sfs = as.numeric(unlist(strsplit(sfs_string, ' ')))
  output_sfs = output_sfs[-1] ## Remove 0-tons
  output_sfs = fold_sfs(output_sfs)
  return(output_sfs)
}

read_input_sfs_original = function(input_file)  {
  ## Reads input SFS in Dadi Format
  this_file = file(input_file)
  on.exit(close(this_file))
  sfs_string = readLines(this_file)[2]
  output_sfs = as.numeric(unlist(strsplit(sfs_string, ' ')))
  return(output_sfs)
}

sfs_from_demography = function(input_file) {
  ## Reads input SFS from output *demography.txt
  this_file = file(input_file)
  on.exit(close(this_file))
  sfs_string = readLines(this_file)[7]
  output_sfs = strsplit(sfs_string, '-- ')
  output_sfs = unlist(output_sfs)[2]
  output_sfs = unlist(strsplit(output_sfs, ' '))
  output_sfs = output_sfs[-length(output_sfs)]
  ## output_sfs = output_sfs[-1]
  output_sfs = as.numeric(output_sfs)
  output_sfs = fold_sfs(output_sfs)
  return(output_sfs)
}

gamma_sfs_from_dfe = function(input_file) {
  ## Reads input SFS from output *demography.txt
  this_file = file(input_file)
  on.exit(close(this_file))
  sfs_string = readLines(this_file)[7]
  output_sfs = strsplit(sfs_string, '-- ')
  output_sfs = unlist(output_sfs)[2]
  output_sfs = unlist(strsplit(output_sfs, ' '))
  # output_sfs = output_sfs[-length(output_sfs)]
  ## output_sfs = output_sfs[-1]
  output_sfs = as.numeric(output_sfs)
  return(output_sfs)
}

neugamma_sfs_from_dfe = function(input_file) {
  ## Reads input SFS from output *demography.txt
  this_file = file(input_file)
  on.exit(close(this_file))
  sfs_string = readLines(this_file)[14]
  output_sfs = strsplit(sfs_string, '-- ')
  output_sfs = unlist(output_sfs)[2]
  output_sfs = unlist(strsplit(output_sfs, ' '))
  # output_sfs = output_sfs[-length(output_sfs)]
  ## output_sfs = output_sfs[-1]
  output_sfs = as.numeric(output_sfs)
  return(output_sfs)
}

empirical_sfs_from_dfe = function(input_file) {
  ## Reads input SFS from output *demography.txt
  this_file = file(input_file)
  on.exit(close(this_file))
  sfs_string = readLines(this_file)[6]
  output_sfs = strsplit(sfs_string, '-- ')
  output_sfs = unlist(output_sfs)[2]
  output_sfs = unlist(strsplit(output_sfs, ' '))
  # output_sfs = output_sfs[-length(output_sfs)]
  ## output_sfs = output_sfs[-1]
  output_sfs = as.numeric(output_sfs)
  return(output_sfs)
}

plot_likelihood_surface = function(input) {
  species_surface = read.csv(input, header=FALSE)
  names(species_surface) = c('likelihood', 'nu', 'tau')
  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]
  print(head(species_surface, 1))
  species_surface_quantile = quantile(species_surface$likelihood, 0.80)
  species_surface_max_minus = max(species_surface$likelihood) - 10
  species_surface_cutoff = max(c(species_surface_quantile, species_surface_max_minus))
  
  species_surface[species_surface$likelihood < species_surface_cutoff, ]$likelihood = species_surface_cutoff
  
  # species_surface_scatter = ggplot(data = species_surface) +
  #   geom_tile(aes(x = nu, y = tau, fill = likelihood))
    # stat_contour(aes(x = nu, y = tau, z = likelihood))
    # scale_x_continuous(trans='log10') +
    # scale_y_continuous(trans='log10')
  
  
  species_surface_scatter = ggplot(data=species_surface, aes(x=nu, y=tau)) + 
   geom_point(aes(color=likelihood)) +
   scale_fill_brewer(palette = "Greys") +
   scale_x_continuous(trans='log10') +
   scale_y_continuous(trans='log10') +
   geom_vline(xintercept=1.0, color='red') +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"))

  return(species_surface_scatter)
}

compare_sfs = function(empirical, one_epoch, two_epoch) {
  x_axis = 1:length(empirical)

  input_df = data.frame(empirical,
                        two_epoch,
                        one_epoch,
                        x_axis)
  
  names(input_df) = c('Empirical',
                      'Two-epoch',
                      'One-epoch',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                                                     aes(x=x_axis, 
                                                         y=value,
                                                         fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}


compare_hmp_sfs = function(original, complete, one_epoch) {
  x_axis = 1:length(original[-1])
  
  input_df = data.frame(proportional_sfs(original[-1]),
                        proportional_sfs(complete[-1]),
                        proportional_sfs(one_epoch),
                        x_axis)
  
  names(input_df) = c('Original SFS',
                      'Complete SFS',
                      'One Epoch',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}

compare_isolate_sfs = function(HMP_QP, Isolate, one_epoch, two_epoch) {
  x_axis = 1:length(HMP_QP)
  
  input_df = data.frame(proportional_sfs(HMP_QP), 
                        proportional_sfs(Isolate), 
                        proportional_sfs(one_epoch),
                        proportional_sfs(two_epoch),
                        x_axis)
  
  names(input_df) = c('HMP QP',
                      'Isolate',
                      'One epoch (Isolate)',
                      'Two epoch (Isolate)',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    ylim(0, 1.0)
  return(p_input_comparison)
}

compare_sfs_cornejo_count = function(input_directory) {
  empirical_syn_sfs_file = paste(input_directory, 'downsampled_syn_sfs.txt', sep='')
  model_syn_sfs_file = paste(input_directory, 'two_epoch_demography.txt', sep='')
  nonsyn_sfs_file = paste(input_directory, 'inferred_DFE.txt', sep='')
  
  model_syn_sfs = sfs_from_demography(model_syn_sfs_file)
  empirical_nonsyn_sfs = empirical_sfs_from_dfe(nonsyn_sfs_file)
  model_nonsyn_sfs = gamma_sfs_from_dfe(nonsyn_sfs_file)
  neugamma_nonsyn_sfs = neugamma_sfs_from_dfe(nonsyn_sfs_file)
  
  x_axis = 1:length(empirical_nonsyn_sfs)
  
  input_df = data.frame(empirical_nonsyn_sfs,
                        model_syn_sfs,
                        model_nonsyn_sfs,
                        neugamma_nonsyn_sfs,
                        x_axis)
  
  names(input_df) = c('Empirical nonsynonymous',
                      'Two-epoch Demography',
                      'Gamma-distributed DFE',
                      'Neu-gamma-distributed DFE',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)  
}

compare_isolate_hmp_sfs = function(hmp_orig, orig_demo, hmp_complete, complete_demo, isolate, isolate_demo, one_epoch) {
  x_axis = 1:length(hmp_orig)
  
  input_df = data.frame(proportional_sfs(hmp_orig),
                        proportional_sfs(orig_demo),
                        proportional_sfs(hmp_complete),
                        proportional_sfs(complete_demo),
                        proportional_sfs(isolate),
                        proportional_sfs(isolate_demo),
                        proportional_sfs(one_epoch),
                        x_axis)
  
  names(input_df) = c('HMP (Garud/Good)',
                      'Garud/Good Two Epoch',
                      'HMP (Complete)',
                      'Complete Two Epoch',
                      'UHGG Isolate',
                      'Isolate Two Epoch',
                      'One Epoch',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    ylim(0, 1.0)
  return(p_input_comparison)
}

compare_sfs_cornejo_proportional = function(input_directory) {
  empirical_syn_sfs_file = paste(input_directory, 'downsampled_syn_sfs.txt', sep='')
  model_syn_sfs_file = paste(input_directory, 'two_epoch_demography.txt', sep='')
  nonsyn_sfs_file = paste(input_directory, 'inferred_DFE.txt', sep='')
  
  model_syn_sfs = proportional_sfs(sfs_from_demography(model_syn_sfs_file))
  empirical_nonsyn_sfs = proportional_sfs(empirical_sfs_from_dfe(nonsyn_sfs_file))
  model_nonsyn_sfs = proportional_sfs(gamma_sfs_from_dfe(nonsyn_sfs_file))
  neugamma_nonsyn_sfs = proportional_sfs(neugamma_sfs_from_dfe(nonsyn_sfs_file))
  
  x_axis = 1:length(empirical_nonsyn_sfs)
  
  input_df = data.frame(empirical_nonsyn_sfs,
                        model_syn_sfs,
                        model_nonsyn_sfs,
                        neugamma_nonsyn_sfs,
                        x_axis)
  
  names(input_df) = c('Empirical nonsynonymous',
                      'Two-epoch Demography',
                      'Gamma-distributed DFE',
                      'Neu-gamma-distributed DFE',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)  
}

compare_sfs_with_selection_count = function(input_directory) {
  empirical_syn_sfs_file = paste(input_directory, 'downsampled_syn_sfs.txt', sep='')
  model_syn_sfs_file = paste(input_directory, 'two_epoch_demography.txt', sep='')
  nonsyn_sfs_file = paste(input_directory, 'inferred_DFE.txt', sep='')

  empirical_syn_sfs = read_input_sfs(empirical_syn_sfs_file)
  model_syn_sfs = sfs_from_demography(model_syn_sfs_file)
  empirical_nonsyn_sfs = empirical_sfs_from_dfe(nonsyn_sfs_file)
  model_nonsyn_sfs = gamma_sfs_from_dfe(nonsyn_sfs_file)
  
  x_axis = 1:length(empirical_syn_sfs)
  
  input_df = data.frame(empirical_syn_sfs,
                        model_syn_sfs,
                        empirical_nonsyn_sfs,
                        model_nonsyn_sfs,
                        x_axis)
  
  names(input_df) = c('Empirical synonymous',
                      'Two-epoch Demography',
                      'Empirical nonsynonymous',
                      'Gamma-distributed DFE',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}

compare_sfs_with_selection_proportional = function(input_directory) {
  empirical_syn_sfs_file = paste(input_directory, 'downsampled_syn_sfs.txt', sep='')
  model_syn_sfs_file = paste(input_directory, 'two_epoch_demography.txt', sep='')
  nonsyn_sfs_file = paste(input_directory, 'inferred_DFE.txt', sep='')

  empirical_syn_sfs = proportional_sfs(read_input_sfs(empirical_syn_sfs_file))
  model_syn_sfs = proportional_sfs(sfs_from_demography(model_syn_sfs_file))
  empirical_nonsyn_sfs = proportional_sfs(empirical_sfs_from_dfe(nonsyn_sfs_file))
  model_nonsyn_sfs = proportional_sfs(gamma_sfs_from_dfe(nonsyn_sfs_file))
  
  x_axis = 1:length(empirical_syn_sfs)
  
  input_df = data.frame(empirical_syn_sfs,
                        model_syn_sfs,
                        empirical_nonsyn_sfs,
                        model_nonsyn_sfs,
                        x_axis)
  
  names(input_df) = c('Empirical synonymous',
                      'Two-epoch Demography',
                      'Empirical nonsynonymous',
                      'Gamma-distributed DFE',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Count of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}



plot_original_empirical_sfs = function(input) {
  x_axis = 0:(length(input)-1)
  
  input_df = data.frame(input,
                        x_axis)
  
  names(input_df) = c('Empirical',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=x_axis, limits=c(-0.5, length(x_axis) + 0.5)) +
    ylab('Number of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  return(p_input_comparison)
}

plot_dfe = function(input_dfe_file) {
  ## Reads input DFE from output *inferred_DFE.txt
  this_file = file(input_dfe_file) # Open file
  on.exit(close(this_file)) # Close when done
  # Parse file and string manipulation
  param_string_high = readLines(this_file)[4]
  param_string_high = strsplit(param_string_high, split=': ')
  param_string_high = unlist(param_string_high)[2]
  param_string_high = str_sub(param_string_high, 2, -3)
  param_string_high = unlist(strsplit(param_string_high, ' '))
  param_string_high = as.numeric(param_string_high)
  
  gamma_shape = param_string_high[1]
  gamma_scale_high = param_string_high[2]
  
  param_string_low = readLines(this_file)[5]
  param_string_low = strsplit(param_string_low, split=': ')
  param_string_low = unlist(param_string_low)[2]
  param_string_low = str_sub(param_string_low, 2, -3)
  param_string_low = unlist(strsplit(param_string_low, ' '))
  param_string_low = as.numeric(param_string_low)
  
  gamma_scale_low = param_string_low[2]
  
  gamma_dfe_dist_high = rgamma(10000, shape=gamma_shape, scale=gamma_scale_high)
  gamma_dfe_dist_low = rgamma(10000, shape=gamma_shape, scale=gamma_scale_low)
  
  param_string_high = readLines(this_file)[11]
  param_string_high = strsplit(param_string_high, split=': ')
  param_string_high = unlist(param_string_high)[2]
  param_string_high = str_sub(param_string_high, 2, -3)
  param_string_high = unlist(strsplit(param_string_high, ' '))
  param_string_high = as.numeric(param_string_high)
  
  neugamma_proportion = param_string_high[1]
  neugamma_shape = param_string_high[2]
  neugamma_scale_high = param_string_high[3]
  
  param_string_low = readLines(this_file)[12]
  param_string_low = strsplit(param_string_low, split=': ')
  param_string_low = unlist(param_string_low)[2]
  param_string_low = str_sub(param_string_low, 2, -3)
  param_string_low = unlist(strsplit(param_string_low, ' '))
  param_string_low = as.numeric(param_string_low)
  
  neugamma_scale_low = param_string_low[3]
  
  neugamma_dfe_dist_high = rgamma(10000, shape=neugamma_shape, scale=neugamma_scale_high)
  neugamma_dfe_dist_low = rgamma(10000, shape=neugamma_shape, scale=neugamma_scale_low)
  
  zeroed_sites = as.integer(10000 * neugamma_proportion)
  
  neugamma_dfe_dist_high[1:zeroed_sites] = 1e-06* 1.1
  neugamma_dfe_dist_low[1:zeroed_sites] = 1e-06 * 1.1
  
  dfe_df = data.frame(gamma_dfe_dist_high, 
                      gamma_dfe_dist_low,
                      neugamma_dfe_dist_high,
                      neugamma_dfe_dist_low)
  dfe_df[dfe_df < 1e-6] = 1e-6
  
  names(dfe_df) = c('Gamma, mu=6.93E-10', 'Gamma, mu=4.08E-10',
                    'Neugamma, mu=6.93E-10', 'Neugamma, mu=4.08E-10')

  ggplot(melt(dfe_df), aes(x=value, y=..density.., fill=variable)) +
    geom_histogram(position='dodge',
                   breaks=c(0.000001, 0.00001,  0.0001, 0.001, 0.01)) +
    scale_x_log10() +
    ylab('Proportion of sites') +
    xlab('Selective Effect') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    guides(fill=guide_legend(title="Estimated mutation rate"))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Downsampled to 10

## A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_10/a_finegoldii_10.csv')
a_finegoldii_10_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_10/empirical_sfs.txt'
)
a_finegoldii_10_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_10/one_epoch_demography.txt'
)
a_finegoldii_10_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_10/two_epoch_demography.txt'
)
a_finegoldii_original_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_10/original_empirical_sfs.txt'
)

compare_sfs(a_finegoldii_10_empirical,
            a_finegoldii_10_one_epoch,
            a_finegoldii_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 10')

compare_sfs(proportional_sfs(a_finegoldii_10_empirical),
            proportional_sfs(a_finegoldii_10_one_epoch),
            proportional_sfs(a_finegoldii_10_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 10')

## A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_10/a_muciniphila_10.csv')
a_muciniphila_10_empirical =  read_input_sfs(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_10/empirical_sfs.txt'
)
a_muciniphila_10_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_10/one_epoch_demography.txt'
)
a_muciniphila_10_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_10/two_epoch_demography.txt'
)
a_muciniphila_original_empirical = read_input_sfs_original(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(a_muciniphila_10_empirical,
            a_muciniphila_10_one_epoch,
            a_muciniphila_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 10')

compare_sfs(proportional_sfs(a_muciniphila_10_empirical),
            proportional_sfs(a_muciniphila_10_one_epoch),
            proportional_sfs(a_muciniphila_10_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 10')


## A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_10/a_onderdonkii_10.csv')
a_onderdonkii_10_empirical =  read_input_sfs(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_10/empirical_sfs.txt'
)
a_onderdonkii_10_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_10/one_epoch_demography.txt'
)
a_onderdonkii_10_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_10/two_epoch_demography.txt'
)
a_onderdonkii_original_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_10/original_empirical_sfs.txt'
)

compare_sfs(a_onderdonkii_10_empirical,
            a_onderdonkii_10_one_epoch,
            a_onderdonkii_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 10')

compare_sfs(proportional_sfs(a_onderdonkii_10_empirical),
            proportional_sfs(a_onderdonkii_10_one_epoch),
            proportional_sfs(a_onderdonkii_10_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 10')


## A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_10/a_putredinis_10.csv')
a_putredinis_10_empirical =  read_input_sfs(
  '../Analysis/Alistipes_putredinis_61533_downsampled_10/empirical_sfs.txt'
)
a_putredinis_10_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_10/one_epoch_demography.txt'
)
a_putredinis_10_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_10/two_epoch_demography.txt'
)
a_putredinis_original_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_putredinis_61533_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(a_putredinis_10_empirical,
            a_putredinis_10_one_epoch,
            a_putredinis_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 10')

compare_sfs(proportional_sfs(a_putredinis_10_empirical),
            proportional_sfs(a_putredinis_10_one_epoch),
            proportional_sfs(a_putredinis_10_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 10')



## A. shahii
plot_likelihood_surface('../Analysis/qp_gut_10/a_shahii_10.csv')
a_shahii_10_empirical =  read_input_sfs(
  '../Analysis/Alistipes_shahii_62199_downsampled_10/empirical_sfs.txt'
)
a_shahii_10_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_10/one_epoch_demography.txt'
)
a_shahii_10_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_10/two_epoch_demography.txt'
)
a_shahii_original_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_shahii_62199_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(a_shahii_10_empirical,
            a_shahii_10_one_epoch,
            a_shahii_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 10')

compare_sfs(proportional_sfs(a_shahii_10_empirical),
            proportional_sfs(a_shahii_10_one_epoch),
            proportional_sfs(a_shahii_10_two_epoch)) +
  ggtitle('A. shahii Downsampled to 10')


## B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_10/b_bacterium_10.csv')
b_bacterium_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_10/empirical_sfs.txt'
)
b_bacterium_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_10/one_epoch_demography.txt'
)
b_bacterium_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_10/two_epoch_demography.txt'
)
b_bacterium_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_bacterium_10_empirical,
            b_bacterium_10_one_epoch,
            b_bacterium_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 10')

compare_sfs(proportional_sfs(b_bacterium_10_empirical),
            proportional_sfs(b_bacterium_10_one_epoch),
            proportional_sfs(b_bacterium_10_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 10')

## B. caccae
plot_likelihood_surface('../Analysis/qp_gut_10/b_caccae_10.csv')
b_caccae_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_caccae_53434_downsampled_10/empirical_sfs.txt'
)
b_caccae_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_10/one_epoch_demography.txt'
)
b_caccae_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_10/two_epoch_demography.txt'
)
b_caccae_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_caccae_53434_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_caccae_10_empirical,
            b_caccae_10_one_epoch,
            b_caccae_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 10')

compare_sfs(proportional_sfs(b_caccae_10_empirical),
            proportional_sfs(b_caccae_10_one_epoch),
            proportional_sfs(b_caccae_10_two_epoch)) +
  ggtitle('B. caccae Downsampled to 10')

## B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_10/b_cellulosilyticus_10.csv')
b_cellulosilyticus_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/empirical_sfs.txt'
)
b_cellulosilyticus_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/one_epoch_demography.txt'
)
b_cellulosilyticus_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/two_epoch_demography.txt'
)
b_cellulosilyticus_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_cellulosilyticus_10_empirical,
            b_cellulosilyticus_10_one_epoch,
            b_cellulosilyticus_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 10')

compare_sfs(proportional_sfs(b_cellulosilyticus_10_empirical),
            proportional_sfs(b_cellulosilyticus_10_one_epoch),
            proportional_sfs(b_cellulosilyticus_10_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 10')


##  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_10/b_fragilis_10.csv')
b_fragilis_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_10/empirical_sfs.txt'
)
b_fragilis_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_10/one_epoch_demography.txt'
)
b_fragilis_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_10/two_epoch_demography.txt'
)
b_fragilis_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_fragilis_10_empirical,
            b_fragilis_10_one_epoch,
            b_fragilis_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 10')

compare_sfs(proportional_sfs(b_fragilis_10_empirical),
            proportional_sfs(b_fragilis_10_one_epoch),
            proportional_sfs(b_fragilis_10_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 10')


## B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_10/b_intestinihominis_10.csv')
b_intestinihominis_10_empirical =  read_input_sfs(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/empirical_sfs.txt'
)
b_intestinihominis_10_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/one_epoch_demography.txt'
)
b_intestinihominis_10_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/two_epoch_demography.txt'
)
b_intestinihominis_original_empirical = read_input_sfs_original(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_intestinihominis_10_empirical,
            b_intestinihominis_10_one_epoch,
            b_intestinihominis_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 10')

compare_sfs(proportional_sfs(b_intestinihominis_10_empirical),
            proportional_sfs(b_intestinihominis_10_one_epoch),
            proportional_sfs(b_intestinihominis_10_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 10')



## B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_10/b_ovatus_10.csv')
b_ovatus_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_10/empirical_sfs.txt'
)
b_ovatus_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_10/one_epoch_demography.txt'
)
b_ovatus_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_10/two_epoch_demography.txt'
)
b_ovatus_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_ovatus_10_empirical,
            b_ovatus_10_one_epoch,
            b_ovatus_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 10')

compare_sfs(proportional_sfs(b_ovatus_10_empirical),
            proportional_sfs(b_ovatus_10_one_epoch),
            proportional_sfs(b_ovatus_10_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 10')


## B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_10/b_thetaiotaomicron_10.csv')
b_thetaiotaomicron_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/empirical_sfs.txt'
)
b_thetaiotaomicron_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/one_epoch_demography.txt'
)
b_thetaiotaomicron_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/two_epoch_demography.txt'
)
b_thetaiotaomicron_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_thetaiotaomicron_10_empirical,
            b_thetaiotaomicron_10_one_epoch,
            b_thetaiotaomicron_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 10')

compare_sfs(proportional_sfs(b_thetaiotaomicron_10_empirical),
            proportional_sfs(b_thetaiotaomicron_10_one_epoch),
            proportional_sfs(b_thetaiotaomicron_10_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 10')


## B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_10/b_uniformis_10.csv')
b_uniformis_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_10/empirical_sfs.txt'
)
b_uniformis_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_10/one_epoch_demography.txt'
)
b_uniformis_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_10/two_epoch_demography.txt'
)
b_uniformis_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_uniformis_10_empirical,
            b_uniformis_10_one_epoch,
            b_uniformis_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 10')

compare_sfs(proportional_sfs(b_uniformis_10_empirical),
            proportional_sfs(b_uniformis_10_one_epoch),
            proportional_sfs(b_uniformis_10_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 10')


## B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_10/b_vulgatus_10.csv')
b_vulgatus_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_10/empirical_sfs.txt'
)
b_vulgatus_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_10/one_epoch_demography.txt'
)
b_vulgatus_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_10/two_epoch_demography.txt'
)
b_vulgatus_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_vulgatus_10_empirical,
            b_vulgatus_10_one_epoch,
            b_vulgatus_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 10')

compare_sfs(proportional_sfs(b_vulgatus_10_empirical),
            proportional_sfs(b_vulgatus_10_one_epoch),
            proportional_sfs(b_vulgatus_10_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 10')



## B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_10/b_xylanisolvens_10.csv')
b_xylanisolvens_10_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/empirical_sfs.txt'
)
b_xylanisolvens_10_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/one_epoch_demography.txt'
)
b_xylanisolvens_10_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/two_epoch_demography.txt'
)
b_xylanisolvens_original_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(b_xylanisolvens_10_empirical,
            b_xylanisolvens_10_one_epoch,
            b_xylanisolvens_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 10')

compare_sfs(proportional_sfs(b_xylanisolvens_10_empirical),
            proportional_sfs(b_xylanisolvens_10_one_epoch),
            proportional_sfs(b_xylanisolvens_10_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 10')



## D. invisus
plot_likelihood_surface('../Analysis/qp_gut_10/d_invisus_10.csv')
d_invisus_10_empirical =  read_input_sfs(
  '../Analysis/Dialister_invisus_61905_downsampled_10/empirical_sfs.txt'
)
d_invisus_10_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_10/one_epoch_demography.txt'
)
d_invisus_10_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_10/two_epoch_demography.txt'
)
d_invisus_original_empirical = read_input_sfs_original(
  '../Analysis/Dialister_invisus_61905_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(d_invisus_10_empirical,
            d_invisus_10_one_epoch,
            d_invisus_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 10')

compare_sfs(proportional_sfs(d_invisus_10_empirical),
            proportional_sfs(d_invisus_10_one_epoch),
            proportional_sfs(d_invisus_10_two_epoch)) +
  ggtitle('D. invisus Downsampled to 10')


## E. eligens
plot_likelihood_surface('../Analysis/qp_gut_10/e_eligens_10.csv')
e_eligens_10_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_10/empirical_sfs.txt'
)
e_eligens_10_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_10/one_epoch_demography.txt'
)
e_eligens_10_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_10/two_epoch_demography.txt'
)
e_eligens_original_empirical = read_input_sfs_original(
  '../Analysis/Eubacterium_eligens_61678_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(e_eligens_10_empirical,
            e_eligens_10_one_epoch,
            e_eligens_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 10')

compare_sfs(proportional_sfs(e_eligens_10_empirical),
            proportional_sfs(e_eligens_10_one_epoch),
            proportional_sfs(e_eligens_10_two_epoch)) +
  ggtitle('E. eligens Downsampled to 10')



## E. rectale
plot_likelihood_surface('../Analysis/qp_gut_10/e_rectale_10.csv')
e_rectale_10_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_rectale_56927_downsampled_10/empirical_sfs.txt'
)
e_rectale_10_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_10/one_epoch_demography.txt'
)
e_rectale_10_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_10/two_epoch_demography.txt'
)
e_rectale_original_empirical = read_input_sfs_original(
  '../Analysis/Eubacterium_rectale_56927_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(e_rectale_10_empirical,
            e_rectale_10_one_epoch,
            e_rectale_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 10')

compare_sfs(proportional_sfs(e_rectale_10_empirical),
            proportional_sfs(e_rectale_10_one_epoch),
            proportional_sfs(e_rectale_10_two_epoch)) +
  ggtitle('E. rectale Downsampled to 10')


## F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_10/f_prausnitzii_10.csv')
f_prausnitzii_10_empirical =  read_input_sfs(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/empirical_sfs.txt'
)
f_prausnitzii_10_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/one_epoch_demography.txt'
)
f_prausnitzii_10_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/two_epoch_demography.txt'
)
f_prausnitzii_original_empirical = read_input_sfs_original(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(f_prausnitzii_10_empirical,
            f_prausnitzii_10_one_epoch,
            f_prausnitzii_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 10')

compare_sfs(proportional_sfs(f_prausnitzii_10_empirical),
            proportional_sfs(f_prausnitzii_10_one_epoch),
            proportional_sfs(f_prausnitzii_10_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 10')


## Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_10/o_sp_10.csv')
o_sp_10_empirical =  read_input_sfs(
  '../Analysis/Oscillibacter_sp_60799_downsampled_10/empirical_sfs.txt'
)
o_sp_10_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_10/one_epoch_demography.txt'
)
o_sp_10_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_10/two_epoch_demography.txt'
)
o_sp_original_empirical = read_input_sfs_original(
  '../Analysis/Oscillibacter_sp_60799_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(o_sp_10_empirical,
            o_sp_10_one_epoch,
            o_sp_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 10')

compare_sfs(proportional_sfs(o_sp_10_empirical),
            proportional_sfs(o_sp_10_one_epoch),
            proportional_sfs(o_sp_10_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 10')


## o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_10/o_splanchnicus_10.csv')
o_splanchnicus_10_empirical =  read_input_sfs(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/empirical_sfs.txt'
)
o_splanchnicus_10_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/one_epoch_demography.txt'
)
o_splanchnicus_10_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/two_epoch_demography.txt'
)
o_splanchnicus_original_empirical = read_input_sfs_original(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(o_splanchnicus_10_empirical,
            o_splanchnicus_10_one_epoch,
            o_splanchnicus_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 10')

compare_sfs(proportional_sfs(o_splanchnicus_10_empirical),
            proportional_sfs(o_splanchnicus_10_one_epoch),
            proportional_sfs(o_splanchnicus_10_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 10')


## P. copri
plot_likelihood_surface('../Analysis/qp_gut_10/p_copri_10.csv')
p_copri_10_empirical =  read_input_sfs(
  '../Analysis/Prevotella_copri_61740_downsampled_10/empirical_sfs.txt'
)
p_copri_10_one_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_10/one_epoch_demography.txt'
)
p_copri_10_two_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_10/two_epoch_demography.txt'
)
p_copri_original_empirical = read_input_sfs_original(
  '../Analysis/Prevotella_copri_61740_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(p_copri_10_empirical,
            p_copri_10_one_epoch,
            p_copri_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. copri Downsampled to 10')

compare_sfs(proportional_sfs(p_copri_10_empirical),
            proportional_sfs(p_copri_10_one_epoch),
            proportional_sfs(p_copri_10_two_epoch)) +
  ggtitle('P. copri Downsampled to 10')


## P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_10/p_distasonis_10.csv')
p_distasonis_10_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_10/empirical_sfs.txt'
)
p_distasonis_10_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_10/one_epoch_demography.txt'
)
p_distasonis_10_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_10/two_epoch_demography.txt'
)
p_distasonis_original_empirical = read_input_sfs_original(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(p_distasonis_10_empirical,
            p_distasonis_10_one_epoch,
            p_distasonis_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 10')

compare_sfs(proportional_sfs(p_distasonis_10_empirical),
            proportional_sfs(p_distasonis_10_one_epoch),
            proportional_sfs(p_distasonis_10_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 10')


## P. merdae
plot_likelihood_surface('../Analysis/qp_gut_10/p_merdae_10.csv')
p_merdae_10_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_10/empirical_sfs.txt'
)
p_merdae_10_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_10/one_epoch_demography.txt'
)
p_merdae_10_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_10/two_epoch_demography.txt'
)
p_merdae_original_empirical = read_input_sfs_original(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(p_merdae_10_empirical,
            p_merdae_10_one_epoch,
            p_merdae_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 10')

compare_sfs(proportional_sfs(p_merdae_10_empirical),
            proportional_sfs(p_merdae_10_one_epoch),
            proportional_sfs(p_merdae_10_two_epoch)) +
  ggtitle('P. merdae Downsampled to 10')


## Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_10/p_sp_10.csv')
p_sp_10_empirical =  read_input_sfs(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/empirical_sfs.txt'
)
p_sp_10_one_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/one_epoch_demography.txt'
)
p_sp_10_two_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/two_epoch_demography.txt'
)
p_sp_original_empirical = read_input_sfs_original(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(p_sp_10_empirical,
            p_sp_10_one_epoch,
            p_sp_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Phascolarctobacterium sp. Downsampled to 10')

compare_sfs(proportional_sfs(p_sp_10_empirical),
            proportional_sfs(p_sp_10_one_epoch),
            proportional_sfs(p_sp_10_two_epoch)) +
  ggtitle('Phascolarctobacterium sp. Downsampled to 10')


## R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_10/r_bicirculans_10.csv')
r_bicirculans_10_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/empirical_sfs.txt'
)
r_bicirculans_10_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/one_epoch_demography.txt'
)
r_bicirculans_10_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/two_epoch_demography.txt'
)
r_bicirculans_original_empirical = read_input_sfs_original(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(r_bicirculans_10_empirical,
            r_bicirculans_10_one_epoch,
            r_bicirculans_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 10')

compare_sfs(proportional_sfs(r_bicirculans_10_empirical),
            proportional_sfs(r_bicirculans_10_one_epoch),
            proportional_sfs(r_bicirculans_10_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 10')

## R. bromii
plot_likelihood_surface('../Analysis/qp_gut_10/r_bromii_10.csv')
r_bromii_10_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_10/empirical_sfs.txt'
)
r_bromii_10_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_10/one_epoch_demography.txt'
)
r_bromii_10_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_10/two_epoch_demography.txt'
)
r_bromii_original_empirical = read_input_sfs_original(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_10/original_empirical_sfs.txt'
)
compare_sfs(r_bromii_10_empirical,
            r_bromii_10_one_epoch,
            r_bromii_10_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 10')

compare_sfs(proportional_sfs(r_bromii_10_empirical),
            proportional_sfs(r_bromii_10_one_epoch),
            proportional_sfs(r_bromii_10_two_epoch)) +
  ggtitle('R. bromii Downsampled to 10')

# Downsampled to 12

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_12/a_finegoldii_12.csv')
a_finegoldii_12_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_12/empirical_sfs.txt'
)
a_finegoldii_12_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_12/one_epoch_demography.txt'
)
a_finegoldii_12_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_12/two_epoch_demography.txt'
)

compare_sfs(a_finegoldii_12_empirical,
            a_finegoldii_12_one_epoch,
            a_finegoldii_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 12')

compare_sfs(proportional_sfs(a_finegoldii_12_empirical),
            proportional_sfs(a_finegoldii_12_one_epoch),
            proportional_sfs(a_finegoldii_12_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 12')

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_12/a_muciniphila_12.csv')
a_muciniphila_12_empirical =  read_input_sfs(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_12/empirical_sfs.txt'
)
a_muciniphila_12_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_12/one_epoch_demography.txt'
)
a_muciniphila_12_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_12_empirical,
            a_muciniphila_12_one_epoch,
            a_muciniphila_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 12')

compare_sfs(proportional_sfs(a_muciniphila_12_empirical),
            proportional_sfs(a_muciniphila_12_one_epoch),
            proportional_sfs(a_muciniphila_12_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 12')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_12/a_onderdonkii_12.csv')
a_onderdonkii_12_empirical =  read_input_sfs(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_12/empirical_sfs.txt'
)
a_onderdonkii_12_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_12/one_epoch_demography.txt'
)
a_onderdonkii_12_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_12/two_epoch_demography.txt'
)

compare_sfs(a_onderdonkii_12_empirical,
            a_onderdonkii_12_one_epoch,
            a_onderdonkii_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 12')

compare_sfs(proportional_sfs(a_onderdonkii_12_empirical),
            proportional_sfs(a_onderdonkii_12_one_epoch),
            proportional_sfs(a_onderdonkii_12_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 12')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_12/a_putredinis_12.csv')
a_putredinis_12_empirical =  read_input_sfs(
  '../Analysis/Alistipes_putredinis_61533_downsampled_12/empirical_sfs.txt'
)
a_putredinis_12_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_12/one_epoch_demography.txt'
)
a_putredinis_12_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_12_empirical,
            a_putredinis_12_one_epoch,
            a_putredinis_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 12')

compare_sfs(proportional_sfs(a_putredinis_12_empirical),
            proportional_sfs(a_putredinis_12_one_epoch),
            proportional_sfs(a_putredinis_12_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 12')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_12/a_shahii_12.csv')
a_shahii_12_empirical =  read_input_sfs(
  '../Analysis/Alistipes_shahii_62199_downsampled_12/empirical_sfs.txt'
)
a_shahii_12_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_12/one_epoch_demography.txt'
)
a_shahii_12_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(a_shahii_12_empirical,
            a_shahii_12_one_epoch,
            a_shahii_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 12')

compare_sfs(proportional_sfs(a_shahii_12_empirical),
            proportional_sfs(a_shahii_12_one_epoch),
            proportional_sfs(a_shahii_12_two_epoch)) +
  ggtitle('A. shahii Downsampled to 12')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_12/b_bacterium_12.csv')
b_bacterium_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_12/empirical_sfs.txt'
)
b_bacterium_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_12/one_epoch_demography.txt'
)
b_bacterium_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_12_empirical,
            b_bacterium_12_one_epoch,
            b_bacterium_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 12')

compare_sfs(proportional_sfs(b_bacterium_12_empirical),
            proportional_sfs(b_bacterium_12_one_epoch),
            proportional_sfs(b_bacterium_12_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 12')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_12/b_caccae_12.csv')
b_caccae_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_caccae_53434_downsampled_12/empirical_sfs.txt'
)
b_caccae_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_12/one_epoch_demography.txt'
)
b_caccae_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_caccae_12_empirical,
            b_caccae_12_one_epoch,
            b_caccae_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 12')

compare_sfs(proportional_sfs(b_caccae_12_empirical),
            proportional_sfs(b_caccae_12_one_epoch),
            proportional_sfs(b_caccae_12_two_epoch)) +
  ggtitle('B. caccae Downsampled to 12')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_12/b_cellulosilyticus_12.csv')
b_cellulosilyticus_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/empirical_sfs.txt'
)
b_cellulosilyticus_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/one_epoch_demography.txt'
)
b_cellulosilyticus_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_12_empirical,
            b_cellulosilyticus_12_one_epoch,
            b_cellulosilyticus_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 12')

compare_sfs(proportional_sfs(b_cellulosilyticus_12_empirical),
            proportional_sfs(b_cellulosilyticus_12_one_epoch),
            proportional_sfs(b_cellulosilyticus_12_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 12')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_12/b_fragilis_12.csv')
b_fragilis_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_12/empirical_sfs.txt'
)
b_fragilis_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_12/one_epoch_demography.txt'
)
b_fragilis_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_12_empirical,
            b_fragilis_12_one_epoch,
            b_fragilis_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 12')

compare_sfs(proportional_sfs(b_fragilis_12_empirical),
            proportional_sfs(b_fragilis_12_one_epoch),
            proportional_sfs(b_fragilis_12_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 12')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_12/b_intestinihominis_12.csv')
b_intestinihominis_12_empirical =  read_input_sfs(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/empirical_sfs.txt'
)
b_intestinihominis_12_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/one_epoch_demography.txt'
)
b_intestinihominis_12_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_12_empirical,
            b_intestinihominis_12_one_epoch,
            b_intestinihominis_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 12')

compare_sfs(proportional_sfs(b_intestinihominis_12_empirical),
            proportional_sfs(b_intestinihominis_12_one_epoch),
            proportional_sfs(b_intestinihominis_12_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 12')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_12/b_ovatus_12.csv')
b_ovatus_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_12/empirical_sfs.txt'
)
b_ovatus_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_12/one_epoch_demography.txt'
)
b_ovatus_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_12_empirical,
            b_ovatus_12_one_epoch,
            b_ovatus_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 12')

compare_sfs(proportional_sfs(b_ovatus_12_empirical),
            proportional_sfs(b_ovatus_12_one_epoch),
            proportional_sfs(b_ovatus_12_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 12')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_12/b_thetaiotaomicron_12.csv')
b_thetaiotaomicron_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/empirical_sfs.txt'
)
b_thetaiotaomicron_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/one_epoch_demography.txt'
)
b_thetaiotaomicron_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_12_empirical,
            b_thetaiotaomicron_12_one_epoch,
            b_thetaiotaomicron_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 12')

compare_sfs(proportional_sfs(b_thetaiotaomicron_12_empirical),
            proportional_sfs(b_thetaiotaomicron_12_one_epoch),
            proportional_sfs(b_thetaiotaomicron_12_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 12')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_12/b_uniformis_12.csv')
b_uniformis_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_12/empirical_sfs.txt'
)
b_uniformis_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_12/one_epoch_demography.txt'
)
b_uniformis_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_12_empirical,
            b_uniformis_12_one_epoch,
            b_uniformis_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 12')

compare_sfs(proportional_sfs(b_uniformis_12_empirical),
            proportional_sfs(b_uniformis_12_one_epoch),
            proportional_sfs(b_uniformis_12_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 12')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_12/b_vulgatus_12.csv')
b_vulgatus_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_12/empirical_sfs.txt'
)
b_vulgatus_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_12/one_epoch_demography.txt'
)
b_vulgatus_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_12_empirical,
            b_vulgatus_12_one_epoch,
            b_vulgatus_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 12')

compare_sfs(proportional_sfs(b_vulgatus_12_empirical),
            proportional_sfs(b_vulgatus_12_one_epoch),
            proportional_sfs(b_vulgatus_12_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 12')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_12/b_xylanisolvens_12.csv')
b_xylanisolvens_12_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/empirical_sfs.txt'
)
b_xylanisolvens_12_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/one_epoch_demography.txt'
)
b_xylanisolvens_12_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_12_empirical,
            b_xylanisolvens_12_one_epoch,
            b_xylanisolvens_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 12')

compare_sfs(proportional_sfs(b_xylanisolvens_12_empirical),
            proportional_sfs(b_xylanisolvens_12_one_epoch),
            proportional_sfs(b_xylanisolvens_12_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 12')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_12/d_invisus_12.csv')
d_invisus_12_empirical =  read_input_sfs(
  '../Analysis/Dialister_invisus_61905_downsampled_12/empirical_sfs.txt'
)
d_invisus_12_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_12/one_epoch_demography.txt'
)
d_invisus_12_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(d_invisus_12_empirical,
            d_invisus_12_one_epoch,
            d_invisus_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 12')

compare_sfs(proportional_sfs(d_invisus_12_empirical),
            proportional_sfs(d_invisus_12_one_epoch),
            proportional_sfs(d_invisus_12_two_epoch)) +
  ggtitle('D. invisus Downsampled to 12')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_12/e_eligens_12.csv')
e_eligens_12_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_12/empirical_sfs.txt'
)
e_eligens_12_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_12/one_epoch_demography.txt'
)
e_eligens_12_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(e_eligens_12_empirical,
            e_eligens_12_one_epoch,
            e_eligens_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 12')

compare_sfs(proportional_sfs(e_eligens_12_empirical),
            proportional_sfs(e_eligens_12_one_epoch),
            proportional_sfs(e_eligens_12_two_epoch)) +
  ggtitle('E. eligens Downsampled to 12')



# E. rectale
plot_likelihood_surface('../Analysis/qp_gut_12/e_rectale_12.csv')
e_rectale_12_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_rectale_56927_downsampled_12/empirical_sfs.txt'
)
e_rectale_12_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_12/one_epoch_demography.txt'
)
e_rectale_12_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(e_rectale_12_empirical,
            e_rectale_12_one_epoch,
            e_rectale_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 12')

compare_sfs(proportional_sfs(e_rectale_12_empirical),
            proportional_sfs(e_rectale_12_one_epoch),
            proportional_sfs(e_rectale_12_two_epoch)) +
  ggtitle('E. rectale Downsampled to 12')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_12/f_prausnitzii_12.csv')
f_prausnitzii_12_empirical =  read_input_sfs(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/empirical_sfs.txt'
)
f_prausnitzii_12_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/one_epoch_demography.txt'
)
f_prausnitzii_12_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_12_empirical,
            f_prausnitzii_12_one_epoch,
            f_prausnitzii_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 12')

compare_sfs(proportional_sfs(f_prausnitzii_12_empirical),
            proportional_sfs(f_prausnitzii_12_one_epoch),
            proportional_sfs(f_prausnitzii_12_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 12')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_12/o_sp_12.csv')
o_sp_12_empirical =  read_input_sfs(
  '../Analysis/Oscillibacter_sp_60799_downsampled_12/empirical_sfs.txt'
)
o_sp_12_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_12/one_epoch_demography.txt'
)
o_sp_12_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(o_sp_12_empirical,
            o_sp_12_one_epoch,
            o_sp_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 12')

compare_sfs(proportional_sfs(o_sp_12_empirical),
            proportional_sfs(o_sp_12_one_epoch),
            proportional_sfs(o_sp_12_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 12')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_12/o_splanchnicus_12.csv')
o_splanchnicus_12_empirical =  read_input_sfs(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/empirical_sfs.txt'
)
o_splanchnicus_12_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/one_epoch_demography.txt'
)
o_splanchnicus_12_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_12_empirical,
            o_splanchnicus_12_one_epoch,
            o_splanchnicus_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 12')

compare_sfs(proportional_sfs(o_splanchnicus_12_empirical),
            proportional_sfs(o_splanchnicus_12_one_epoch),
            proportional_sfs(o_splanchnicus_12_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 12')


# P. copri
plot_likelihood_surface('../Analysis/qp_gut_12/p_copri_12.csv')
p_copri_12_empirical =  read_input_sfs(
  '../Analysis/Prevotella_copri_61740_downsampled_12/empirical_sfs.txt'
)
p_copri_12_one_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_12/one_epoch_demography.txt'
)
p_copri_12_two_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(p_copri_12_empirical,
            p_copri_12_one_epoch,
            p_copri_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. copri Downsampled to 12')

compare_sfs(proportional_sfs(p_copri_12_empirical),
            proportional_sfs(p_copri_12_one_epoch),
            proportional_sfs(p_copri_12_two_epoch)) +
  ggtitle('P. copri Downsampled to 12')


# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_12/p_distasonis_12.csv')
p_distasonis_12_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_12/empirical_sfs.txt'
)
p_distasonis_12_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_12/one_epoch_demography.txt'
)
p_distasonis_12_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_12_empirical,
            p_distasonis_12_one_epoch,
            p_distasonis_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 12')

compare_sfs(proportional_sfs(p_distasonis_12_empirical),
            proportional_sfs(p_distasonis_12_one_epoch),
            proportional_sfs(p_distasonis_12_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 12')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_12/p_merdae_12.csv')
p_merdae_12_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_12/empirical_sfs.txt'
)
p_merdae_12_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_12/one_epoch_demography.txt'
)
p_merdae_12_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(p_merdae_12_empirical,
            p_merdae_12_one_epoch,
            p_merdae_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 12')

compare_sfs(proportional_sfs(p_merdae_12_empirical),
            proportional_sfs(p_merdae_12_one_epoch),
            proportional_sfs(p_merdae_12_two_epoch)) +
  ggtitle('P. merdae Downsampled to 12')


# Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_12/p_sp_12.csv')
p_sp_12_empirical =  read_input_sfs(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/empirical_sfs.txt'
)
p_sp_12_one_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/one_epoch_demography.txt'
)
p_sp_12_two_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(p_sp_12_empirical,
            p_sp_12_one_epoch,
            p_sp_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Phascolarctobacterium sp. Downsampled to 12')

compare_sfs(proportional_sfs(p_sp_12_empirical),
            proportional_sfs(p_sp_12_one_epoch),
            proportional_sfs(p_sp_12_two_epoch)) +
  ggtitle('Phascolarctobacterium sp. Downsampled to 12')


# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_12/r_bicirculans_12.csv')
r_bicirculans_12_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/empirical_sfs.txt'
)
r_bicirculans_12_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/one_epoch_demography.txt'
)
r_bicirculans_12_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_12_empirical,
            r_bicirculans_12_one_epoch,
            r_bicirculans_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 12')

compare_sfs(proportional_sfs(r_bicirculans_12_empirical),
            proportional_sfs(r_bicirculans_12_one_epoch),
            proportional_sfs(r_bicirculans_12_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 12')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_12/r_bromii_12.csv')
r_bromii_12_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_12/empirical_sfs.txt'
)
r_bromii_12_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_12/one_epoch_demography.txt'
)
r_bromii_12_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_12/two_epoch_demography.txt'
)
compare_sfs(r_bromii_12_empirical,
            r_bromii_12_one_epoch,
            r_bromii_12_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 12')

compare_sfs(proportional_sfs(r_bromii_12_empirical),
            proportional_sfs(r_bromii_12_one_epoch),
            proportional_sfs(r_bromii_12_two_epoch)) +
  ggtitle('R. bromii Downsampled to 12')

# Downsampled to 14

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_14/a_finegoldii_14.csv')
a_finegoldii_14_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_sfs.txt'
)
a_finegoldii_14_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/one_epoch_demography.txt'
)
a_finegoldii_14_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/two_epoch_demography.txt'
)

compare_sfs(a_finegoldii_14_empirical,
            a_finegoldii_14_one_epoch,
            a_finegoldii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 14')

compare_sfs(proportional_sfs(a_finegoldii_14_empirical),
            proportional_sfs(a_finegoldii_14_one_epoch),
            proportional_sfs(a_finegoldii_14_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 14')

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_14/a_muciniphila_14.csv')
a_muciniphila_14_empirical = read_input_sfs_original(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_sfs.txt'
)
a_muciniphila_14_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/one_epoch_demography.txt'
)
a_muciniphila_14_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_14_empirical,
            a_muciniphila_14_one_epoch,
            a_muciniphila_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 14')

compare_sfs(proportional_sfs(a_muciniphila_14_empirical),
            proportional_sfs(a_muciniphila_14_one_epoch),
            proportional_sfs(a_muciniphila_14_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 14')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_14/a_onderdonkii_14.csv')
a_onderdonkii_14_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_sfs.txt'
)
a_onderdonkii_14_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/one_epoch_demography.txt'
)
a_onderdonkii_14_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/two_epoch_demography.txt'
)

compare_sfs(a_onderdonkii_14_empirical,
            a_onderdonkii_14_one_epoch,
            a_onderdonkii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 14')

compare_sfs(proportional_sfs(a_onderdonkii_14_empirical),
            proportional_sfs(a_onderdonkii_14_one_epoch),
            proportional_sfs(a_onderdonkii_14_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 14')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_14/a_putredinis_14.csv')
a_putredinis_14_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_sfs.txt'
)
a_putredinis_14_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/one_epoch_demography.txt'
)
a_putredinis_14_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_14_empirical,
            a_putredinis_14_one_epoch,
            a_putredinis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 14')

compare_sfs(proportional_sfs(a_putredinis_14_empirical),
            proportional_sfs(a_putredinis_14_one_epoch),
            proportional_sfs(a_putredinis_14_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 14')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_14/a_shahii_14.csv')
a_shahii_14_empirical = read_input_sfs_original(
  '../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_sfs.txt'
)
a_shahii_14_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_14/one_epoch_demography.txt'
)
a_shahii_14_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_shahii_14_empirical,
            a_shahii_14_one_epoch,
            a_shahii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 14')

compare_sfs(proportional_sfs(a_shahii_14_empirical),
            proportional_sfs(a_shahii_14_one_epoch),
            proportional_sfs(a_shahii_14_two_epoch)) +
  ggtitle('A. shahii Downsampled to 14')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_14/b_bacterium_14.csv')
b_bacterium_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_sfs.txt'
)
b_bacterium_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_14/one_epoch_demography.txt'
)
b_bacterium_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_14_empirical,
            b_bacterium_14_one_epoch,
            b_bacterium_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 14')

compare_sfs(proportional_sfs(b_bacterium_14_empirical),
            proportional_sfs(b_bacterium_14_one_epoch),
            proportional_sfs(b_bacterium_14_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 14')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_14/b_caccae_14.csv')
b_caccae_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_sfs.txt'
)
b_caccae_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/one_epoch_demography.txt'
)
b_caccae_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_caccae_14_empirical,
            b_caccae_14_one_epoch,
            b_caccae_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 14')

compare_sfs(proportional_sfs(b_caccae_14_empirical),
            proportional_sfs(b_caccae_14_one_epoch),
            proportional_sfs(b_caccae_14_two_epoch)) +
  ggtitle('B. caccae Downsampled to 14')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_14/b_cellulosilyticus_14.csv')
b_cellulosilyticus_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_sfs.txt'
)
b_cellulosilyticus_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/one_epoch_demography.txt'
)
b_cellulosilyticus_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_14_empirical,
            b_cellulosilyticus_14_one_epoch,
            b_cellulosilyticus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 14')

compare_sfs(proportional_sfs(b_cellulosilyticus_14_empirical),
            proportional_sfs(b_cellulosilyticus_14_one_epoch),
            proportional_sfs(b_cellulosilyticus_14_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 14')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_14/b_fragilis_14.csv')
b_fragilis_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_sfs.txt'
)
b_fragilis_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/one_epoch_demography.txt'
)
b_fragilis_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_14_empirical,
            b_fragilis_14_one_epoch,
            b_fragilis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 14')

compare_sfs(proportional_sfs(b_fragilis_14_empirical),
            proportional_sfs(b_fragilis_14_one_epoch),
            proportional_sfs(b_fragilis_14_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 14')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_14/b_intestinihominis_14.csv')
b_intestinihominis_14_empirical = read_input_sfs_original(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_sfs.txt'
)
b_intestinihominis_14_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/one_epoch_demography.txt'
)
b_intestinihominis_14_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_14_empirical,
            b_intestinihominis_14_one_epoch,
            b_intestinihominis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 14')

compare_sfs(proportional_sfs(b_intestinihominis_14_empirical),
            proportional_sfs(b_intestinihominis_14_one_epoch),
            proportional_sfs(b_intestinihominis_14_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 14')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_14/b_ovatus_14.csv')
b_ovatus_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_sfs.txt'
)
b_ovatus_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/one_epoch_demography.txt'
)
b_ovatus_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_14_empirical,
            b_ovatus_14_one_epoch,
            b_ovatus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 14')

compare_sfs(proportional_sfs(b_ovatus_14_empirical),
            proportional_sfs(b_ovatus_14_one_epoch),
            proportional_sfs(b_ovatus_14_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 14')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_14/b_thetaiotaomicron_14.csv')
b_thetaiotaomicron_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_sfs.txt'
)
b_thetaiotaomicron_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/one_epoch_demography.txt'
)
b_thetaiotaomicron_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_14_empirical,
            b_thetaiotaomicron_14_one_epoch,
            b_thetaiotaomicron_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 14')

compare_sfs(proportional_sfs(b_thetaiotaomicron_14_empirical),
            proportional_sfs(b_thetaiotaomicron_14_one_epoch),
            proportional_sfs(b_thetaiotaomicron_14_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 14')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_14/b_uniformis_14.csv')
b_uniformis_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_sfs.txt'
)
b_uniformis_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_14/one_epoch_demography.txt'
)
b_uniformis_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_14_empirical,
            b_uniformis_14_one_epoch,
            b_uniformis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 14')

compare_sfs(proportional_sfs(b_uniformis_14_empirical),
            proportional_sfs(b_uniformis_14_one_epoch),
            proportional_sfs(b_uniformis_14_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 14')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_14/b_vulgatus_14.csv')
b_vulgatus_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_sfs.txt'
)
b_vulgatus_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/one_epoch_demography.txt'
)
b_vulgatus_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_14_empirical,
            b_vulgatus_14_one_epoch,
            b_vulgatus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 14')

compare_sfs(proportional_sfs(b_vulgatus_14_empirical),
            proportional_sfs(b_vulgatus_14_one_epoch),
            proportional_sfs(b_vulgatus_14_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 14')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_14/b_xylanisolvens_14.csv')
b_xylanisolvens_14_empirical = read_input_sfs_original(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_sfs.txt'
)
b_xylanisolvens_14_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/one_epoch_demography.txt'
)
b_xylanisolvens_14_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_14_empirical,
            b_xylanisolvens_14_one_epoch,
            b_xylanisolvens_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 14')

compare_sfs(proportional_sfs(b_xylanisolvens_14_empirical),
            proportional_sfs(b_xylanisolvens_14_one_epoch),
            proportional_sfs(b_xylanisolvens_14_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 14')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_14/d_invisus_14.csv')
d_invisus_14_empirical = read_input_sfs_original(
  '../Analysis/Dialister_invisus_61905_downsampled_14/empirical_sfs.txt'
)
d_invisus_14_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_14/one_epoch_demography.txt'
)
d_invisus_14_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(d_invisus_14_empirical,
            d_invisus_14_one_epoch,
            d_invisus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 14')

compare_sfs(proportional_sfs(d_invisus_14_empirical),
            proportional_sfs(d_invisus_14_one_epoch),
            proportional_sfs(d_invisus_14_two_epoch)) +
  ggtitle('D. invisus Downsampled to 14')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_14/e_eligens_14.csv')
e_eligens_14_empirical = read_input_sfs_original(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
e_eligens_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
e_eligens_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(e_eligens_14_empirical,
            e_eligens_14_one_epoch,
            e_eligens_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(e_eligens_14_empirical),
            proportional_sfs(e_eligens_14_one_epoch),
            proportional_sfs(e_eligens_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')



# E. rectale
plot_likelihood_surface('../Analysis/qp_gut_14/e_rectale_14.csv')
e_rectale_14_empirical = read_input_sfs_original(
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_sfs.txt'
)
e_rectale_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/one_epoch_demography.txt'
)
e_rectale_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(e_rectale_14_empirical,
            e_rectale_14_one_epoch,
            e_rectale_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 14')

compare_sfs(proportional_sfs(e_rectale_14_empirical),
            proportional_sfs(e_rectale_14_one_epoch),
            proportional_sfs(e_rectale_14_two_epoch)) +
  ggtitle('E. rectale Downsampled to 14')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_14/f_prausnitzii_14.csv')
f_prausnitzii_14_empirical = read_input_sfs_original(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_sfs.txt'
)
f_prausnitzii_14_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/one_epoch_demography.txt'
)
f_prausnitzii_14_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_14_empirical,
            f_prausnitzii_14_one_epoch,
            f_prausnitzii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 14')

compare_sfs(proportional_sfs(f_prausnitzii_14_empirical),
            proportional_sfs(f_prausnitzii_14_one_epoch),
            proportional_sfs(f_prausnitzii_14_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 14')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_14/o_sp_14.csv')
o_sp_14_empirical = read_input_sfs_original(
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_sfs.txt'
)
o_sp_14_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/one_epoch_demography.txt'
)
o_sp_14_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(o_sp_14_empirical,
            o_sp_14_one_epoch,
            o_sp_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 14')

compare_sfs(proportional_sfs(o_sp_14_empirical),
            proportional_sfs(o_sp_14_one_epoch),
            proportional_sfs(o_sp_14_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 14')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_14/o_splanchnicus_14.csv')
o_splanchnicus_14_empirical = read_input_sfs_original(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_sfs.txt'
)
o_splanchnicus_14_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/one_epoch_demography.txt'
)
o_splanchnicus_14_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_14_empirical,
            o_splanchnicus_14_one_epoch,
            o_splanchnicus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 14')

compare_sfs(proportional_sfs(o_splanchnicus_14_empirical),
            proportional_sfs(o_splanchnicus_14_one_epoch),
            proportional_sfs(o_splanchnicus_14_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 14')


# P. copri
plot_likelihood_surface('../Analysis/qp_gut_14/p_copri_14.csv')
p_copri_14_empirical = read_input_sfs_original(
  '../Analysis/Prevotella_copri_61740_downsampled_14/empirical_sfs.txt'
)
p_copri_14_one_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_14/one_epoch_demography.txt'
)
p_copri_14_two_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_copri_14_empirical,
            p_copri_14_one_epoch,
            p_copri_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. copri Downsampled to 14')

compare_sfs(proportional_sfs(p_copri_14_empirical),
            proportional_sfs(p_copri_14_one_epoch),
            proportional_sfs(p_copri_14_two_epoch)) +
  ggtitle('P. copri Downsampled to 14')


# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_14/p_distasonis_14.csv')
p_distasonis_14_empirical = read_input_sfs_original(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_sfs.txt'
)
p_distasonis_14_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/one_epoch_demography.txt'
)
p_distasonis_14_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_14_empirical,
            p_distasonis_14_one_epoch,
            p_distasonis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 14')

compare_sfs(proportional_sfs(p_distasonis_14_empirical),
            proportional_sfs(p_distasonis_14_one_epoch),
            proportional_sfs(p_distasonis_14_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 14')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_14/p_merdae_14.csv')
p_merdae_14_empirical = read_input_sfs_original(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_sfs.txt'
)
p_merdae_14_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/one_epoch_demography.txt'
)
p_merdae_14_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_merdae_14_empirical,
            p_merdae_14_one_epoch,
            p_merdae_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 14')

compare_sfs(proportional_sfs(p_merdae_14_empirical),
            proportional_sfs(p_merdae_14_one_epoch),
            proportional_sfs(p_merdae_14_two_epoch)) +
  ggtitle('P. merdae Downsampled to 14')


# Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_14/p_sp_14.csv')
p_sp_14_empirical = read_input_sfs_original(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_sfs.txt'
)
p_sp_14_one_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/one_epoch_demography.txt'
)
p_sp_14_two_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_sp_14_empirical,
            p_sp_14_one_epoch,
            p_sp_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Phascolarctobacterium sp. Downsampled to 14')

compare_sfs(proportional_sfs(p_sp_14_empirical),
            proportional_sfs(p_sp_14_one_epoch),
            proportional_sfs(p_sp_14_two_epoch)) +
  ggtitle('Phascolarctobacterium sp. Downsampled to 14')


# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_14/r_bicirculans_14.csv')
r_bicirculans_14_empirical = read_input_sfs_original(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_sfs.txt'
)
r_bicirculans_14_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/one_epoch_demography.txt'
)
r_bicirculans_14_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_14_empirical,
            r_bicirculans_14_one_epoch,
            r_bicirculans_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 14')

compare_sfs(proportional_sfs(r_bicirculans_14_empirical),
            proportional_sfs(r_bicirculans_14_one_epoch),
            proportional_sfs(r_bicirculans_14_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 14')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_14/r_bromii_14.csv')
r_bromii_14_empirical = read_input_sfs_original(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_sfs.txt'
)
r_bromii_14_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/one_epoch_demography.txt'
)
r_bromii_14_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(r_bromii_14_empirical,
            r_bromii_14_one_epoch,
            r_bromii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 14')

compare_sfs(proportional_sfs(r_bromii_14_empirical),
            proportional_sfs(r_bromii_14_one_epoch),
            proportional_sfs(r_bromii_14_two_epoch)) +
  ggtitle('R. bromii Downsampled to 14')


# Downsampled to 16

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_16/a_finegoldii_16.csv')
a_finegoldii_16_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_16/empirical_sfs.txt'
)
a_finegoldii_16_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_16/one_epoch_demography.txt'
)
a_finegoldii_16_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_16/two_epoch_demography.txt'
)

compare_sfs(a_finegoldii_16_empirical,
            a_finegoldii_16_one_epoch,
            a_finegoldii_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 16')

compare_sfs(proportional_sfs(a_finegoldii_16_empirical),
            proportional_sfs(a_finegoldii_16_one_epoch),
            proportional_sfs(a_finegoldii_16_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 16')

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_16/a_muciniphila_16.csv')
a_muciniphila_16_empirical =  read_input_sfs(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_16/empirical_sfs.txt'
)
a_muciniphila_16_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_16/one_epoch_demography.txt'
)
a_muciniphila_16_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_16_empirical,
            a_muciniphila_16_one_epoch,
            a_muciniphila_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 16')

compare_sfs(proportional_sfs(a_muciniphila_16_empirical),
            proportional_sfs(a_muciniphila_16_one_epoch),
            proportional_sfs(a_muciniphila_16_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 16')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_16/a_onderdonkii_16.csv')
a_onderdonkii_16_empirical =  read_input_sfs(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_16/empirical_sfs.txt'
)
a_onderdonkii_16_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_16/one_epoch_demography.txt'
)
a_onderdonkii_16_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_16/two_epoch_demography.txt'
)

compare_sfs(a_onderdonkii_16_empirical,
            a_onderdonkii_16_one_epoch,
            a_onderdonkii_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 16')

compare_sfs(proportional_sfs(a_onderdonkii_16_empirical),
            proportional_sfs(a_onderdonkii_16_one_epoch),
            proportional_sfs(a_onderdonkii_16_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 16')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_16/a_putredinis_16.csv')
a_putredinis_16_empirical =  read_input_sfs(
  '../Analysis/Alistipes_putredinis_61533_downsampled_16/empirical_sfs.txt'
)
a_putredinis_16_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_16/one_epoch_demography.txt'
)
a_putredinis_16_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_16_empirical,
            a_putredinis_16_one_epoch,
            a_putredinis_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 16')

compare_sfs(proportional_sfs(a_putredinis_16_empirical),
            proportional_sfs(a_putredinis_16_one_epoch),
            proportional_sfs(a_putredinis_16_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 16')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_16/a_shahii_16.csv')
a_shahii_16_empirical =  read_input_sfs(
  '../Analysis/Alistipes_shahii_62199_downsampled_16/empirical_sfs.txt'
)
a_shahii_16_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_16/one_epoch_demography.txt'
)
a_shahii_16_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(a_shahii_16_empirical,
            a_shahii_16_one_epoch,
            a_shahii_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 16')

compare_sfs(proportional_sfs(a_shahii_16_empirical),
            proportional_sfs(a_shahii_16_one_epoch),
            proportional_sfs(a_shahii_16_two_epoch)) +
  ggtitle('A. shahii Downsampled to 16')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_16/b_bacterium_16.csv')
b_bacterium_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_16/empirical_sfs.txt'
)
b_bacterium_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_16/one_epoch_demography.txt'
)
b_bacterium_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_16_empirical,
            b_bacterium_16_one_epoch,
            b_bacterium_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 16')

compare_sfs(proportional_sfs(b_bacterium_16_empirical),
            proportional_sfs(b_bacterium_16_one_epoch),
            proportional_sfs(b_bacterium_16_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 16')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_16/b_caccae_16.csv')
b_caccae_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_caccae_53434_downsampled_16/empirical_sfs.txt'
)
b_caccae_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_16/one_epoch_demography.txt'
)
b_caccae_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_caccae_16_empirical,
            b_caccae_16_one_epoch,
            b_caccae_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 16')

compare_sfs(proportional_sfs(b_caccae_16_empirical),
            proportional_sfs(b_caccae_16_one_epoch),
            proportional_sfs(b_caccae_16_two_epoch)) +
  ggtitle('B. caccae Downsampled to 16')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_16/b_cellulosilyticus_16.csv')
b_cellulosilyticus_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/empirical_sfs.txt'
)
b_cellulosilyticus_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/one_epoch_demography.txt'
)
b_cellulosilyticus_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_16_empirical,
            b_cellulosilyticus_16_one_epoch,
            b_cellulosilyticus_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 16')

compare_sfs(proportional_sfs(b_cellulosilyticus_16_empirical),
            proportional_sfs(b_cellulosilyticus_16_one_epoch),
            proportional_sfs(b_cellulosilyticus_16_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 16')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_16/b_fragilis_16.csv')
b_fragilis_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_16/empirical_sfs.txt'
)
b_fragilis_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_16/one_epoch_demography.txt'
)
b_fragilis_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_16_empirical,
            b_fragilis_16_one_epoch,
            b_fragilis_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 16')

compare_sfs(proportional_sfs(b_fragilis_16_empirical),
            proportional_sfs(b_fragilis_16_one_epoch),
            proportional_sfs(b_fragilis_16_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 16')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_16/b_intestinihominis_16.csv')
b_intestinihominis_16_empirical =  read_input_sfs(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/empirical_sfs.txt'
)
b_intestinihominis_16_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/one_epoch_demography.txt'
)
b_intestinihominis_16_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_16_empirical,
            b_intestinihominis_16_one_epoch,
            b_intestinihominis_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 16')

compare_sfs(proportional_sfs(b_intestinihominis_16_empirical),
            proportional_sfs(b_intestinihominis_16_one_epoch),
            proportional_sfs(b_intestinihominis_16_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 16')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_16/b_ovatus_16.csv')
b_ovatus_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_16/empirical_sfs.txt'
)
b_ovatus_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_16/one_epoch_demography.txt'
)
b_ovatus_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_16_empirical,
            b_ovatus_16_one_epoch,
            b_ovatus_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 16')

compare_sfs(proportional_sfs(b_ovatus_16_empirical),
            proportional_sfs(b_ovatus_16_one_epoch),
            proportional_sfs(b_ovatus_16_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 16')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_16/b_thetaiotaomicron_16.csv')
b_thetaiotaomicron_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/empirical_sfs.txt'
)
b_thetaiotaomicron_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/one_epoch_demography.txt'
)
b_thetaiotaomicron_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_16_empirical,
            b_thetaiotaomicron_16_one_epoch,
            b_thetaiotaomicron_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 16')

compare_sfs(proportional_sfs(b_thetaiotaomicron_16_empirical),
            proportional_sfs(b_thetaiotaomicron_16_one_epoch),
            proportional_sfs(b_thetaiotaomicron_16_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 16')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_16/b_uniformis_16.csv')
b_uniformis_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_16/empirical_sfs.txt'
)
b_uniformis_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_16/one_epoch_demography.txt'
)
b_uniformis_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_16_empirical,
            b_uniformis_16_one_epoch,
            b_uniformis_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 16')

compare_sfs(proportional_sfs(b_uniformis_16_empirical),
            proportional_sfs(b_uniformis_16_one_epoch),
            proportional_sfs(b_uniformis_16_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 16')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_16/b_vulgatus_16.csv')
b_vulgatus_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_16/empirical_sfs.txt'
)
b_vulgatus_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_16/one_epoch_demography.txt'
)
b_vulgatus_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_16_empirical,
            b_vulgatus_16_one_epoch,
            b_vulgatus_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 16')

compare_sfs(proportional_sfs(b_vulgatus_16_empirical),
            proportional_sfs(b_vulgatus_16_one_epoch),
            proportional_sfs(b_vulgatus_16_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 16')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_16/b_xylanisolvens_16.csv')
b_xylanisolvens_16_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/empirical_sfs.txt'
)
b_xylanisolvens_16_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/one_epoch_demography.txt'
)
b_xylanisolvens_16_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_16_empirical,
            b_xylanisolvens_16_one_epoch,
            b_xylanisolvens_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 16')

compare_sfs(proportional_sfs(b_xylanisolvens_16_empirical),
            proportional_sfs(b_xylanisolvens_16_one_epoch),
            proportional_sfs(b_xylanisolvens_16_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 16')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_16/d_invisus_16.csv')
d_invisus_16_empirical =  read_input_sfs(
  '../Analysis/Dialister_invisus_61905_downsampled_16/empirical_sfs.txt'
)
d_invisus_16_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_16/one_epoch_demography.txt'
)
d_invisus_16_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(d_invisus_16_empirical,
            d_invisus_16_one_epoch,
            d_invisus_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 16')

compare_sfs(proportional_sfs(d_invisus_16_empirical),
            proportional_sfs(d_invisus_16_one_epoch),
            proportional_sfs(d_invisus_16_two_epoch)) +
  ggtitle('D. invisus Downsampled to 16')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_16/e_eligens_16.csv')
e_eligens_16_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_16/empirical_sfs.txt'
)
e_eligens_16_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_16/one_epoch_demography.txt'
)
e_eligens_16_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(e_eligens_16_empirical,
            e_eligens_16_one_epoch,
            e_eligens_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 16')

compare_sfs(proportional_sfs(e_eligens_16_empirical),
            proportional_sfs(e_eligens_16_one_epoch),
            proportional_sfs(e_eligens_16_two_epoch)) +
  ggtitle('E. eligens Downsampled to 16')



# E. rectale
plot_likelihood_surface('../Analysis/qp_gut_16/e_rectale_16.csv')
e_rectale_16_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_rectale_56927_downsampled_16/empirical_sfs.txt'
)
e_rectale_16_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_16/one_epoch_demography.txt'
)
e_rectale_16_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(e_rectale_16_empirical,
            e_rectale_16_one_epoch,
            e_rectale_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 16')

compare_sfs(proportional_sfs(e_rectale_16_empirical),
            proportional_sfs(e_rectale_16_one_epoch),
            proportional_sfs(e_rectale_16_two_epoch)) +
  ggtitle('E. rectale Downsampled to 16')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_16/f_prausnitzii_16.csv')
f_prausnitzii_16_empirical =  read_input_sfs(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/empirical_sfs.txt'
)
f_prausnitzii_16_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/one_epoch_demography.txt'
)
f_prausnitzii_16_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_16_empirical,
            f_prausnitzii_16_one_epoch,
            f_prausnitzii_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 16')

compare_sfs(proportional_sfs(f_prausnitzii_16_empirical),
            proportional_sfs(f_prausnitzii_16_one_epoch),
            proportional_sfs(f_prausnitzii_16_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 16')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_16/o_sp_16.csv')
o_sp_16_empirical =  read_input_sfs(
  '../Analysis/Oscillibacter_sp_60799_downsampled_16/empirical_sfs.txt'
)
o_sp_16_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_16/one_epoch_demography.txt'
)
o_sp_16_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(o_sp_16_empirical,
            o_sp_16_one_epoch,
            o_sp_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 16')

compare_sfs(proportional_sfs(o_sp_16_empirical),
            proportional_sfs(o_sp_16_one_epoch),
            proportional_sfs(o_sp_16_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 16')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_16/o_splanchnicus_16.csv')
o_splanchnicus_16_empirical =  read_input_sfs(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/empirical_sfs.txt'
)
o_splanchnicus_16_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/one_epoch_demography.txt'
)
o_splanchnicus_16_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_16_empirical,
            o_splanchnicus_16_one_epoch,
            o_splanchnicus_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 16')

compare_sfs(proportional_sfs(o_splanchnicus_16_empirical),
            proportional_sfs(o_splanchnicus_16_one_epoch),
            proportional_sfs(o_splanchnicus_16_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 16')


# P. copri
# plot_likelihood_surface('../Analysis/qp_gut_16/p_copri_16.csv')
# p_copri_16_empirical =  read_input_sfs(
#   '../Analysis/Prevotella_copri_61740_downsampled_16/empirical_sfs.txt'
# )
# p_copri_16_one_epoch = sfs_from_demography(
#   '../Analysis/Prevotella_copri_61740_downsampled_16/one_epoch_demography.txt'
# )
# p_copri_16_two_epoch = sfs_from_demography(
#   '../Analysis/Prevotella_copri_61740_downsampled_16/two_epoch_demography.txt'
# )
# compare_sfs(p_copri_16_empirical,
#             p_copri_16_one_epoch,
#             p_copri_16_two_epoch) +
#   ylab('Raw Count of Segregating Sites') +
#   ggtitle('P. copri Downsampled to 16')
# 
# compare_sfs(proportional_sfs(p_copri_16_empirical),
#             proportional_sfs(p_copri_16_one_epoch),
#             proportional_sfs(p_copri_16_two_epoch)) +
#   ggtitle('P. copri Downsampled to 16')
# 
# 
# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_16/p_distasonis_16.csv')
p_distasonis_16_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_16/empirical_sfs.txt'
)
p_distasonis_16_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_16/one_epoch_demography.txt'
)
p_distasonis_16_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_16_empirical,
            p_distasonis_16_one_epoch,
            p_distasonis_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 16')

compare_sfs(proportional_sfs(p_distasonis_16_empirical),
            proportional_sfs(p_distasonis_16_one_epoch),
            proportional_sfs(p_distasonis_16_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 16')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_16/p_merdae_16.csv')
p_merdae_16_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_16/empirical_sfs.txt'
)
p_merdae_16_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_16/one_epoch_demography.txt'
)
p_merdae_16_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(p_merdae_16_empirical,
            p_merdae_16_one_epoch,
            p_merdae_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 16')

compare_sfs(proportional_sfs(p_merdae_16_empirical),
            proportional_sfs(p_merdae_16_one_epoch),
            proportional_sfs(p_merdae_16_two_epoch)) +
  ggtitle('P. merdae Downsampled to 16')


# Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_16/p_sp_16.csv')
p_sp_16_empirical =  read_input_sfs(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/empirical_sfs.txt'
)
p_sp_16_one_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/one_epoch_demography.txt'
)
p_sp_16_two_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(p_sp_16_empirical,
            p_sp_16_one_epoch,
            p_sp_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Phascolarctobacterium sp. Downsampled to 16')

compare_sfs(proportional_sfs(p_sp_16_empirical),
            proportional_sfs(p_sp_16_one_epoch),
            proportional_sfs(p_sp_16_two_epoch)) +
  ggtitle('Phascolarctobacterium sp. Downsampled to 16')


# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_16/r_bicirculans_16.csv')
r_bicirculans_16_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/empirical_sfs.txt'
)
r_bicirculans_16_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/one_epoch_demography.txt'
)
r_bicirculans_16_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_16_empirical,
            r_bicirculans_16_one_epoch,
            r_bicirculans_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 16')

compare_sfs(proportional_sfs(r_bicirculans_16_empirical),
            proportional_sfs(r_bicirculans_16_one_epoch),
            proportional_sfs(r_bicirculans_16_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 16')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_16/r_bromii_16.csv')
r_bromii_16_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_16/empirical_sfs.txt'
)
r_bromii_16_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_16/one_epoch_demography.txt'
)
r_bromii_16_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_16/two_epoch_demography.txt'
)
compare_sfs(r_bromii_16_empirical,
            r_bromii_16_one_epoch,
            r_bromii_16_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 16')

compare_sfs(proportional_sfs(r_bromii_16_empirical),
            proportional_sfs(r_bromii_16_one_epoch),
            proportional_sfs(r_bromii_16_two_epoch)) +
  ggtitle('R. bromii Downsampled to 16')

# Downsampled to 18

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_18/a_finegoldii_18.csv')
a_finegoldii_18_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_18/empirical_sfs.txt'
)
a_finegoldii_18_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_18/one_epoch_demography.txt'
)
a_finegoldii_18_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_18/two_epoch_demography.txt'
)

compare_sfs(a_finegoldii_18_empirical,
            a_finegoldii_18_one_epoch,
            a_finegoldii_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 18')

compare_sfs(proportional_sfs(a_finegoldii_18_empirical),
            proportional_sfs(a_finegoldii_18_one_epoch),
            proportional_sfs(a_finegoldii_18_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 18')

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_18/a_muciniphila_18.csv')
a_muciniphila_18_empirical =  read_input_sfs(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_18/empirical_sfs.txt'
)
a_muciniphila_18_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_18/one_epoch_demography.txt'
)
a_muciniphila_18_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_18_empirical,
            a_muciniphila_18_one_epoch,
            a_muciniphila_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 18')

compare_sfs(proportional_sfs(a_muciniphila_18_empirical),
            proportional_sfs(a_muciniphila_18_one_epoch),
            proportional_sfs(a_muciniphila_18_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 18')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_18/a_onderdonkii_18.csv')
a_onderdonkii_18_empirical =  read_input_sfs(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_18/empirical_sfs.txt'
)
a_onderdonkii_18_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_18/one_epoch_demography.txt'
)
a_onderdonkii_18_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_18/two_epoch_demography.txt'
)

compare_sfs(a_onderdonkii_18_empirical,
            a_onderdonkii_18_one_epoch,
            a_onderdonkii_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 18')

compare_sfs(proportional_sfs(a_onderdonkii_18_empirical),
            proportional_sfs(a_onderdonkii_18_one_epoch),
            proportional_sfs(a_onderdonkii_18_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 18')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_18/a_putredinis_18.csv')
a_putredinis_18_empirical =  read_input_sfs(
  '../Analysis/Alistipes_putredinis_61533_downsampled_18/empirical_sfs.txt'
)
a_putredinis_18_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_18/one_epoch_demography.txt'
)
a_putredinis_18_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_18_empirical,
            a_putredinis_18_one_epoch,
            a_putredinis_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 18')

compare_sfs(proportional_sfs(a_putredinis_18_empirical),
            proportional_sfs(a_putredinis_18_one_epoch),
            proportional_sfs(a_putredinis_18_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 18')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_18/a_shahii_18.csv')
a_shahii_18_empirical =  read_input_sfs(
  '../Analysis/Alistipes_shahii_62199_downsampled_18/empirical_sfs.txt'
)
a_shahii_18_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_18/one_epoch_demography.txt'
)
a_shahii_18_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(a_shahii_18_empirical,
            a_shahii_18_one_epoch,
            a_shahii_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 18')

compare_sfs(proportional_sfs(a_shahii_18_empirical),
            proportional_sfs(a_shahii_18_one_epoch),
            proportional_sfs(a_shahii_18_two_epoch)) +
  ggtitle('A. shahii Downsampled to 18')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_18/b_bacterium_18.csv')
b_bacterium_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_18/empirical_sfs.txt'
)
b_bacterium_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_18/one_epoch_demography.txt'
)
b_bacterium_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_18_empirical,
            b_bacterium_18_one_epoch,
            b_bacterium_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 18')

compare_sfs(proportional_sfs(b_bacterium_18_empirical),
            proportional_sfs(b_bacterium_18_one_epoch),
            proportional_sfs(b_bacterium_18_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 18')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_18/b_caccae_18.csv')
b_caccae_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_caccae_53434_downsampled_18/empirical_sfs.txt'
)
b_caccae_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_18/one_epoch_demography.txt'
)
b_caccae_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_caccae_18_empirical,
            b_caccae_18_one_epoch,
            b_caccae_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 18')

compare_sfs(proportional_sfs(b_caccae_18_empirical),
            proportional_sfs(b_caccae_18_one_epoch),
            proportional_sfs(b_caccae_18_two_epoch)) +
  ggtitle('B. caccae Downsampled to 18')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_18/b_cellulosilyticus_18.csv')
b_cellulosilyticus_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/empirical_sfs.txt'
)
b_cellulosilyticus_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/one_epoch_demography.txt'
)
b_cellulosilyticus_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_18_empirical,
            b_cellulosilyticus_18_one_epoch,
            b_cellulosilyticus_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 18')

compare_sfs(proportional_sfs(b_cellulosilyticus_18_empirical),
            proportional_sfs(b_cellulosilyticus_18_one_epoch),
            proportional_sfs(b_cellulosilyticus_18_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 18')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_18/b_fragilis_18.csv')
b_fragilis_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_18/empirical_sfs.txt'
)
b_fragilis_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_18/one_epoch_demography.txt'
)
b_fragilis_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_18_empirical,
            b_fragilis_18_one_epoch,
            b_fragilis_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 18')

compare_sfs(proportional_sfs(b_fragilis_18_empirical),
            proportional_sfs(b_fragilis_18_one_epoch),
            proportional_sfs(b_fragilis_18_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 18')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_18/b_intestinihominis_18.csv')
b_intestinihominis_18_empirical =  read_input_sfs(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/empirical_sfs.txt'
)
b_intestinihominis_18_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/one_epoch_demography.txt'
)
b_intestinihominis_18_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_18_empirical,
            b_intestinihominis_18_one_epoch,
            b_intestinihominis_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 18')

compare_sfs(proportional_sfs(b_intestinihominis_18_empirical),
            proportional_sfs(b_intestinihominis_18_one_epoch),
            proportional_sfs(b_intestinihominis_18_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 18')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_18/b_ovatus_18.csv')
b_ovatus_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_18/empirical_sfs.txt'
)
b_ovatus_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_18/one_epoch_demography.txt'
)
b_ovatus_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_18_empirical,
            b_ovatus_18_one_epoch,
            b_ovatus_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 18')

compare_sfs(proportional_sfs(b_ovatus_18_empirical),
            proportional_sfs(b_ovatus_18_one_epoch),
            proportional_sfs(b_ovatus_18_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 18')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_18/b_thetaiotaomicron_18.csv')
b_thetaiotaomicron_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/empirical_sfs.txt'
)
b_thetaiotaomicron_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/one_epoch_demography.txt'
)
b_thetaiotaomicron_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_18_empirical,
            b_thetaiotaomicron_18_one_epoch,
            b_thetaiotaomicron_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 18')

compare_sfs(proportional_sfs(b_thetaiotaomicron_18_empirical),
            proportional_sfs(b_thetaiotaomicron_18_one_epoch),
            proportional_sfs(b_thetaiotaomicron_18_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 18')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_18/b_uniformis_18.csv')
b_uniformis_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_18/empirical_sfs.txt'
)
b_uniformis_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_18/one_epoch_demography.txt'
)
b_uniformis_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_18_empirical,
            b_uniformis_18_one_epoch,
            b_uniformis_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 18')

compare_sfs(proportional_sfs(b_uniformis_18_empirical),
            proportional_sfs(b_uniformis_18_one_epoch),
            proportional_sfs(b_uniformis_18_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 18')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_18/b_vulgatus_18.csv')
b_vulgatus_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_18/empirical_sfs.txt'
)
b_vulgatus_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_18/one_epoch_demography.txt'
)
b_vulgatus_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_18_empirical,
            b_vulgatus_18_one_epoch,
            b_vulgatus_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 18')

compare_sfs(proportional_sfs(b_vulgatus_18_empirical),
            proportional_sfs(b_vulgatus_18_one_epoch),
            proportional_sfs(b_vulgatus_18_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 18')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_18/b_xylanisolvens_18.csv')
b_xylanisolvens_18_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/empirical_sfs.txt'
)
b_xylanisolvens_18_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/one_epoch_demography.txt'
)
b_xylanisolvens_18_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_18_empirical,
            b_xylanisolvens_18_one_epoch,
            b_xylanisolvens_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 18')

compare_sfs(proportional_sfs(b_xylanisolvens_18_empirical),
            proportional_sfs(b_xylanisolvens_18_one_epoch),
            proportional_sfs(b_xylanisolvens_18_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 18')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_18/d_invisus_18.csv')
d_invisus_18_empirical =  read_input_sfs(
  '../Analysis/Dialister_invisus_61905_downsampled_18/empirical_sfs.txt'
)
d_invisus_18_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_18/one_epoch_demography.txt'
)
d_invisus_18_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(d_invisus_18_empirical,
            d_invisus_18_one_epoch,
            d_invisus_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 18')

compare_sfs(proportional_sfs(d_invisus_18_empirical),
            proportional_sfs(d_invisus_18_one_epoch),
            proportional_sfs(d_invisus_18_two_epoch)) +
  ggtitle('D. invisus Downsampled to 18')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_18/e_eligens_18.csv')
e_eligens_18_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_18/empirical_sfs.txt'
)
e_eligens_18_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_18/one_epoch_demography.txt'
)
e_eligens_18_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(e_eligens_18_empirical,
            e_eligens_18_one_epoch,
            e_eligens_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 18')

compare_sfs(proportional_sfs(e_eligens_18_empirical),
            proportional_sfs(e_eligens_18_one_epoch),
            proportional_sfs(e_eligens_18_two_epoch)) +
  ggtitle('E. eligens Downsampled to 18')



# E. rectale
plot_likelihood_surface('../Analysis/qp_gut_18/e_rectale_18.csv')
e_rectale_18_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_rectale_56927_downsampled_18/empirical_sfs.txt'
)
e_rectale_18_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_18/one_epoch_demography.txt'
)
e_rectale_18_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(e_rectale_18_empirical,
            e_rectale_18_one_epoch,
            e_rectale_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 18')

compare_sfs(proportional_sfs(e_rectale_18_empirical),
            proportional_sfs(e_rectale_18_one_epoch),
            proportional_sfs(e_rectale_18_two_epoch)) +
  ggtitle('E. rectale Downsampled to 18')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_18/f_prausnitzii_18.csv')
f_prausnitzii_18_empirical =  read_input_sfs(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/empirical_sfs.txt'
)
f_prausnitzii_18_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/one_epoch_demography.txt'
)
f_prausnitzii_18_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_18_empirical,
            f_prausnitzii_18_one_epoch,
            f_prausnitzii_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 18')

compare_sfs(proportional_sfs(f_prausnitzii_18_empirical),
            proportional_sfs(f_prausnitzii_18_one_epoch),
            proportional_sfs(f_prausnitzii_18_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 18')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_18/o_sp_18.csv')
o_sp_18_empirical =  read_input_sfs(
  '../Analysis/Oscillibacter_sp_60799_downsampled_18/empirical_sfs.txt'
)
o_sp_18_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_18/one_epoch_demography.txt'
)
o_sp_18_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(o_sp_18_empirical,
            o_sp_18_one_epoch,
            o_sp_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 18')

compare_sfs(proportional_sfs(o_sp_18_empirical),
            proportional_sfs(o_sp_18_one_epoch),
            proportional_sfs(o_sp_18_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 18')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_18/o_splanchnicus_18.csv')
o_splanchnicus_18_empirical =  read_input_sfs(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/empirical_sfs.txt'
)
o_splanchnicus_18_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/one_epoch_demography.txt'
)
o_splanchnicus_18_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_18_empirical,
            o_splanchnicus_18_one_epoch,
            o_splanchnicus_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 18')

compare_sfs(proportional_sfs(o_splanchnicus_18_empirical),
            proportional_sfs(o_splanchnicus_18_one_epoch),
            proportional_sfs(o_splanchnicus_18_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 18')


# P. copri
# plot_likelihood_surface('../Analysis/qp_gut_18/p_copri_18.csv')
# p_copri_18_empirical =  read_input_sfs(
#   '../Analysis/Prevotella_copri_61740_downsampled_18/empirical_sfs.txt'
# )
# p_copri_18_one_epoch = sfs_from_demography(
#   '../Analysis/Prevotella_copri_61740_downsampled_18/one_epoch_demography.txt'
# )
# p_copri_18_two_epoch = sfs_from_demography(
#   '../Analysis/Prevotella_copri_61740_downsampled_18/two_epoch_demography.txt'
# )
# compare_sfs(p_copri_18_empirical,
#             p_copri_18_one_epoch,
#             p_copri_18_two_epoch) +
#   ylab('Raw Count of Segregating Sites') +
#   ggtitle('P. copri Downsampled to 18')
# 
# compare_sfs(proportional_sfs(p_copri_18_empirical),
#             proportional_sfs(p_copri_18_one_epoch),
#             proportional_sfs(p_copri_18_two_epoch)) +
#   ggtitle('P. copri Downsampled to 18')
# 
# 
# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_18/p_distasonis_18.csv')
p_distasonis_18_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_18/empirical_sfs.txt'
)
p_distasonis_18_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_18/one_epoch_demography.txt'
)
p_distasonis_18_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_18_empirical,
            p_distasonis_18_one_epoch,
            p_distasonis_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 18')

compare_sfs(proportional_sfs(p_distasonis_18_empirical),
            proportional_sfs(p_distasonis_18_one_epoch),
            proportional_sfs(p_distasonis_18_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 18')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_18/p_merdae_18.csv')
p_merdae_18_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_18/empirical_sfs.txt'
)
p_merdae_18_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_18/one_epoch_demography.txt'
)
p_merdae_18_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(p_merdae_18_empirical,
            p_merdae_18_one_epoch,
            p_merdae_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 18')

compare_sfs(proportional_sfs(p_merdae_18_empirical),
            proportional_sfs(p_merdae_18_one_epoch),
            proportional_sfs(p_merdae_18_two_epoch)) +
  ggtitle('P. merdae Downsampled to 18')


# Phascolarctobacterium sp.
# plot_likelihood_surface('../Analysis/qp_gut_18/p_sp_18.csv')
# p_sp_18_empirical =  read_input_sfs(
#   '../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/empirical_sfs.txt'
# )
# p_sp_18_one_epoch = sfs_from_demography(
#   '../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/one_epoch_demography.txt'
# )
# p_sp_18_two_epoch = sfs_from_demography(
#   '../Analysis/Phascolarctobacterium_sp_59817_downsampled_18/two_epoch_demography.txt'
# )
# compare_sfs(p_sp_18_empirical,
#             p_sp_18_one_epoch,
#             p_sp_18_two_epoch) +
#   ylab('Raw Count of Segregating Sites') +
#   ggtitle('Phascolarctobacterium sp. Downsampled to 18')
# 
# compare_sfs(proportional_sfs(p_sp_18_empirical),
#             proportional_sfs(p_sp_18_one_epoch),
#             proportional_sfs(p_sp_18_two_epoch)) +
#   ggtitle('Phascolarctobacterium sp. Downsampled to 18')
# 
# 
# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_18/r_bicirculans_18.csv')
r_bicirculans_18_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/empirical_sfs.txt'
)
r_bicirculans_18_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/one_epoch_demography.txt'
)
r_bicirculans_18_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_18_empirical,
            r_bicirculans_18_one_epoch,
            r_bicirculans_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 18')

compare_sfs(proportional_sfs(r_bicirculans_18_empirical),
            proportional_sfs(r_bicirculans_18_one_epoch),
            proportional_sfs(r_bicirculans_18_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 18')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_18/r_bromii_18.csv')
r_bromii_18_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_18/empirical_sfs.txt'
)
r_bromii_18_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_18/one_epoch_demography.txt'
)
r_bromii_18_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_18/two_epoch_demography.txt'
)
compare_sfs(r_bromii_18_empirical,
            r_bromii_18_one_epoch,
            r_bromii_18_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 18')

compare_sfs(proportional_sfs(r_bromii_18_empirical),
            proportional_sfs(r_bromii_18_one_epoch),
            proportional_sfs(r_bromii_18_two_epoch)) +
  ggtitle('R. bromii Downsampled to 18')

# Downsampled to 20

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_20/a_finegoldii_20.csv')
a_finegoldii_20_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_20/empirical_sfs.txt'
)
a_finegoldii_20_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_20/one_epoch_demography.txt'
)
a_finegoldii_20_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_20/two_epoch_demography.txt'
)

compare_sfs(a_finegoldii_20_empirical,
            a_finegoldii_20_one_epoch,
            a_finegoldii_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 20')

compare_sfs(proportional_sfs(a_finegoldii_20_empirical),
            proportional_sfs(a_finegoldii_20_one_epoch),
            proportional_sfs(a_finegoldii_20_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 20')

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_20/a_muciniphila_20.csv')
a_muciniphila_20_empirical =  read_input_sfs(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_20/empirical_sfs.txt'
)
a_muciniphila_20_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_20/one_epoch_demography.txt'
)
a_muciniphila_20_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_20_empirical,
            a_muciniphila_20_one_epoch,
            a_muciniphila_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 20')

compare_sfs(proportional_sfs(a_muciniphila_20_empirical),
            proportional_sfs(a_muciniphila_20_one_epoch),
            proportional_sfs(a_muciniphila_20_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 20')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_20/a_onderdonkii_20.csv')
a_onderdonkii_20_empirical =  read_input_sfs(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_20/empirical_sfs.txt'
)
a_onderdonkii_20_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_20/one_epoch_demography.txt'
)
a_onderdonkii_20_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_20/two_epoch_demography.txt'
)

compare_sfs(a_onderdonkii_20_empirical,
            a_onderdonkii_20_one_epoch,
            a_onderdonkii_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 20')

compare_sfs(proportional_sfs(a_onderdonkii_20_empirical),
            proportional_sfs(a_onderdonkii_20_one_epoch),
            proportional_sfs(a_onderdonkii_20_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 20')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_20/a_putredinis_20.csv')
a_putredinis_20_empirical =  read_input_sfs(
  '../Analysis/Alistipes_putredinis_61533_downsampled_20/empirical_sfs.txt'
)
a_putredinis_20_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_20/one_epoch_demography.txt'
)
a_putredinis_20_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_20_empirical,
            a_putredinis_20_one_epoch,
            a_putredinis_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 20')

compare_sfs(proportional_sfs(a_putredinis_20_empirical),
            proportional_sfs(a_putredinis_20_one_epoch),
            proportional_sfs(a_putredinis_20_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 20')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_20/a_shahii_20.csv')
a_shahii_20_empirical =  read_input_sfs(
  '../Analysis/Alistipes_shahii_62199_downsampled_20/empirical_sfs.txt'
)
a_shahii_20_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_20/one_epoch_demography.txt'
)
a_shahii_20_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(a_shahii_20_empirical,
            a_shahii_20_one_epoch,
            a_shahii_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 20')

compare_sfs(proportional_sfs(a_shahii_20_empirical),
            proportional_sfs(a_shahii_20_one_epoch),
            proportional_sfs(a_shahii_20_two_epoch)) +
  ggtitle('A. shahii Downsampled to 20')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_20/b_bacterium_20.csv')
b_bacterium_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_20/empirical_sfs.txt'
)
b_bacterium_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_20/one_epoch_demography.txt'
)
b_bacterium_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_20_empirical,
            b_bacterium_20_one_epoch,
            b_bacterium_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 20')

compare_sfs(proportional_sfs(b_bacterium_20_empirical),
            proportional_sfs(b_bacterium_20_one_epoch),
            proportional_sfs(b_bacterium_20_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 20')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_20/b_caccae_20.csv')
b_caccae_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_caccae_53434_downsampled_20/empirical_sfs.txt'
)
b_caccae_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_20/one_epoch_demography.txt'
)
b_caccae_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_caccae_20_empirical,
            b_caccae_20_one_epoch,
            b_caccae_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 20')

compare_sfs(proportional_sfs(b_caccae_20_empirical),
            proportional_sfs(b_caccae_20_one_epoch),
            proportional_sfs(b_caccae_20_two_epoch)) +
  ggtitle('B. caccae Downsampled to 20')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_20/b_cellulosilyticus_20.csv')
b_cellulosilyticus_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/empirical_sfs.txt'
)
b_cellulosilyticus_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/one_epoch_demography.txt'
)
b_cellulosilyticus_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_20_empirical,
            b_cellulosilyticus_20_one_epoch,
            b_cellulosilyticus_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 20')

compare_sfs(proportional_sfs(b_cellulosilyticus_20_empirical),
            proportional_sfs(b_cellulosilyticus_20_one_epoch),
            proportional_sfs(b_cellulosilyticus_20_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 20')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_20/b_fragilis_20.csv')
b_fragilis_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_20/empirical_sfs.txt'
)
b_fragilis_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_20/one_epoch_demography.txt'
)
b_fragilis_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_20_empirical,
            b_fragilis_20_one_epoch,
            b_fragilis_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 20')

compare_sfs(proportional_sfs(b_fragilis_20_empirical),
            proportional_sfs(b_fragilis_20_one_epoch),
            proportional_sfs(b_fragilis_20_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 20')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_20/b_intestinihominis_20.csv')
b_intestinihominis_20_empirical =  read_input_sfs(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/empirical_sfs.txt'
)
b_intestinihominis_20_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/one_epoch_demography.txt'
)
b_intestinihominis_20_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_20_empirical,
            b_intestinihominis_20_one_epoch,
            b_intestinihominis_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 20')

compare_sfs(proportional_sfs(b_intestinihominis_20_empirical),
            proportional_sfs(b_intestinihominis_20_one_epoch),
            proportional_sfs(b_intestinihominis_20_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 20')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_20/b_ovatus_20.csv')
b_ovatus_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_20/empirical_sfs.txt'
)
b_ovatus_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_20/one_epoch_demography.txt'
)
b_ovatus_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_20_empirical,
            b_ovatus_20_one_epoch,
            b_ovatus_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 20')

compare_sfs(proportional_sfs(b_ovatus_20_empirical),
            proportional_sfs(b_ovatus_20_one_epoch),
            proportional_sfs(b_ovatus_20_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 20')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_20/b_thetaiotaomicron_20.csv')
b_thetaiotaomicron_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/empirical_sfs.txt'
)
b_thetaiotaomicron_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/one_epoch_demography.txt'
)
b_thetaiotaomicron_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_20_empirical,
            b_thetaiotaomicron_20_one_epoch,
            b_thetaiotaomicron_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 20')

compare_sfs(proportional_sfs(b_thetaiotaomicron_20_empirical),
            proportional_sfs(b_thetaiotaomicron_20_one_epoch),
            proportional_sfs(b_thetaiotaomicron_20_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 20')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_20/b_uniformis_20.csv')
b_uniformis_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_20/empirical_sfs.txt'
)
b_uniformis_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_20/one_epoch_demography.txt'
)
b_uniformis_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_20_empirical,
            b_uniformis_20_one_epoch,
            b_uniformis_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 20')

compare_sfs(proportional_sfs(b_uniformis_20_empirical),
            proportional_sfs(b_uniformis_20_one_epoch),
            proportional_sfs(b_uniformis_20_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 20')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_20/b_vulgatus_20.csv')
b_vulgatus_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_20/empirical_sfs.txt'
)
b_vulgatus_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_20/one_epoch_demography.txt'
)
b_vulgatus_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_20_empirical,
            b_vulgatus_20_one_epoch,
            b_vulgatus_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 20')

compare_sfs(proportional_sfs(b_vulgatus_20_empirical),
            proportional_sfs(b_vulgatus_20_one_epoch),
            proportional_sfs(b_vulgatus_20_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 20')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_20/b_xylanisolvens_20.csv')
b_xylanisolvens_20_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/empirical_sfs.txt'
)
b_xylanisolvens_20_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/one_epoch_demography.txt'
)
b_xylanisolvens_20_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_20_empirical,
            b_xylanisolvens_20_one_epoch,
            b_xylanisolvens_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 20')

compare_sfs(proportional_sfs(b_xylanisolvens_20_empirical),
            proportional_sfs(b_xylanisolvens_20_one_epoch),
            proportional_sfs(b_xylanisolvens_20_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 20')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_20/d_invisus_20.csv')
d_invisus_20_empirical =  read_input_sfs(
  '../Analysis/Dialister_invisus_61905_downsampled_20/empirical_sfs.txt'
)
d_invisus_20_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_20/one_epoch_demography.txt'
)
d_invisus_20_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(d_invisus_20_empirical,
            d_invisus_20_one_epoch,
            d_invisus_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 20')

compare_sfs(proportional_sfs(d_invisus_20_empirical),
            proportional_sfs(d_invisus_20_one_epoch),
            proportional_sfs(d_invisus_20_two_epoch)) +
  ggtitle('D. invisus Downsampled to 20')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_20/e_eligens_20.csv')
e_eligens_20_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_20/empirical_sfs.txt'
)
e_eligens_20_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_20/one_epoch_demography.txt'
)
e_eligens_20_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(e_eligens_20_empirical,
            e_eligens_20_one_epoch,
            e_eligens_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 20')

compare_sfs(proportional_sfs(e_eligens_20_empirical),
            proportional_sfs(e_eligens_20_one_epoch),
            proportional_sfs(e_eligens_20_two_epoch)) +
  ggtitle('E. eligens Downsampled to 20')



# E. rectale
plot_likelihood_surface('../Analysis/qp_gut_20/e_rectale_20.csv')
e_rectale_20_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_rectale_56927_downsampled_20/empirical_sfs.txt'
)
e_rectale_20_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_20/one_epoch_demography.txt'
)
e_rectale_20_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(e_rectale_20_empirical,
            e_rectale_20_one_epoch,
            e_rectale_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 20')

compare_sfs(proportional_sfs(e_rectale_20_empirical),
            proportional_sfs(e_rectale_20_one_epoch),
            proportional_sfs(e_rectale_20_two_epoch)) +
  ggtitle('E. rectale Downsampled to 20')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_20/f_prausnitzii_20.csv')
f_prausnitzii_20_empirical =  read_input_sfs(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/empirical_sfs.txt'
)
f_prausnitzii_20_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/one_epoch_demography.txt'
)
f_prausnitzii_20_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_20_empirical,
            f_prausnitzii_20_one_epoch,
            f_prausnitzii_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 20')

compare_sfs(proportional_sfs(f_prausnitzii_20_empirical),
            proportional_sfs(f_prausnitzii_20_one_epoch),
            proportional_sfs(f_prausnitzii_20_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 20')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_20/o_sp_20.csv')
o_sp_20_empirical =  read_input_sfs(
  '../Analysis/Oscillibacter_sp_60799_downsampled_20/empirical_sfs.txt'
)
o_sp_20_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_20/one_epoch_demography.txt'
)
o_sp_20_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(o_sp_20_empirical,
            o_sp_20_one_epoch,
            o_sp_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 20')

compare_sfs(proportional_sfs(o_sp_20_empirical),
            proportional_sfs(o_sp_20_one_epoch),
            proportional_sfs(o_sp_20_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 20')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_20/o_splanchnicus_20.csv')
o_splanchnicus_20_empirical =  read_input_sfs(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/empirical_sfs.txt'
)
o_splanchnicus_20_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/one_epoch_demography.txt'
)
o_splanchnicus_20_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_20_empirical,
            o_splanchnicus_20_one_epoch,
            o_splanchnicus_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 20')

compare_sfs(proportional_sfs(o_splanchnicus_20_empirical),
            proportional_sfs(o_splanchnicus_20_one_epoch),
            proportional_sfs(o_splanchnicus_20_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 20')


# P. copri
plot_likelihood_surface('../Analysis/qp_gut_20/p_copri_20.csv')
p_copri_20_empirical =  read_input_sfs(
  '../Analysis/Prevotella_copri_61740_downsampled_20/empirical_sfs.txt'
)
p_copri_20_one_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_20/one_epoch_demography.txt'
)
p_copri_20_two_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(p_copri_20_empirical,
            p_copri_20_one_epoch,
            p_copri_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. copri Downsampled to 20')

compare_sfs(proportional_sfs(p_copri_20_empirical),
            proportional_sfs(p_copri_20_one_epoch),
            proportional_sfs(p_copri_20_two_epoch)) +
  ggtitle('P. copri Downsampled to 20')


# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_20/p_distasonis_20.csv')
p_distasonis_20_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_20/empirical_sfs.txt'
)
p_distasonis_20_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_20/one_epoch_demography.txt'
)
p_distasonis_20_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_20_empirical,
            p_distasonis_20_one_epoch,
            p_distasonis_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 20')

compare_sfs(proportional_sfs(p_distasonis_20_empirical),
            proportional_sfs(p_distasonis_20_one_epoch),
            proportional_sfs(p_distasonis_20_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 20')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_20/p_merdae_20.csv')
p_merdae_20_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_20/empirical_sfs.txt'
)
p_merdae_20_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_20/one_epoch_demography.txt'
)
p_merdae_20_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(p_merdae_20_empirical,
            p_merdae_20_one_epoch,
            p_merdae_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 20')

compare_sfs(proportional_sfs(p_merdae_20_empirical),
            proportional_sfs(p_merdae_20_one_epoch),
            proportional_sfs(p_merdae_20_two_epoch)) +
  ggtitle('P. merdae Downsampled to 20')


# Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_20/p_sp_20.csv')
p_sp_20_empirical =  read_input_sfs(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/empirical_sfs.txt'
)
p_sp_20_one_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/one_epoch_demography.txt'
)
p_sp_20_two_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(p_sp_20_empirical,
            p_sp_20_one_epoch,
            p_sp_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Phascolarctobacterium sp. Downsampled to 20')

compare_sfs(proportional_sfs(p_sp_20_empirical),
            proportional_sfs(p_sp_20_one_epoch),
            proportional_sfs(p_sp_20_two_epoch)) +
  ggtitle('Phascolarctobacterium sp. Downsampled to 20')


# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_20/r_bicirculans_20.csv')
r_bicirculans_20_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/empirical_sfs.txt'
)
r_bicirculans_20_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/one_epoch_demography.txt'
)
r_bicirculans_20_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_20_empirical,
            r_bicirculans_20_one_epoch,
            r_bicirculans_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 20')

compare_sfs(proportional_sfs(r_bicirculans_20_empirical),
            proportional_sfs(r_bicirculans_20_one_epoch),
            proportional_sfs(r_bicirculans_20_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 20')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_20/r_bromii_20.csv')
r_bromii_20_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_20/empirical_sfs.txt'
)
r_bromii_20_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_20/one_epoch_demography.txt'
)
r_bromii_20_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_20/two_epoch_demography.txt'
)
compare_sfs(r_bromii_20_empirical,
            r_bromii_20_one_epoch,
            r_bromii_20_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 20')

compare_sfs(proportional_sfs(r_bromii_20_empirical),
            proportional_sfs(r_bromii_20_one_epoch),
            proportional_sfs(r_bromii_20_two_epoch)) +
  ggtitle('R. bromii Downsampled to 20')

# Downsampled to 30

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_30/a_finegoldii_30.csv')
a_finegoldii_30_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_30/empirical_sfs.txt'
)
a_finegoldii_30_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_30/one_epoch_demography.txt'
)
a_finegoldii_30_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_30/two_epoch_demography.txt'
)

compare_sfs(a_finegoldii_30_empirical,
            a_finegoldii_30_one_epoch,
            a_finegoldii_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. finegoldii Downsampled to 30')

compare_sfs(proportional_sfs(a_finegoldii_30_empirical),
            proportional_sfs(a_finegoldii_30_one_epoch),
            proportional_sfs(a_finegoldii_30_two_epoch)) +
  ggtitle('A. finegoldii Downsampled to 30')

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_30/a_muciniphila_30.csv')
a_muciniphila_30_empirical =  read_input_sfs(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_30/empirical_sfs.txt'
)
a_muciniphila_30_one_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_30/one_epoch_demography.txt'
)
a_muciniphila_30_two_epoch = sfs_from_demography(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_30_empirical,
            a_muciniphila_30_one_epoch,
            a_muciniphila_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. muciniphila Downsampled to 30')

compare_sfs(proportional_sfs(a_muciniphila_30_empirical),
            proportional_sfs(a_muciniphila_30_one_epoch),
            proportional_sfs(a_muciniphila_30_two_epoch)) +
  ggtitle('A. muciniphila Downsampled to 30')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_30/a_onderdonkii_30.csv')
a_onderdonkii_30_empirical =  read_input_sfs(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_30/empirical_sfs.txt'
)
a_onderdonkii_30_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_30/one_epoch_demography.txt'
)
a_onderdonkii_30_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_30/two_epoch_demography.txt'
)

compare_sfs(a_onderdonkii_30_empirical,
            a_onderdonkii_30_one_epoch,
            a_onderdonkii_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. onderdonkii Downsampled to 30')

compare_sfs(proportional_sfs(a_onderdonkii_30_empirical),
            proportional_sfs(a_onderdonkii_30_one_epoch),
            proportional_sfs(a_onderdonkii_30_two_epoch)) +
  ggtitle('A. onderdonkii Downsampled to 30')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_30/a_putredinis_30.csv')
a_putredinis_30_empirical =  read_input_sfs(
  '../Analysis/Alistipes_putredinis_61533_downsampled_30/empirical_sfs.txt'
)
a_putredinis_30_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_30/one_epoch_demography.txt'
)
a_putredinis_30_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_putredinis_61533_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_30_empirical,
            a_putredinis_30_one_epoch,
            a_putredinis_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. putredinis Downsampled to 30')

compare_sfs(proportional_sfs(a_putredinis_30_empirical),
            proportional_sfs(a_putredinis_30_one_epoch),
            proportional_sfs(a_putredinis_30_two_epoch)) +
  ggtitle('A. putredinis Downsampled to 30')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_30/a_shahii_30.csv')
a_shahii_30_empirical =  read_input_sfs(
  '../Analysis/Alistipes_shahii_62199_downsampled_30/empirical_sfs.txt'
)
a_shahii_30_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_30/one_epoch_demography.txt'
)
a_shahii_30_two_epoch = sfs_from_demography(
  '../Analysis/Alistipes_shahii_62199_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(a_shahii_30_empirical,
            a_shahii_30_one_epoch,
            a_shahii_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('A. shahii Downsampled to 30')

compare_sfs(proportional_sfs(a_shahii_30_empirical),
            proportional_sfs(a_shahii_30_one_epoch),
            proportional_sfs(a_shahii_30_two_epoch)) +
  ggtitle('A. shahii Downsampled to 30')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_30/b_bacterium_30.csv')
b_bacterium_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_30/empirical_sfs.txt'
)
b_bacterium_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_30/one_epoch_demography.txt'
)
b_bacterium_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_30_empirical,
            b_bacterium_30_one_epoch,
            b_bacterium_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. bacterium Downsampled to 30')

compare_sfs(proportional_sfs(b_bacterium_30_empirical),
            proportional_sfs(b_bacterium_30_one_epoch),
            proportional_sfs(b_bacterium_30_two_epoch)) +
  ggtitle('B. bacterium Downsampled to 30')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_30/b_caccae_30.csv')
b_caccae_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_caccae_53434_downsampled_30/empirical_sfs.txt'
)
b_caccae_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_30/one_epoch_demography.txt'
)
b_caccae_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_caccae_53434_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_caccae_30_empirical,
            b_caccae_30_one_epoch,
            b_caccae_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. caccae Downsampled to 30')

compare_sfs(proportional_sfs(b_caccae_30_empirical),
            proportional_sfs(b_caccae_30_one_epoch),
            proportional_sfs(b_caccae_30_two_epoch)) +
  ggtitle('B. caccae Downsampled to 30')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_30/b_cellulosilyticus_30.csv')
b_cellulosilyticus_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/empirical_sfs.txt'
)
b_cellulosilyticus_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/one_epoch_demography.txt'
)
b_cellulosilyticus_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_30_empirical,
            b_cellulosilyticus_30_one_epoch,
            b_cellulosilyticus_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. cellulosilyticus Downsampled to 30')

compare_sfs(proportional_sfs(b_cellulosilyticus_30_empirical),
            proportional_sfs(b_cellulosilyticus_30_one_epoch),
            proportional_sfs(b_cellulosilyticus_30_two_epoch)) +
  ggtitle('B. cellulosilyticus Downsampled to 30')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_30/b_fragilis_30.csv')
b_fragilis_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_30/empirical_sfs.txt'
)
b_fragilis_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_30/one_epoch_demography.txt'
)
b_fragilis_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_30_empirical,
            b_fragilis_30_one_epoch,
            b_fragilis_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. fragilis Downsampled to 30')

compare_sfs(proportional_sfs(b_fragilis_30_empirical),
            proportional_sfs(b_fragilis_30_one_epoch),
            proportional_sfs(b_fragilis_30_two_epoch)) +
  ggtitle('B. fragilis Downsampled to 30')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_30/b_intestinihominis_30.csv')
b_intestinihominis_30_empirical =  read_input_sfs(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/empirical_sfs.txt'
)
b_intestinihominis_30_one_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/one_epoch_demography.txt'
)
b_intestinihominis_30_two_epoch = sfs_from_demography(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_30_empirical,
            b_intestinihominis_30_one_epoch,
            b_intestinihominis_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. intestinihominis Downsampled to 30')

compare_sfs(proportional_sfs(b_intestinihominis_30_empirical),
            proportional_sfs(b_intestinihominis_30_one_epoch),
            proportional_sfs(b_intestinihominis_30_two_epoch)) +
  ggtitle('B. intestinihominis Downsampled to 30')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_30/b_ovatus_30.csv')
b_ovatus_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_30/empirical_sfs.txt'
)
b_ovatus_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_30/one_epoch_demography.txt'
)
b_ovatus_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_30_empirical,
            b_ovatus_30_one_epoch,
            b_ovatus_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. ovatus Downsampled to 30')

compare_sfs(proportional_sfs(b_ovatus_30_empirical),
            proportional_sfs(b_ovatus_30_one_epoch),
            proportional_sfs(b_ovatus_30_two_epoch)) +
  ggtitle('B. ovatus Downsampled to 30')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_30/b_thetaiotaomicron_30.csv')
b_thetaiotaomicron_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/empirical_sfs.txt'
)
b_thetaiotaomicron_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/one_epoch_demography.txt'
)
b_thetaiotaomicron_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_30_empirical,
            b_thetaiotaomicron_30_one_epoch,
            b_thetaiotaomicron_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. thetaiotaomicron Downsampled to 30')

compare_sfs(proportional_sfs(b_thetaiotaomicron_30_empirical),
            proportional_sfs(b_thetaiotaomicron_30_one_epoch),
            proportional_sfs(b_thetaiotaomicron_30_two_epoch)) +
  ggtitle('B. thetaiotaomicron Downsampled to 30')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_30/b_uniformis_30.csv')
b_uniformis_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_30/empirical_sfs.txt'
)
b_uniformis_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_30/one_epoch_demography.txt'
)
b_uniformis_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_uniformis_57318_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_30_empirical,
            b_uniformis_30_one_epoch,
            b_uniformis_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. uniformis Downsampled to 30')

compare_sfs(proportional_sfs(b_uniformis_30_empirical),
            proportional_sfs(b_uniformis_30_one_epoch),
            proportional_sfs(b_uniformis_30_two_epoch)) +
  ggtitle('B. uniformis Downsampled to 30')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_30/b_vulgatus_30.csv')
b_vulgatus_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_30/empirical_sfs.txt'
)
b_vulgatus_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_30/one_epoch_demography.txt'
)
b_vulgatus_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_30_empirical,
            b_vulgatus_30_one_epoch,
            b_vulgatus_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. vulgatus Downsampled to 30')

compare_sfs(proportional_sfs(b_vulgatus_30_empirical),
            proportional_sfs(b_vulgatus_30_one_epoch),
            proportional_sfs(b_vulgatus_30_two_epoch)) +
  ggtitle('B. vulgatus Downsampled to 30')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_30/b_xylanisolvens_30.csv')
b_xylanisolvens_30_empirical =  read_input_sfs(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/empirical_sfs.txt'
)
b_xylanisolvens_30_one_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/one_epoch_demography.txt'
)
b_xylanisolvens_30_two_epoch = sfs_from_demography(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_30_empirical,
            b_xylanisolvens_30_one_epoch,
            b_xylanisolvens_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('B. xylanisolvens Downsampled to 30')

compare_sfs(proportional_sfs(b_xylanisolvens_30_empirical),
            proportional_sfs(b_xylanisolvens_30_one_epoch),
            proportional_sfs(b_xylanisolvens_30_two_epoch)) +
  ggtitle('B. xylanisolvens Downsampled to 30')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_30/d_invisus_30.csv')
d_invisus_30_empirical =  read_input_sfs(
  '../Analysis/Dialister_invisus_61905_downsampled_30/empirical_sfs.txt'
)
d_invisus_30_one_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_30/one_epoch_demography.txt'
)
d_invisus_30_two_epoch = sfs_from_demography(
  '../Analysis/Dialister_invisus_61905_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(d_invisus_30_empirical,
            d_invisus_30_one_epoch,
            d_invisus_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('D. invisus Downsampled to 30')

compare_sfs(proportional_sfs(d_invisus_30_empirical),
            proportional_sfs(d_invisus_30_one_epoch),
            proportional_sfs(d_invisus_30_two_epoch)) +
  ggtitle('D. invisus Downsampled to 30')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_30/e_eligens_30.csv')
e_eligens_30_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_30/empirical_sfs.txt'
)
e_eligens_30_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_30/one_epoch_demography.txt'
)
e_eligens_30_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(e_eligens_30_empirical,
            e_eligens_30_one_epoch,
            e_eligens_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 30')

compare_sfs(proportional_sfs(e_eligens_30_empirical),
            proportional_sfs(e_eligens_30_one_epoch),
            proportional_sfs(e_eligens_30_two_epoch)) +
  ggtitle('E. eligens Downsampled to 30')



# E. rectale
plot_likelihood_surface('../Analysis/qp_gut_30/e_rectale_30.csv')
e_rectale_30_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_rectale_56927_downsampled_30/empirical_sfs.txt'
)
e_rectale_30_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_30/one_epoch_demography.txt'
)
e_rectale_30_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_rectale_56927_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(e_rectale_30_empirical,
            e_rectale_30_one_epoch,
            e_rectale_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. rectale Downsampled to 30')

compare_sfs(proportional_sfs(e_rectale_30_empirical),
            proportional_sfs(e_rectale_30_one_epoch),
            proportional_sfs(e_rectale_30_two_epoch)) +
  ggtitle('E. rectale Downsampled to 30')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_30/f_prausnitzii_30.csv')
f_prausnitzii_30_empirical =  read_input_sfs(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/empirical_sfs.txt'
)
f_prausnitzii_30_one_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/one_epoch_demography.txt'
)
f_prausnitzii_30_two_epoch = sfs_from_demography(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_30_empirical,
            f_prausnitzii_30_one_epoch,
            f_prausnitzii_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('F. prausnitzii Downsampled to 30')

compare_sfs(proportional_sfs(f_prausnitzii_30_empirical),
            proportional_sfs(f_prausnitzii_30_one_epoch),
            proportional_sfs(f_prausnitzii_30_two_epoch)) +
  ggtitle('F. prausnitzii Downsampled to 30')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_30/o_sp_30.csv')
o_sp_30_empirical =  read_input_sfs(
  '../Analysis/Oscillibacter_sp_60799_downsampled_30/empirical_sfs.txt'
)
o_sp_30_one_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_30/one_epoch_demography.txt'
)
o_sp_30_two_epoch = sfs_from_demography(
  '../Analysis/Oscillibacter_sp_60799_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(o_sp_30_empirical,
            o_sp_30_one_epoch,
            o_sp_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Oscillibacter sp. Downsampled to 30')

compare_sfs(proportional_sfs(o_sp_30_empirical),
            proportional_sfs(o_sp_30_one_epoch),
            proportional_sfs(o_sp_30_two_epoch)) +
  ggtitle('Oscillibacter sp. Downsampled to 30')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_30/o_splanchnicus_30.csv')
o_splanchnicus_30_empirical =  read_input_sfs(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/empirical_sfs.txt'
)
o_splanchnicus_30_one_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/one_epoch_demography.txt'
)
o_splanchnicus_30_two_epoch = sfs_from_demography(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_30_empirical,
            o_splanchnicus_30_one_epoch,
            o_splanchnicus_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('O. splanchnicus Downsampled to 30')

compare_sfs(proportional_sfs(o_splanchnicus_30_empirical),
            proportional_sfs(o_splanchnicus_30_one_epoch),
            proportional_sfs(o_splanchnicus_30_two_epoch)) +
  ggtitle('O. splanchnicus Downsampled to 30')


# P. copri
plot_likelihood_surface('../Analysis/qp_gut_30/p_copri_30.csv')
p_copri_30_empirical =  read_input_sfs(
  '../Analysis/Prevotella_copri_61740_downsampled_30/empirical_sfs.txt'
)
p_copri_30_one_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_30/one_epoch_demography.txt'
)
p_copri_30_two_epoch = sfs_from_demography(
  '../Analysis/Prevotella_copri_61740_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(p_copri_30_empirical,
            p_copri_30_one_epoch,
            p_copri_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. copri Downsampled to 30')

compare_sfs(proportional_sfs(p_copri_30_empirical),
            proportional_sfs(p_copri_30_one_epoch),
            proportional_sfs(p_copri_30_two_epoch)) +
  ggtitle('P. copri Downsampled to 30')


# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_30/p_distasonis_30.csv')
p_distasonis_30_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_30/empirical_sfs.txt'
)
p_distasonis_30_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_30/one_epoch_demography.txt'
)
p_distasonis_30_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_30_empirical,
            p_distasonis_30_one_epoch,
            p_distasonis_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. distasonis Downsampled to 30')

compare_sfs(proportional_sfs(p_distasonis_30_empirical),
            proportional_sfs(p_distasonis_30_one_epoch),
            proportional_sfs(p_distasonis_30_two_epoch)) +
  ggtitle('P. distasonis Downsampled to 30')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_30/p_merdae_30.csv')
p_merdae_30_empirical =  read_input_sfs(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_30/empirical_sfs.txt'
)
p_merdae_30_one_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_30/one_epoch_demography.txt'
)
p_merdae_30_two_epoch = sfs_from_demography(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(p_merdae_30_empirical,
            p_merdae_30_one_epoch,
            p_merdae_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('P. merdae Downsampled to 30')

compare_sfs(proportional_sfs(p_merdae_30_empirical),
            proportional_sfs(p_merdae_30_one_epoch),
            proportional_sfs(p_merdae_30_two_epoch)) +
  ggtitle('P. merdae Downsampled to 30')


# Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_30/p_sp_30.csv')
p_sp_30_empirical =  read_input_sfs(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/empirical_sfs.txt'
)
p_sp_30_one_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/one_epoch_demography.txt'
)
p_sp_30_two_epoch = sfs_from_demography(
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(p_sp_30_empirical,
            p_sp_30_one_epoch,
            p_sp_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('Phascolarctobacterium sp. Downsampled to 30')

compare_sfs(proportional_sfs(p_sp_30_empirical),
            proportional_sfs(p_sp_30_one_epoch),
            proportional_sfs(p_sp_30_two_epoch)) +
  ggtitle('Phascolarctobacterium sp. Downsampled to 30')


# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_30/r_bicirculans_30.csv')
r_bicirculans_30_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/empirical_sfs.txt'
)
r_bicirculans_30_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/one_epoch_demography.txt'
)
r_bicirculans_30_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_30_empirical,
            r_bicirculans_30_one_epoch,
            r_bicirculans_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bicirculans Downsampled to 30')

compare_sfs(proportional_sfs(r_bicirculans_30_empirical),
            proportional_sfs(r_bicirculans_30_one_epoch),
            proportional_sfs(r_bicirculans_30_two_epoch)) +
  ggtitle('R. bicirculans Downsampled to 30')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_30/r_bromii_30.csv')
r_bromii_30_empirical =  read_input_sfs(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_30/empirical_sfs.txt'
)
r_bromii_30_one_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_30/one_epoch_demography.txt'
)
r_bromii_30_two_epoch = sfs_from_demography(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_30/two_epoch_demography.txt'
)
compare_sfs(r_bromii_30_empirical,
            r_bromii_30_one_epoch,
            r_bromii_30_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('R. bromii Downsampled to 30')

compare_sfs(proportional_sfs(r_bromii_30_empirical),
            proportional_sfs(r_bromii_30_one_epoch),
            proportional_sfs(r_bromii_30_two_epoch)) +
  ggtitle('R. bromii Downsampled to 30')

# Original empirical SFS with Clade Control

plot_original_empirical_sfs(a_muciniphila_original_empirical) + ggtitle('A. muciniphila full empirical SFS (unfolded)')

plot_original_empirical_sfs(fold_sfs(a_muciniphila_original_empirical)) + ggtitle('A. mucinaphila full empirical SFS (folded)')
a_muciniphila_garud_good_empirical = read_input_sfs_original('../Data/Akkermansia_muciniphila_55290_syn.sfs')
plot_original_empirical_sfs(fold_sfs(a_muciniphila_garud_good_empirical)) + ggtitle('A. muciniphila folded SFS (Garud/Good code base)')

temp_x_axis = 0:(length(fold_sfs(a_muciniphila_original_empirical))-1)

compare_sfs(temp_x_axis,
            fold_sfs(a_muciniphila_original_empirical),
            fold_sfs(c(a_muciniphila_garud_good_empirical, 0, 0, 0, 0, 0, 0)))

plot_original_empirical_sfs(a_muciniphila_original_empirical) + ggtitle('A. muciniphila full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(a_finegoldii_original_empirical) + ggtitle('A. finegoldii full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(a_onderdonkii_original_empirical) + ggtitle('A. onderdonkii full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(a_putredinis_original_empirical) + ggtitle('A. putredinis full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(a_shahii_original_empirical) + ggtitle('A. shahii full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_bacterium_original_empirical) + ggtitle('B. bacterium full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_caccae_original_empirical) + ggtitle('B. caccae full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_cellulosilyticus_original_empirical) + ggtitle('B. cellulosilyticus full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_fragilis_original_empirical) + ggtitle('B. fragilis full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_massiliensis_original_empirical) + ggtitle('B. massiliensis full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_ovatus_original_empirical) + ggtitle('B. ovatus full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_stercoris_original_empirical) + ggtitle('B. stercoris full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_thetaiotaomicron_original_empirical) + ggtitle('B. thetaiotaomicron full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_uniformis_original_empirical) + ggtitle('B. uniformis full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_vulgatus_original_empirical) + ggtitle('B. vulgatus full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_xylanisolvens_original_empirical) + ggtitle('B. xylanisolvens full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(b_intestinihominis_original_empirical) + ggtitle('B. intestinihominis full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(c_sp_original_empirical) + ggtitle('Coprococcus sp full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(d_invisus_original_empirical) + ggtitle('D. invisus full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(e_eligens_original_empirical) + ggtitle('E. eligens full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(e_rectale_original_empirical) + ggtitle('E. rectale full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(f_prausnitzii_original_empirical) + ggtitle('F. prausnitzii full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(o_splanchnicus_original_empirical) + ggtitle('O. splanchnicus full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(o_sp_original_empirical) + ggtitle('Oscillibacter sp full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(p_distasonis_original_empirical) + ggtitle('P. distasonis full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(p_merdae_original_empirical) + ggtitle('P. merdae full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(p_sp_original_empirical) + ggtitle('Phascolarctobacterium sp. full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(p_copri_original_empirical) + ggtitle('P. copri full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(r_bicirculans_original_empirical) + ggtitle('R. bicirculans full empirical SFS (unfolded + Clade Control)')
plot_original_empirical_sfs(r_bromii_original_empirical) + ggtitle('R. bromii full empirical SFS (unfolded + Clade Control)')

## Folded

plot_original_empirical_sfs(fold_sfs(a_muciniphila_original_empirical)) + ggtitle('A. muciniphila full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_finegoldii_original_empirical)) + ggtitle('A. finegoldii full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_onderdonkii_original_empirical)) + ggtitle('A. onderdonkii full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_putredinis_original_empirical)) + ggtitle('A. putredinis full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_shahii_original_empirical)) + ggtitle('A. shahii full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_bacterium_original_empirical)) + ggtitle('B. bacterium full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_caccae_original_empirical)) + ggtitle('B. caccae full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_cellulosilyticus_original_empirical)) + ggtitle('B. cellulosilyticus full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_fragilis_original_empirical)) + ggtitle('B. fragilis full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_massiliensis_original_empirical)) + ggtitle('B. massiliensis full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_ovatus_original_empirical)) + ggtitle('B. ovatus full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_stercoris_original_empirical)) + ggtitle('B. stercoris full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_thetaiotaomicron_original_empirical)) + ggtitle('B. thetaiotaomicron full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_uniformis_original_empirical)) + ggtitle('B. uniformis full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_vulgatus_original_empirical)) + ggtitle('B. vulgatus full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_xylanisolvens_original_empirical)) + ggtitle('B. xylanisolvens full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_intestinihominis_original_empirical)) + ggtitle('B. intestinihominis full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(c_sp_original_empirical)) + ggtitle('Coprococcus sp full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(d_invisus_original_empirical)) + ggtitle('D. invisus full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(e_eligens_original_empirical)) + ggtitle('E. eligens full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(e_rectale_original_empirical)) + ggtitle('E. rectale full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(f_prausnitzii_original_empirical)) + ggtitle('F. prausnitzii full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(o_splanchnicus_original_empirical)) + ggtitle('O. splanchnicus full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(o_sp_original_empirical)) + ggtitle('Oscillibacter sp full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_distasonis_original_empirical)) + ggtitle('P. distasonis full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_merdae_original_empirical)) + ggtitle('P. merdae full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_sp_original_empirical)) + ggtitle('Phascolarctobacterium sp. full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_copri_original_empirical)) + ggtitle('P. copri full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(r_bicirculans_original_empirical)) + ggtitle('R. bicirculans full empirical SFS (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(r_bromii_original_empirical)) + ggtitle('R. bromii full empirical SFS (folded + Clade Control)')


# SFS without Clade Control
a_muciniphila_no_clade_control = read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_no_clade_control/empirical_sfs.txt')
a_finegoldii_no_clade_control = read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_no_clade_control/empirical_sfs.txt')
a_onderdonkii_no_clade_control = read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_no_clade_control/empirical_sfs.txt')
a_putredinis_no_clade_control = read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_no_clade_control/empirical_sfs.txt')
a_shahii_no_clade_control = read_input_sfs_original('../Analysis/Alistipes_shahii_62199_no_clade_control/empirical_sfs.txt')
b_bacterium_no_clade_control = read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_no_clade_control/empirical_sfs.txt')
b_caccae_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_no_clade_control/empirical_sfs.txt')
b_cellulosilyticus_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_no_clade_control/empirical_sfs.txt')
b_fragilis_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_no_clade_control/empirical_sfs.txt')
b_massiliensis_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_no_clade_control/empirical_sfs.txt')
b_ovatus_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_no_clade_control/empirical_sfs.txt')
b_stercoris_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_no_clade_control/empirical_sfs.txt')
b_thetaiotaomicron_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_no_clade_control/empirical_sfs.txt')
b_uniformis_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_no_clade_control/empirical_sfs.txt')
b_vulgatus_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_no_clade_control/empirical_sfs.txt')
b_xylanisolvens_no_clade_control = read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_no_clade_control/empirical_sfs.txt')
b_intestinihominis_no_clade_control = read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_no_clade_control/empirical_sfs.txt')
c_sp_no_clade_control = read_input_sfs_original('../Analysis/Coprococcus_sp_62244_no_clade_control/empirical_sfs.txt')
d_invisus_no_clade_control = read_input_sfs_original('../Analysis/Dialister_invisus_61905_no_clade_control/empirical_sfs.txt')
e_eligens_no_clade_control = read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_no_clade_control/empirical_sfs.txt')
e_rectale_no_clade_control = read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_no_clade_control/empirical_sfs.txt')
f_prausnitzii_no_clade_control = read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_no_clade_control/empirical_sfs.txt')
o_splanchnicus_no_clade_control = read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_no_clade_control/empirical_sfs.txt')
o_sp_no_clade_control = read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_no_clade_control/empirical_sfs.txt')
p_distasonis_no_clade_control = read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_no_clade_control/empirical_sfs.txt')
p_merdae_no_clade_control = read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_no_clade_control/empirical_sfs.txt')
p_sp_no_clade_control = read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_no_clade_control/empirical_sfs.txt')
p_copri_no_clade_control = read_input_sfs_original('../Analysis/Prevotella_copri_61740_no_clade_control/empirical_sfs.txt')
r_bicirculans_no_clade_control = read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_no_clade_control/empirical_sfs.txt')
r_bromii_no_clade_control = read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_no_clade_control/empirical_sfs.txt')

# Plotting SFS without Clade Control
plot_original_empirical_sfs(a_muciniphila_no_clade_control) + ggtitle('A. muciniphila full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(a_finegoldii_no_clade_control) + ggtitle('A. finegoldii full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(a_onderdonkii_no_clade_control) + ggtitle('A. onderdonkii full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(a_putredinis_no_clade_control) + ggtitle('A. putredinis full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(a_shahii_no_clade_control) + ggtitle('A. shahii full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_bacterium_no_clade_control) + ggtitle('B. bacterium full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_caccae_no_clade_control) + ggtitle('B. caccae full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_cellulosilyticus_no_clade_control) + ggtitle('B. cellulosilyticus full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_fragilis_no_clade_control) + ggtitle('B. fragilis full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_massiliensis_no_clade_control) + ggtitle('B. massiliensis full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_ovatus_no_clade_control) + ggtitle('B. ovatus full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_stercoris_no_clade_control) + ggtitle('B. stercoris full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_thetaiotaomicron_no_clade_control) + ggtitle('B. thetaiotaomicron full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_uniformis_no_clade_control) + ggtitle('B. uniformis full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_vulgatus_no_clade_control) + ggtitle('B. vulgatus full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_xylanisolvens_no_clade_control) + ggtitle('B. xylanisolvens full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(b_intestinihominis_no_clade_control) + ggtitle('B. intestinihominis full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(c_sp_no_clade_control) + ggtitle('Coprococcus sp full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(d_invisus_no_clade_control) + ggtitle('D. invisus full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(e_eligens_no_clade_control) + ggtitle('E. eligens full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(e_rectale_no_clade_control) + ggtitle('E. rectale full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(f_prausnitzii_no_clade_control) + ggtitle('F. prausnitzii full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(o_splanchnicus_no_clade_control) + ggtitle('O. splanchnicus full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(o_sp_no_clade_control) + ggtitle('Oscillibacter sp full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(p_distasonis_no_clade_control) + ggtitle('P. distasonis full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(p_merdae_no_clade_control) + ggtitle('P. merdae full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(p_sp_no_clade_control) + ggtitle('Phascolarctobacterium sp. full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(p_copri_no_clade_control) + ggtitle('P. copri full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(r_bicirculans_no_clade_control) + ggtitle('R. bicirculans full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(r_bromii_no_clade_control) + ggtitle('R. bromii full empirical SFS (unfolded with no Clade Control)') +
  scale_x_continuous()

## Folded

# Plotting SFS without Clade Control
plot_original_empirical_sfs(fold_sfs(a_muciniphila_no_clade_control)) + ggtitle('A. muciniphila full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(a_finegoldii_no_clade_control)) + ggtitle('A. finegoldii full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(a_onderdonkii_no_clade_control)) + ggtitle('A. onderdonkii full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(a_putredinis_no_clade_control)) + ggtitle('A. putredinis full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(a_shahii_no_clade_control)) + ggtitle('A. shahii full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_bacterium_no_clade_control)) + ggtitle('B. bacterium full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_caccae_no_clade_control)) + ggtitle('B. caccae full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_cellulosilyticus_no_clade_control)) + ggtitle('B. cellulosilyticus full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_fragilis_no_clade_control)) + ggtitle('B. fragilis full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_massiliensis_no_clade_control)) + ggtitle('B. massiliensis full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_ovatus_no_clade_control)) + ggtitle('B. ovatus full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_stercoris_no_clade_control)) + ggtitle('B. stercoris full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_thetaiotaomicron_no_clade_control)) + ggtitle('B. thetaiotaomicron full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_uniformis_no_clade_control)) + ggtitle('B. uniformis full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_vulgatus_no_clade_control)) + ggtitle('B. vulgatus full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_xylanisolvens_no_clade_control)) + ggtitle('B. xylanisolvens full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(b_intestinihominis_no_clade_control)) + ggtitle('B. intestinihominis full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(c_sp_no_clade_control)) + ggtitle('Coprococcus sp full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(d_invisus_no_clade_control)) + ggtitle('D. invisus full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(e_eligens_no_clade_control)) + ggtitle('E. eligens full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(e_rectale_no_clade_control)) + ggtitle('E. rectale full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(f_prausnitzii_no_clade_control)) + ggtitle('F. prausnitzii full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(o_splanchnicus_no_clade_control)) + ggtitle('O. splanchnicus full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(o_sp_no_clade_control)) + ggtitle('Oscillibacter sp full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(p_distasonis_no_clade_control)) + ggtitle('P. distasonis full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(p_merdae_no_clade_control)) + ggtitle('P. merdae full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(p_sp_no_clade_control)) + ggtitle('Phascolarctobacterium sp. full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(p_copri_no_clade_control)) + ggtitle('P. copri full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(r_bicirculans_no_clade_control)) + ggtitle('R. bicirculans full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()
plot_original_empirical_sfs(fold_sfs(r_bromii_no_clade_control)) + ggtitle('R. bromii full empirical SFS (folded with no Clade Control)') +
  scale_x_continuous()

# Plot SFS downsampled to 14
plot_original_empirical_sfs(fold_sfs(a_muciniphila_14_empirical)) + ggtitle('A. muciniphila downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_finegoldii_14_empirical)) + ggtitle('A. finegoldii downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_onderdonkii_14_empirical)) + ggtitle('A. onderdonkii downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_putredinis_14_empirical)) + ggtitle('A. putredinis downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(a_shahii_14_empirical)) + ggtitle('A. shahii downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_bacterium_14_empirical)) + ggtitle('B. bacterium downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_caccae_14_empirical)) + ggtitle('B. caccae downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_cellulosilyticus_14_empirical)) + ggtitle('B. cellulosilyticus downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_fragilis_14_empirical)) + ggtitle('B. fragilis downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_massiliensis_14_empirical)) + ggtitle('B. massiliensis downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_ovatus_14_empirical)) + ggtitle('B. ovatus downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_stercoris_14_empirical)) + ggtitle('B. stercoris downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_thetaiotaomicron_14_empirical)) + ggtitle('B. thetaiotaomicron downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_uniformis_14_empirical)) + ggtitle('B. uniformis downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_vulgatus_14_empirical)) + ggtitle('B. vulgatus downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_xylanisolvens_14_empirical)) + ggtitle('B. xylanisolvens downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(b_intestinihominis_14_empirical)) + ggtitle('B. intestinihominis downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(c_sp_14_empirical)) + ggtitle('Coprococcus sp downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(d_invisus_14_empirical)) + ggtitle('D. invisus downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(e_eligens_14_empirical)) + ggtitle('E. eligens downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(e_rectale_14_empirical)) + ggtitle('E. rectale downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(f_prausnitzii_14_empirical)) + ggtitle('F. prausnitzii downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(o_splanchnicus_14_empirical)) + ggtitle('O. splanchnicus downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(o_sp_14_empirical)) + ggtitle('Oscillibacter sp downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_distasonis_14_empirical)) + ggtitle('P. distasonis downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_merdae_14_empirical)) + ggtitle('P. merdae downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_sp_14_empirical)) + ggtitle('Phascolarctobacterium sp. downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(p_copri_14_empirical)) + ggtitle('P. copri downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(r_bicirculans_14_empirical)) + ggtitle('R. bicirculans downsampled to 14 (folded + Clade Control)')
plot_original_empirical_sfs(fold_sfs(r_bromii_14_empirical)) + ggtitle('R. bromii full empirical SFS (folded + Clade Control))')


# DFE

# Akkermansia_muciniphila_55290
compare_sfs_cornejo_count('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/') +
  ggtitle('A. muciniphila SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/') +
  ggtitle('A. muciniphila SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/') +
  ggtitle('A. muciniphila SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/') +
  ggtitle('A. muciniphila SFS Comparison')

plot_dfe('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/inferred_DFE.txt') +
  ggtitle('A. muciniphila DFE Comparison')

# Alistipes_finegoldii_56071
compare_sfs_cornejo_count('../Analysis/Alistipes_finegoldii_56071_downsampled_14/') +
  ggtitle('A. finegoldii SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Alistipes_finegoldii_56071_downsampled_14/') +
  ggtitle('A. finegoldii SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Alistipes_finegoldii_56071_downsampled_14/') +
  ggtitle('A. finegoldii SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Alistipes_finegoldii_56071_downsampled_14/') +
  ggtitle('A. finegoldii SFS Comparison')

plot_dfe('../Analysis/Alistipes_finegoldii_56071_downsampled_14/inferred_DFE.txt') +
  ggtitle('A. finegoldii DFE Comparison')

# Alistipes_onderdonkii_55464
compare_sfs_cornejo_count('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/') +
  ggtitle('A. onderdonkii SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/') +
  ggtitle('A. onerdonkii SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/') +
  ggtitle('A. onderdonkii SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/') +
  ggtitle('A. onerdonkii SFS Comparison')

plot_dfe('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/inferred_DFE.txt') +
  ggtitle('A. onderdonkii DFE Comparison')

# Alistipes_putredinis_61533
compare_sfs_cornejo_count('../Analysis/Alistipes_putredinis_61533_downsampled_14/') +
  ggtitle('A. putredinis SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Alistipes_putredinis_61533_downsampled_14/') +
  ggtitle('A. putredinis SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Alistipes_putredinis_61533_downsampled_14/') +
  ggtitle('A. putredinis SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Alistipes_putredinis_61533_downsampled_14/') +
  ggtitle('A. putredinis SFS Comparison')

plot_dfe('../Analysis/Alistipes_putredinis_61533_downsampled_14/inferred_DFE.txt') +
  ggtitle('A. putredinis DFE Comparison')

# Alistipes_shahii_62199
compare_sfs_cornejo_count('../Analysis/Alistipes_shahii_62199_downsampled_14/') +
  ggtitle('A. shahii SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Alistipes_shahii_62199_downsampled_14/') +
  ggtitle('A. shahii SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Alistipes_shahii_62199_downsampled_14/') +
  ggtitle('A. shahii SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Alistipes_shahii_62199_downsampled_14/') +
  ggtitle('A. shahii SFS Comparison')

plot_dfe('../Analysis/Alistipes_shahii_62199_downsampled_14/inferred_DFE.txt') +
  ggtitle('A. shahii DFE Comparison')

# Bacteroidales_bacterium_58650
compare_sfs_cornejo_count('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/') +
  ggtitle('B. bacterium SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/') +
  ggtitle('B. bacterium SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/') +
  ggtitle('B. bacterium SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/') +
  ggtitle('B. bacterium SFS Comparison')

plot_dfe('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. bacterium DFE Comparison')

# Bacteroides_caccae_53434
compare_sfs_cornejo_count('../Analysis/Bacteroides_caccae_53434_downsampled_14/') +
  ggtitle('B. caccae SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_caccae_53434_downsampled_14/') +
  ggtitle('B. caccae SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_caccae_53434_downsampled_14/') +
  ggtitle('B. caccae SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_caccae_53434_downsampled_14/') +
  ggtitle('B. caccae SFS Comparison')

plot_dfe('../Analysis/Bacteroides_caccae_53434_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. caccae DFE Comparison')

# Bacteroides_cellulosilyticus_58046
compare_sfs_cornejo_count('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/') +
  ggtitle('B. cellulosilyticus SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/') +
  ggtitle('B. cellulosilyticus SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/') +
  ggtitle('B. cellulosilyticus SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/') +
  ggtitle('B. cellulosilyticus SFS Comparison')

plot_dfe('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. cellulosilyticus DFE Comparison')

# Bacteroides_fragilis_54507
compare_sfs_cornejo_count('../Analysis/Bacteroides_fragilis_54507_downsampled_14/') +
  ggtitle('B. fragilis SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_fragilis_54507_downsampled_14/') +
  ggtitle('B. fragilis SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_fragilis_54507_downsampled_14/') +
  ggtitle('B. fragilis SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_fragilis_54507_downsampled_14/') +
  ggtitle('B. fragilis SFS Comparison')

plot_dfe('../Analysis/Bacteroides_fragilis_54507_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. fragilis DFE Comparison')

# Bacteroides_massiliensis_44749
# compare_sfs_with_selection_count('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/') +
#   ggtitle('B. massiliensis SFS Comparison')
# 
# compare_sfs_with_selection_proportional('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/') +
#   ggtitle('B. massiliensis SFS Comparison')
#
# plot_dfe('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/inferred_DFE.txt') +
#   ggtitle('B. massiliensis SFS Comparison')

# Bacteroides_ovatus_58035
compare_sfs_cornejo_count('../Analysis/Bacteroides_ovatus_58035_downsampled_14/') +
  ggtitle('B. ovatus SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_ovatus_58035_downsampled_14/') +
  ggtitle('B. ovatus SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_ovatus_58035_downsampled_14/') +
  ggtitle('B. ovatus SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_ovatus_58035_downsampled_14/') +
  ggtitle('B. ovatus SFS Comparison')

plot_dfe('../Analysis/Bacteroides_ovatus_58035_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. ovatus DFE Comparison')

# Bacteroides_stercoris_56735
# compare_sfs_with_selection_count('../Analysis/Bacteroides_stercoris_56735_downsampled_14/') +
#   ggtitle('B. stercoris SFS Comparison')
# 
# compare_sfs_with_selection_proportional('../Analysis/Bacteroides_stercoris_56735_downsampled_14/') +
#   ggtitle('B. stercoris SFS Comparison')
# 
# plot_dfe('../Analysis/Bacteroides_stercoris_56735_downsampled_14/inferred_DFE.txt') +
#  ggtitle('B. stercoris SFS Comparison')

# Bacteroides_thetaiotaomicron_56941
compare_sfs_cornejo_count('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/') +
  ggtitle('B. thetaiotaomicron SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/') +
  ggtitle('B. thetaiotaomicron SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/') +
  ggtitle('B. thetaiotaomicron SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/') +
  ggtitle('B. thetaiotaomicron SFS Comparison')

plot_dfe('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. thetaiotaomicron DFE Comparison')

# Bacteroides_uniformis_57318
compare_sfs_cornejo_count('../Analysis/Bacteroides_uniformis_57318_downsampled_14/') +
  ggtitle('B. uniformis SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_uniformis_57318_downsampled_14/') +
  ggtitle('B. uniformis SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_uniformis_57318_downsampled_14/') +
  ggtitle('B. uniformis SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_uniformis_57318_downsampled_14/') +
  ggtitle('B. uniformis SFS Comparison')

plot_dfe('../Analysis/Bacteroides_uniformis_57318_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. uniformis DFE Comparison')

# Bacteroides_vulgatus_57955
compare_sfs_cornejo_count('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/') +
  ggtitle('B. vulgatus SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/') +
  ggtitle('B. vulgatus SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/') +
  ggtitle('B. vulgatus SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/') +
  ggtitle('B. vulgatus SFS Comparison')

plot_dfe('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. vulgatus DFE Comparison')

# Bacteroides_xylanisolvens_57185
compare_sfs_cornejo_count('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/') +
  ggtitle('B. xylanisolvens SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/') +
  ggtitle('B. xylanisolvens SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/') +
  ggtitle('B. xylanisolvens SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/') +
  ggtitle('B. xylanisolvens SFS Comparison')

plot_dfe('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. xylanisolvens DFE Comparison')

# Barnesiella_intestinihominis_62208
compare_sfs_cornejo_count('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/') +
  ggtitle('B. intestinihominis SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/') +
  ggtitle('B. intestinihominis SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/') +
  ggtitle('B. intestinihominis SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/') +
  ggtitle('B. intestinihominis SFS Comparison')

plot_dfe('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/inferred_DFE.txt') +
  ggtitle('B. intestinihominis DFE Comparison')

# Coprococcus_sp_62244
# compare_sfs_with_selection_count('../Analysis/Coprococcus_sp_62244_downsampled_14/') +
#   ggtitle('Coprococcus sp. SFS Comparison')
# 
# compare_sfs_with_selection_proportional('../Analysis/Coprococcus_sp_62244_downsampled_14/') +
#   ggtitle('Coprococcus sp. SFS Comparison')
# 
# plot_dfe('../Analysis/Coprococcus_sp_62244_downsampled_14/inferred_DFE.txt') +
#   ggtitle('Coprococcus sp. SFS Comparison')

# Dialister_invisus_61905
compare_sfs_cornejo_count('../Analysis/Dialister_invisus_61905_downsampled_14/') +
  ggtitle('D. invisus SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Dialister_invisus_61905_downsampled_14/') +
  ggtitle('D. invisus SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Dialister_invisus_61905_downsampled_14/') +
  ggtitle('D. invisus SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Dialister_invisus_61905_downsampled_14/') +
  ggtitle('D. invisus SFS Comparison')

plot_dfe('../Analysis/Dialister_invisus_61905_downsampled_14/inferred_DFE.txt') +
  ggtitle('D. invisus DFE Comparison')

# Eubacterium_eligens_61678
# compare_sfs_with_selection_count('../Analysis/Eubacterium_eligens_61678_downsampled_14/') +
#   ggtitle('E. eligens SFS Comparison')
# 
# compare_sfs_with_selection_proportional('../Analysis/Eubacterium_eligens_61678_downsampled_14/') +
#   ggtitle('E. eligens SFS Comparison')
# 
# plot_dfe('../Analysis/Eubacterium_eligens_61678_downsampled_14/inferred_DFE.txt') +
#   ggtitle('E. eligens SFS Comparison')

# Eubacterium_rectale_56927
compare_sfs_cornejo_count('../Analysis/Eubacterium_rectale_56927_downsampled_14/') +
  ggtitle('E. rectale SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Eubacterium_rectale_56927_downsampled_14/') +
  ggtitle('E. rectale SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Eubacterium_rectale_56927_downsampled_14/') +
  ggtitle('E. rectale SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Eubacterium_rectale_56927_downsampled_14/') +
  ggtitle('E. rectale SFS Comparison')

plot_dfe('../Analysis/Eubacterium_rectale_56927_downsampled_14/inferred_DFE.txt') +
  ggtitle('E. rectale DFE Comparison')

# Faecalibacterium_prausnitzii_57453
compare_sfs_cornejo_count('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/') +
  ggtitle('F. prausnitzii SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/') +
  ggtitle('F. prausnitzii SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/') +
  ggtitle('F. prausnitzii SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/') +
  ggtitle('F. prausnitzii SFS Comparison')

plot_dfe('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/inferred_DFE.txt') +
  ggtitle('F. prausnitzii DFE Comparison')

# Odoribacter_splanchnicus_62174
compare_sfs_cornejo_count('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/') +
  ggtitle('O. splanchnicus SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/') +
  ggtitle('O. splanchnicus SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/') +
  ggtitle('O. splanchnicus SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/') +
  ggtitle('O. splanchnicus SFS Comparison')

plot_dfe('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/inferred_DFE.txt') +
  ggtitle('O. splanchnicus DFE Comparison')

# Oscillibacter_sp_60799
compare_sfs_cornejo_count('../Analysis/Oscillibacter_sp_60799_downsampled_14/') +
  ggtitle('Oscillibacter sp. SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Oscillibacter_sp_60799_downsampled_14/') +
  ggtitle('Oscillibacter sp. SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Oscillibacter_sp_60799_downsampled_14/') +
  ggtitle('Oscillibacter sp. SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Oscillibacter_sp_60799_downsampled_14/') +
  ggtitle('Oscillibacter sp. SFS Comparison')

plot_dfe('../Analysis/Oscillibacter_sp_60799_downsampled_14/inferred_DFE.txt') +
  ggtitle('Oscillibacter sp. DFE Comparison')

# Parabacteroides_distasonis_56985
compare_sfs_cornejo_count('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/') +
  ggtitle('P. distasonis SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/') +
  ggtitle('P. distasonis SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/') +
  ggtitle('P. distasonis SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/') +
  ggtitle('P. distasonis SFS Comparison')

plot_dfe('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/inferred_DFE.txt') +
  ggtitle('P. distasonis DFE Comparison')

# Parabacteroides_merdae_56972
compare_sfs_cornejo_count('../Analysis/Parabacteroides_merdae_56972_downsampled_14/') +
  ggtitle('P. merdae SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Parabacteroides_merdae_56972_downsampled_14/') +
  ggtitle('P. merdae SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Parabacteroides_merdae_56972_downsampled_14/') +
  ggtitle('P. merdae SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Parabacteroides_merdae_56972_downsampled_14/') +
  ggtitle('P. merdae SFS Comparison')

plot_dfe('../Analysis/Parabacteroides_merdae_56972_downsampled_14/inferred_DFE.txt') +
  ggtitle('P. merdae DFE Comparison')

# Phascolarctobacterium_sp_59817
compare_sfs_cornejo_count('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/') +
  ggtitle('Phascolarctobacterium sp. SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/') +
  ggtitle('Phascolarctobacterium sp. SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/') +
  ggtitle('Phascolarctobacterium sp. SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/') +
  ggtitle('Phascolarctobacterium sp. SFS Comparison')

plot_dfe('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/inferred_DFE.txt') +
  ggtitle('Phascolarctobacterium sp. DFE Comparison')

# Prevotella_copri_61740
compare_sfs_cornejo_count('../Analysis/Prevotella_copri_61740_downsampled_14/') +
  ggtitle('P. copri SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Prevotella_copri_61740_downsampled_14/') +
  ggtitle('P. copri SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Prevotella_copri_61740_downsampled_14/') +
  ggtitle('P. copri SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Prevotella_copri_61740_downsampled_14/') +
  ggtitle('P. copri SFS Comparison')

plot_dfe('../Analysis/Prevotella_copri_61740_downsampled_14/inferred_DFE.txt') +
  ggtitle('P. copri DFE Comparison')

# Ruminococcus_bicirculans_59300
# compare_sfs_with_selection_count('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/') +
#   ggtitle('R. bicirculans SFS Comparison')
# 
# compare_sfs_with_selection_proportional('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/') +
#   ggtitle('R. bicirculans SFS Comparison')
# 
# plot_dfe('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/inferred_DFE.txt') +
#   ggtitle('R. bicirculans SFS Comparison')
# 
# Ruminococcus_bromii_62047
compare_sfs_cornejo_count('../Analysis/Ruminococcus_bromii_62047_downsampled_14/') +
  ggtitle('R. bromii SFS Comparison')

compare_sfs_cornejo_proportional('../Analysis/Ruminococcus_bromii_62047_downsampled_14/') +
  ggtitle('R. bromii SFS Comparison')

compare_sfs_with_selection_count('../Analysis/Ruminococcus_bromii_62047_downsampled_14/') +
  ggtitle('R. bromii SFS Comparison')

compare_sfs_with_selection_proportional('../Analysis/Ruminococcus_bromii_62047_downsampled_14/') +
  ggtitle('R. bromii SFS Comparison')

plot_dfe('../Analysis/Ruminococcus_bromii_62047_downsampled_14/inferred_DFE.txt') +
  ggtitle('R. bromii DFE Comparison')

##UHGG


# Bacteroides_A_coprocola

b_a_coprocola_2 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/2_output_sfs.txt')
b_a_coprocola_3 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/3_output_sfs.txt')
b_a_coprocola_4 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/4_output_sfs.txt')
b_a_coprocola_5 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/5_output_sfs.txt')
b_a_coprocola_6 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/6_output_sfs.txt')
b_a_coprocola_7 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/7_output_sfs.txt')
b_a_coprocola_8 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/8_output_sfs.txt')
b_a_coprocola_9 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/9_output_sfs.txt')
b_a_coprocola_10 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/10_output_sfs.txt')
b_a_coprocola_11 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/11_output_sfs.txt')
b_a_coprocola_12 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/12_output_sfs.txt')
b_a_coprocola_13 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/13_output_sfs.txt')
b_a_coprocola_14 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/14_output_sfs.txt')
b_a_coprocola_15 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/15_output_sfs.txt')
b_a_coprocola_16 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/16_output_sfs.txt')
b_a_coprocola_17 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/17_output_sfs.txt')
b_a_coprocola_18 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/18_output_sfs.txt')
b_a_coprocola_19 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_A_coprocola/19_output_sfs.txt')

b_a_coprocola = b_a_coprocola_2 +
  b_a_coprocola_3 +
  b_a_coprocola_4 +
  b_a_coprocola_5 +
  b_a_coprocola_6 +
  b_a_coprocola_7 +
  b_a_coprocola_8 +
  b_a_coprocola_9 +
  b_a_coprocola_10 +
  b_a_coprocola_11 +
  b_a_coprocola_12 +
  b_a_coprocola_13 +
  b_a_coprocola_14 +
  b_a_coprocola_15 +
  b_a_coprocola_16 +
  b_a_coprocola_17 +
  b_a_coprocola_18 +
  b_a_coprocola_19
  
plot_original_empirical_sfs(b_a_coprocola)

plot_original_empirical_sfs(b_a_coprocola) + xlim(-1.5, 20) +
  ggtitle('Bacteroides coprocola [A] synonymous SFS (Isolates w/ Clade Control)')

b_a_coprocola_UHGG_one_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_A_coprocola/one_epoch_demography.txt')
b_a_coprocola_UHGG_two_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_A_coprocola/two_epoch_demography.txt')

compare_sfs(fold_sfs(b_a_coprocola)[-1], b_a_coprocola_UHGG_one_epoch, b_a_coprocola_UHGG_two_epoch) +
  ggtitle('B. coprocola [A], UHGG Isolate SFS Comparison (w/ Clade Control)')

compare_sfs(fold_sfs(b_a_coprocola)[-1], b_a_coprocola_UHGG_one_epoch, b_a_coprocola_UHGG_two_epoch)  +
  xlim(-1.5, 20.5) +
  ggtitle('B. coprocola [A], UHGG Isolate SFS Comparison (w/ Clade Control)')

compare_sfs(proportional_sfs(fold_sfs(b_a_coprocola)[-1]), proportional_sfs(b_a_coprocola_UHGG_one_epoch), proportional_sfs(b_a_coprocola_UHGG_two_epoch))  +
  ggtitle('B. coprocola [A], UHGG Isolate SFS Comparison (w/ Clade Control)')


# B. eggerthii

b_eggerthii_1 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/1_output_sfs.txt')
b_eggerthii_2 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/2_output_sfs.txt')
b_eggerthii_3 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/3_output_sfs.txt')
b_eggerthii_4 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/4_output_sfs.txt')
b_eggerthii_5 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/5_output_sfs.txt')
b_eggerthii_6 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/6_output_sfs.txt')
b_eggerthii_7 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/7_output_sfs.txt')
b_eggerthii_8 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/8_output_sfs.txt')
b_eggerthii_9 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/9_output_sfs.txt')
b_eggerthii_10 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/10_output_sfs.txt')
b_eggerthii_11 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/11_output_sfs.txt')
b_eggerthii_13 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/13_output_sfs.txt')
b_eggerthii_14 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/14_output_sfs.txt')
b_eggerthii_15 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/15_output_sfs.txt')
b_eggerthii_16 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_eggerthii/16_output_sfs.txt')

b_eggerthii = b_eggerthii_1 +
  b_eggerthii_2 +
  b_eggerthii_3 +
  b_eggerthii_4 +
  b_eggerthii_5 +
  b_eggerthii_6 +
  b_eggerthii_7 +
  b_eggerthii_8 +
  b_eggerthii_9 +
  b_eggerthii_10 +
  b_eggerthii_11 +
  b_eggerthii_13 +
  b_eggerthii_14 +
  b_eggerthii_15 +
  b_eggerthii_16

plot_original_empirical_sfs(b_eggerthii)

plot_original_empirical_sfs(b_eggerthii) + xlim(-1.5, 20) + 
  ggtitle('B. eggerthii synonymous SFS (Isolates w/ Clade control)')

plot_original_empirical_sfs(proportional_sfs(b_eggerthii)) + 
  xlim(-1.5, 20) +
  ggtitle('B. eggerthii synonymous SFS (Isolates w/ Clade control)') +
  ylab('Proportion of Segregating Sites')

b_eggerthii_UHGG_one_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_eggerthii/one_epoch_demography.txt')
b_eggerthii_UHGG_two_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_eggerthii/two_epoch_demography.txt')
compare_sfs(fold_sfs(b_eggerthii)[-1], b_eggerthii_UHGG_one_epoch, b_eggerthii_UHGG_two_epoch)

compare_sfs(fold_sfs(b_eggerthii)[-1], b_eggerthii_UHGG_one_epoch, b_eggerthii_UHGG_two_epoch)  +
  xlim(-1.5, 20.5) +
  ggtitle('B. eggerthii, UHGG Isolate SFS Comparison (w/ Clade Control)')

compare_sfs(proportional_sfs(fold_sfs(b_eggerthii)[-1]), proportional_sfs(b_eggerthii_UHGG_one_epoch), proportional_sfs(b_eggerthii_UHGG_two_epoch))  +
  xlim(-1.5, 20.5) +
  ggtitle('B. eggerthii, UHGG Isolate SFS Comparison (w/ Clade Control)')


compare_sfs(fold_sfs(b_eggerthii), b_eggerthii_UHGG_one_epoch, b_eggerthii_UHGG_two_epoch)  +
  xlim(-0.5, 50) +
  ggtitle('B. fragilis, UHGG Isolate SFS Comparison (w/ Clade Control)')

# B. fragilis UHGG test
b_fragilis_1 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_fragilis/1_output_sfs.txt')
b_fragilis_2 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_fragilis/2_output_sfs.txt')
b_fragilis_3 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_fragilis/3_output_sfs.txt')
b_fragilis_4 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_fragilis/4_output_sfs.txt')
b_fragilis_5 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_fragilis/5_output_sfs.txt')
b_fragilis = b_fragilis_1 + b_fragilis_2 + b_fragilis_3 + b_fragilis_4 + b_fragilis_5

plot_original_empirical_sfs(b_fragilis)

plot_original_empirical_sfs(b_fragilis) + xlim(-1.5, 20) + 
  ggtitle('B. fragilis synonymous SFS (Isolates w/ Clade control)')

plot_original_empirical_sfs(proportional_sfs(b_fragilis)) + 
  xlim(-1.5, 20) +
  ggtitle('B. fragilis synonymous SFS (Isolates w/ Clade control)') +
  ylab('Proportion of Segregating Sites')

b_fragilis_UHGG_one_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt')
b_fragilis_UHGG_two_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt')
b_fragilis  = b_fragilis[-1]
compare_sfs(fold_sfs(b_fragilis), b_fragilis_UHGG_one_epoch, b_fragilis_UHGG_two_epoch)

compare_sfs(fold_sfs(b_fragilis), b_fragilis_UHGG_one_epoch, b_fragilis_UHGG_two_epoch)  +
  xlim(-1.5, 20.5) +
  ggtitle('B. fragilis, UHGG Isolate SFS Comparison (w/ Clade Control)')

compare_sfs(proportional_sfs(fold_sfs(b_fragilis)), proportional_sfs(b_fragilis_UHGG_one_epoch), proportional_sfs(b_fragilis_UHGG_two_epoch))  +
  xlim(-1.5, 20.5) +
  ggtitle('B. fragilis, UHGG Isolate SFS Comparison (w/ Clade Control)')


compare_sfs(fold_sfs(b_fragilis), b_fragilis_UHGG_one_epoch, b_fragilis_UHGG_two_epoch)  +
  xlim(-0.5, 50) +
  ggtitle('B. fragilis, UHGG Isolate SFS Comparison (w/ Clade Control)')

# B. stercoris

b_stercoris_1 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/1_output_sfs.txt')
b_stercoris_2 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/2_output_sfs.txt')
b_stercoris_3 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/3_output_sfs.txt')
b_stercoris_4 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/4_output_sfs.txt')
b_stercoris_5 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/5_output_sfs.txt')
b_stercoris_6 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/6_output_sfs.txt')
b_stercoris_7 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/7_output_sfs.txt')
b_stercoris_8 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/8_output_sfs.txt')
b_stercoris_9 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/9_output_sfs.txt')
b_stercoris_19 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/19_output_sfs.txt')
b_stercoris_29 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/29_output_sfs.txt')
b_stercoris_39 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/39_output_sfs.txt')
b_stercoris_53 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/53_output_sfs.txt')
b_stercoris_54 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/54_output_sfs.txt')
b_stercoris_55 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/55_output_sfs.txt')
b_stercoris_56 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/56_output_sfs.txt')
b_stercoris_57 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/57_output_sfs.txt')
b_stercoris_58 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/58_output_sfs.txt')
b_stercoris_59 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/59_output_sfs.txt')
b_stercoris_60 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/60_output_sfs.txt')
b_stercoris_62 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/62_output_sfs.txt')
b_stercoris_63 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/63_output_sfs.txt')
b_stercoris_64 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/64_output_sfs.txt')
b_stercoris_66 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/66_output_sfs.txt')
b_stercoris_67 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/67_output_sfs.txt')
b_stercoris_68 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/68_output_sfs.txt')
b_stercoris_69 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/69_output_sfs.txt')
b_stercoris_70 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/70_output_sfs.txt')
b_stercoris_71 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/71_output_sfs.txt')
b_stercoris_72 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/72_output_sfs.txt')
b_stercoris_73 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/73_output_sfs.txt')
b_stercoris_74 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/74_output_sfs.txt')
b_stercoris_75 = read_input_sfs_original('../Data/UHGG/UHGG_Bacteroides_stercoris/75_output_sfs.txt')

b_stercoris = b_stercoris_1 +
  b_stercoris_2 +
  b_stercoris_3 +
  b_stercoris_4 +
  b_stercoris_5 +
  b_stercoris_6 +
  b_stercoris_7 +
  b_stercoris_8 +
  b_stercoris_9 +
  b_stercoris_19 +
  b_stercoris_29 +
  b_stercoris_39 +
  b_stercoris_53 +
  b_stercoris_54 +
  b_stercoris_55 +
  b_stercoris_56 +
  b_stercoris_57 +
  b_stercoris_58 +
  b_stercoris_59 +
  b_stercoris_60 +
  b_stercoris_62 +
  b_stercoris_63 +
  b_stercoris_64 +
  b_stercoris_66 +
  b_stercoris_67 +
  b_stercoris_68 +
  b_stercoris_69 +
  b_stercoris_70 +
  b_stercoris_71 +
  b_stercoris_72 +
  b_stercoris_73 +
  b_stercoris_74 +
  b_stercoris_75
  
plot_original_empirical_sfs(b_stercoris)

plot_original_empirical_sfs(b_stercoris) + xlim(-1.5, 20) + 
  ggtitle('B. stercoris synonymous SFS (Isolates w/ Clade control)')

plot_original_empirical_sfs(proportional_sfs(b_stercoris)) + 
  xlim(-1.5, 20) +
  ggtitle('B. stercoris synonymous SFS (Isolates w/ Clade control)') +
  ylab('Proportion of Segregating Sites')

b_stercoris_UHGG_one_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_stercoris/one_epoch_demography.txt')
b_stercoris_UHGG_two_epoch = sfs_from_demography('../Data/UHGG/UHGG_Bacteroides_stercoris/two_epoch_demography.txt')
compare_sfs(fold_sfs(b_stercoris)[-1], b_stercoris_UHGG_one_epoch, b_stercoris_UHGG_two_epoch)

compare_sfs(fold_sfs(b_stercoris)[-1], b_stercoris_UHGG_one_epoch, b_stercoris_UHGG_two_epoch)  +
  xlim(-1.5, 20.5) +
  ggtitle('B. stercoris, UHGG Isolate SFS Comparison (w/ Clade Control)')

compare_sfs(proportional_sfs(fold_sfs(b_stercoris)[-1]), proportional_sfs(b_stercoris_UHGG_one_epoch), proportional_sfs(b_stercoris_UHGG_two_epoch))  +
  xlim(-1.5, 20.5) +
  ggtitle('B. stercoris, UHGG Isolate SFS Comparison (w/ Clade Control)')


compare_sfs(fold_sfs(b_stercoris), b_stercoris_UHGG_one_epoch, b_stercoris_UHGG_two_epoch)  +
  xlim(-0.5, 50) +
  ggtitle('B. stercoris, UHGG Isolate SFS Comparison (w/ Clade Control)')

# F. prausnitzii

f_prausnitzii_1 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/1_output_sfs.txt')
f_prausnitzii_2 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/2_output_sfs.txt')
f_prausnitzii_3 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/3_output_sfs.txt')
f_prausnitzii_4 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/4_output_sfs.txt')
f_prausnitzii_5 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/5_output_sfs.txt')
f_prausnitzii_6 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/6_output_sfs.txt')
f_prausnitzii_7 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/7_output_sfs.txt')
f_prausnitzii_8 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/8_output_sfs.txt')
f_prausnitzii_9 = read_input_sfs_original('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/9_output_sfs.txt')

f_prausnitzii = f_prausnitzii_1 +
  f_prausnitzii_2 +
  f_prausnitzii_3 +
  f_prausnitzii_4 +
  f_prausnitzii_5 +
  f_prausnitzii_6 +
  f_prausnitzii_7 +
  f_prausnitzii_8 +
  f_prausnitzii_9


  
plot_original_empirical_sfs(f_prausnitzii) + xlim(-1.5, 20) + 
  ggtitle('F. prausnitzii [K] synonymous SFS (Isolates w/ Clade control)')

plot_original_empirical_sfs(proportional_sfs(f_prausnitzii)) + 
  xlim(-1.5, 20) +
  ggtitle('F. prausnitzii [K] synonymous SFS (Isolates w/ Clade control)') +
  ylab('Proportion of Segregating Sites')


f_prausnitzii_UHGG_one_epoch = sfs_from_demography('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/one_epoch_demography.txt')
f_prausnitzii_UHGG_two_epoch = sfs_from_demography('../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/two_epoch_demography.txt')

compare_sfs(fold_sfs(f_prausnitzii)[-1], f_prausnitzii_UHGG_one_epoch, f_prausnitzii_UHGG_two_epoch) +
  xlim(-0.5, 20.5) +
  ggtitle('F. prausnitzii [K] SFS Comparison, Isolates w/ Clade Control')

compare_sfs(proportional_sfs(fold_sfs(f_prausnitzii))[-1], proportional_sfs(f_prausnitzii_UHGG_one_epoch), proportional_sfs(f_prausnitzii_UHGG_two_epoch)) +
  xlim(-0.5, 20.5) +
  ggtitle('F. prausnitzii [K] SFS Comparison, Isolates w/ Clade Control')


# P. copri

p_copri_1 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/1_output_sfs.txt')
p_copri_2 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/2_output_sfs.txt')
p_copri_3 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/3_output_sfs.txt')
p_copri_4 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/4_output_sfs.txt')
p_copri_5 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/5_output_sfs.txt')
p_copri_6 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/6_output_sfs.txt')
p_copri_7 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/7_output_sfs.txt')
p_copri_8 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/8_output_sfs.txt')
p_copri_9 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/9_output_sfs.txt')
p_copri_59 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/59_output_sfs.txt')
p_copri_68 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/68_output_sfs.txt')
p_copri_69 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/69_output_sfs.txt')
p_copri_78 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/78_output_sfs.txt')
p_copri_79 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/79_output_sfs.txt')
p_copri_85 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/85_output_sfs.txt')
p_copri_86 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/86_output_sfs.txt')
p_copri_87 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/87_output_sfs.txt')
p_copri_88 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/88_output_sfs.txt')
p_copri_89 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/89_output_sfs.txt')
p_copri_90 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/90_output_sfs.txt')
p_copri_91 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/91_output_sfs.txt')
p_copri_92 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/92_output_sfs.txt')
p_copri_93 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/93_output_sfs.txt')
p_copri_94 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/94_output_sfs.txt')
p_copri_95 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/95_output_sfs.txt')
p_copri_96 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/96_output_sfs.txt')
p_copri_97 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/97_output_sfs.txt')
p_copri_98 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/98_output_sfs.txt')
p_copri_99 = read_input_sfs_original('../Data/UHGG/UHGG_Prevotella_copri/99_output_sfs.txt')

p_copri = p_copri_1 + 
  p_copri_2 +
  p_copri_3 +
  p_copri_4 +
  p_copri_5 +
  p_copri_6 +
  p_copri_7 +
  p_copri_8 +
  p_copri_9 +
  p_copri_59 +
  p_copri_68 +
  p_copri_69 +
  p_copri_78 +
  p_copri_79 +
  p_copri_85 +
  p_copri_86 +
  p_copri_87 +
  p_copri_88 +
  p_copri_89 +
  p_copri_90 +
  p_copri_91 +
  p_copri_92 +
  p_copri_93 +
  p_copri_94 +
  p_copri_95 +
  p_copri_96 +
  p_copri_97 +
  p_copri_98 +
  p_copri_99

plot_original_empirical_sfs(p_copri) + xlim(-1.5, 20) + 
  ggtitle('P. copri synonymous SFS (Isolates w/ Clade control)')

plot_original_empirical_sfs(proportional_sfs(p_copri)) + 
  xlim(-1.5, 20) +
  ggtitle('P. copri synonymous SFS (Isolates w/ Clade control)') +
  ylab('Proportion of Segregating Sites')


p_copri = fold_sfs(p_copri)
p_copri = p_copri[-1]
p_copri_UHGG_one_epoch = sfs_from_demography('../Data/UHGG/UHGG_Prevotella_copri/one_epoch_demography.txt')
p_copri_UHGG_two_epoch = sfs_from_demography('../Data/UHGG/UHGG_Prevotella_copri/two_epoch_demography.txt')

p_copri_UHGG_two_epoch = numeric(length(p_copri))
compare_sfs(p_copri, p_copri_UHGG_one_epoch, p_copri_UHGG_two_epoch) +
  xlim(-0.5, 20.5)  +
  ggtitle('P. copri SFS Comparison, Isolates w/ Clade Control')

compare_sfs(proportional_sfs(p_copri), proportional_sfs(p_copri_UHGG_one_epoch), proportional_sfs(p_copri_UHGG_two_epoch)) +
  xlim(-0.5, 20.5) +
  ggtitle('P. copri SFS Comparison, Isolates w/ Clade Control')


## Isolate downsampling

# A. muciniphila B

a_muciniphila_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/downsampled_syn_sfs.txt'
))

a_muciniphila_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt'
))

a_muciniphila_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt'
)
a_muciniphila_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt'
)

compare_isolate_sfs(a_muciniphila_14_hmp[-1], 
                    a_muciniphila_14_isolate[-1],
                    a_muciniphila_isolate_one_epoch,
                    a_muciniphila_isolate_two_epoch) +
  ggtitle('A. muciniphila downsampled SFS comparison')

# A. finegoldii

a_finegoldii_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/downsampled_syn_sfs.txt'
))

a_finegoldii_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt'
))

a_finegoldii_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt'
)
a_finegoldii_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt'
)

compare_isolate_sfs(a_finegoldii_14_hmp[-1], 
                    a_finegoldii_14_isolate[-1],
                    a_finegoldii_isolate_one_epoch,
                    a_finegoldii_isolate_two_epoch) +
  ggtitle('A. finegoldii downsampled SFS comparison')

# A. onderdonkii

a_onderdonkii_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/downsampled_syn_sfs.txt'
))

a_onderdonkii_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt'
))

a_onderdonkii_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt'
)
a_onderdonkii_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt'
)

compare_isolate_sfs(a_onderdonkii_14_hmp[-1], 
                    a_onderdonkii_14_isolate[-1],
                    a_onderdonkii_isolate_one_epoch,
                    a_onderdonkii_isolate_two_epoch) +
  ggtitle('A. onderdonkii downsampled SFS comparison')

# A. putredinis

a_putredinis_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/downsampled_syn_sfs.txt'
))

a_putredinis_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt'
))

a_putredinis_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt'
)
a_putredinis_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt'
)

compare_isolate_sfs(a_putredinis_14_hmp[-1], 
                    a_putredinis_14_isolate[-1],
                    a_putredinis_isolate_one_epoch,
                    a_putredinis_isolate_two_epoch) +
  ggtitle('A. putredinis downsampled SFS comparison')

# A. shahii

a_shahii_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Alistipes_shahii_62199_downsampled_14/downsampled_syn_sfs.txt'
))

a_shahii_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt'
))

a_shahii_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt'
)
a_shahii_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt'
)

compare_isolate_sfs(a_shahii_14_hmp[-1], 
                    a_shahii_14_isolate[-1],
                    a_shahii_isolate_one_epoch,
                    a_shahii_isolate_two_epoch) +
  ggtitle('A. shahii downsampled SFS comparison')

# B. fragilis

b_fragilis_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/downsampled_syn_sfs.txt'
))

b_fragilis_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_sfs.txt'
))

b_fragilis_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/one_epoch_demography.txt'
)
b_fragilis_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_fragilis/two_epoch_demography.txt'
)

compare_isolate_sfs(b_fragilis_14_hmp[-1], 
                    b_fragilis_14_isolate[-1],
                    b_fragilis_isolate_one_epoch,
                    b_fragilis_isolate_two_epoch) +
  ggtitle('B. fragilis downsampled SFS comparison')

# B. ovatus

b_ovatus_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/downsampled_syn_sfs.txt'
))

b_ovatus_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_ovatus/downsampled_sfs.txt'
))

b_ovatus_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_ovatus/one_epoch_demography.txt'
)
b_ovatus_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_ovatus/two_epoch_demography.txt'
)

compare_isolate_sfs(b_ovatus_14_hmp[-1], 
                    b_ovatus_14_isolate[-1],
                    b_ovatus_isolate_one_epoch,
                    b_ovatus_isolate_two_epoch) +
  ggtitle('B. ovatus downsampled SFS comparison')

# B. stercoris

b_stercoris_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/downsampled_syn_sfs.txt'
))

b_stercoris_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_stercoris/downsampled_sfs.txt'
))

b_stercoris_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_stercoris/one_epoch_demography.txt'
)
b_stercoris_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_stercoris/two_epoch_demography.txt'
)

compare_isolate_sfs(b_stercoris_14_hmp[-1], 
                    b_stercoris_14_isolate[-1],
                    b_stercoris_isolate_one_epoch,
                    b_stercoris_isolate_two_epoch) +
  ggtitle('B. stercoris downsampled SFS comparison')

# B. thetaiotaomicron

b_thetaiotaomicron_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/downsampled_syn_sfs.txt'
))

b_thetaiotaomicron_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/downsampled_sfs.txt'
))

b_thetaiotaomicron_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/one_epoch_demography.txt'
)
b_thetaiotaomicron_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_thetaiotaomicron/two_epoch_demography.txt'
)

compare_isolate_sfs(b_thetaiotaomicron_14_hmp[-1], 
                    b_thetaiotaomicron_14_isolate[-1],
                    b_thetaiotaomicron_isolate_one_epoch,
                    b_thetaiotaomicron_isolate_two_epoch) +
  ggtitle('B. thetaiotaomicron downsampled SFS comparison')

# B. xylanisolvens

b_xylanisolvens_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/downsampled_syn_sfs.txt'
))

b_xylanisolvens_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Bacteroides_xylanisolvens/downsampled_sfs.txt'
))

b_xylanisolvens_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_xylanisolvens/one_epoch_demography.txt'
)
b_xylanisolvens_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Bacteroides_xylanisolvens/two_epoch_demography.txt'
)

compare_isolate_sfs(b_xylanisolvens_14_hmp[-1], 
                    b_xylanisolvens_14_isolate[-1],
                    b_xylanisolvens_isolate_one_epoch,
                    b_xylanisolvens_isolate_two_epoch) +
  ggtitle('B. xylanisolvens downsampled SFS comparison')

# B. intestinihominis

b_intestinihominis_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/downsampled_syn_sfs.txt'
))

b_intestinihominis_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Barnesiella_intestinihominis/downsampled_sfs.txt'
))

b_intestinihominis_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Barnesiella_intestinihominis/one_epoch_demography.txt'
)
b_intestinihominis_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Barnesiella_intestinihominis/two_epoch_demography.txt'
)

compare_isolate_sfs(b_intestinihominis_14_hmp[-1], 
                    b_intestinihominis_14_isolate[-1],
                    b_intestinihominis_isolate_one_epoch,
                    b_intestinihominis_isolate_two_epoch) +
  ggtitle('B. intestinihominis downsampled SFS comparison')

# D. invisus

d_invisus_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Dialister_invisus_61905_downsampled_14/downsampled_syn_sfs.txt'
))

d_invisus_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Dialister_invisus/downsampled_sfs.txt'
))

d_invisus_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Dialister_invisus/one_epoch_demography.txt'
)
d_invisus_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Dialister_invisus/two_epoch_demography.txt'
)

compare_isolate_sfs(d_invisus_14_hmp[-1], 
                    d_invisus_14_isolate[-1],
                    d_invisus_isolate_one_epoch,
                    d_invisus_isolate_two_epoch) +
  ggtitle('D. invisus downsampled SFS comparison')

# F. prausnitzii (K)

f_prausnitzii_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/downsampled_syn_sfs.txt'
))

f_prausnitzii_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/downsampled_sfs.txt'
))

f_prausnitzii_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/one_epoch_demography.txt'
)
f_prausnitzii_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/two_epoch_demography.txt'
)

compare_isolate_sfs(f_prausnitzii_14_hmp[-1], 
                    f_prausnitzii_14_isolate[-1],
                    f_prausnitzii_isolate_one_epoch,
                    f_prausnitzii_isolate_two_epoch) +
  ggtitle('F. prausnitzii downsampled SFS comparison')

# O. splanchnicus

o_splanchnicus_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/downsampled_syn_sfs.txt'
))

o_splanchnicus_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Odoribacter_splanchnicus/downsampled_sfs.txt'
))

o_splanchnicus_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Odoribacter_splanchnicus/one_epoch_demography.txt'
)
o_splanchnicus_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Odoribacter_splanchnicus/two_epoch_demography.txt'
)

compare_isolate_sfs(o_splanchnicus_14_hmp[-1], 
                    o_splanchnicus_14_isolate[-1],
                    o_splanchnicus_isolate_one_epoch,
                    o_splanchnicus_isolate_two_epoch) +
  ggtitle('O. splanchnicus downsampled SFS comparison')

# P. merdae

p_merdae_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/downsampled_syn_sfs.txt'
))

p_merdae_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Parabacteroides_merdae/downsampled_sfs.txt'
))

p_merdae_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Parabacteroides_merdae/one_epoch_demography.txt'
)
p_merdae_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Parabacteroides_merdae/two_epoch_demography.txt'
)

compare_isolate_sfs(p_merdae_14_hmp[-1], 
                    p_merdae_14_isolate[-1],
                    p_merdae_isolate_one_epoch,
                    p_merdae_isolate_two_epoch) +
  ggtitle('P. merdae downsampled SFS comparison')

# P. copri

p_copri_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Prevotella_copri_61740_downsampled_14/downsampled_syn_sfs.txt'
))

p_copri_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Prevotella_copri/downsampled_sfs.txt'
))

p_copri_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Prevotella_copri/one_epoch_demography.txt'
)
p_copri_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Prevotella_copri/two_epoch_demography.txt'
)

compare_isolate_sfs(p_copri_14_hmp[-1], 
                    p_copri_14_isolate[-1],
                    p_copri_isolate_one_epoch,
                    p_copri_isolate_two_epoch) +
  ggtitle('P. copri downsampled SFS comparison')


# R. E bromii B

r_bromii_14_hmp = fold_sfs(read_input_sfs_original(
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/downsampled_syn_sfs.txt'
))

r_bromii_14_isolate = fold_sfs(read_input_sfs_original(
  '../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/downsampled_sfs.txt'
))

r_bromii_isolate_one_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/one_epoch_demography.txt'
)
r_bromii_isolate_two_epoch = sfs_from_demography(
  '../Data/UHGG/UHGG_Ruminococcus_E_bromii_B/two_epoch_demography.txt'
)

compare_isolate_sfs(r_bromii_14_hmp[-1], 
                    r_bromii_14_isolate[-1],
                    r_bromii_isolate_one_epoch,
                    r_bromii_isolate_two_epoch) +
  ggtitle('R. bromii (B) downsampled SFS comparison')

# Likelihood surface for UHGG Isolates
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Akkermansia_muciniphila_B_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Alistipes_finegoldii_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Alistipes_onderdonkii_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Alistipes_putredinis_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Alistipes_shahii_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Bacteroides_fragilis_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Bacteroides_ovatus_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Bacteroides_stercoris_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Bacteroides_thetaiotaomicron_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Bacteroides_xylanisolvens_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Barnesiella_intestinihominis_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Dialister_invisus_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Faecalibacterium_prausnitzii_K_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Odoribacter_splanchnicus_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Parabacteroides_distasonis_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Parabacteroides_merdae_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Prevotella_copri_isolate.csv')
plot_likelihood_surface('../Data/UHGG/UHGG_likelihood_surfaces/Ruminococcus_E_bromii_B_isolate.csv')

# Likelihood surface for complete HMP-QP
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Akkermansia_muciniphila_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Alistipes_finegoldii_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Alistipes_onderdonkii_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Alistipes_putredinis_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Alistipes_shahii_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroidales_bacterium_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_caccae_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_cellulosilyticus_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_fragilis_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_ovatus_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_stercoris_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_thetaiotaomicron_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_uniformis_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_vulgatus_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Bacteroides_xylanisolvens_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Barnesiella_intestinihominis_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Dialister_invisus_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Eubacterium_eligens_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Eubacterium_rectale_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Faecalibacterium_prausnitzii_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Odoribacter_splanchnicus_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Oscillibacter_sp_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Parabacteroides_distasonis_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Parabacteroides_merdae_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Phascolarctobacterium_sp_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Prevotella_copri_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Ruminococcus_bicirculans_hmp.csv')
plot_likelihood_surface('../Data/HMP_QP_likelihood_surfaces/Ruminococcus_bromii_hmp.csv')

# HMP-QP with and without 0-tons

a_muciniphila_orig = fold_sfs(read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/downsampled_syn_sfs.txt'))
a_finegoldii_orig = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_downsampled_14/downsampled_syn_sfs.txt'))
a_onderdonkii_orig = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/downsampled_syn_sfs.txt'))
a_putredinis_orig = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_downsampled_14/downsampled_syn_sfs.txt'))
a_shahii_orig = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_shahii_62199_downsampled_14/downsampled_syn_sfs.txt'))
b_bacterium_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/downsampled_syn_sfs.txt'))
b_caccae_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_downsampled_14/downsampled_syn_sfs.txt'))
b_cellulosilyticus_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/downsampled_syn_sfs.txt'))
b_fragilis_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_downsampled_14/downsampled_syn_sfs.txt'))
# b_massiliensis_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/downsampled_syn_sfs.txt'))
b_ovatus_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_downsampled_14/downsampled_syn_sfs.txt'))
b_stercoris_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_downsampled_14/downsampled_syn_sfs.txt'))
b_thetaiotaomicron_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/downsampled_syn_sfs.txt'))
b_uniformis_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_downsampled_14/downsampled_syn_sfs.txt'))
b_vulgatus_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/downsampled_syn_sfs.txt'))
b_xylanisolvens_orig = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/downsampled_syn_sfs.txt'))
b_intestinihominis_orig = fold_sfs(read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/downsampled_syn_sfs.txt'))
# c_sp_orig = fold_sfs(read_input_sfs_original('../Analysis/Coprococcus_sp_62244_downsampled_14/downsampled_syn_sfs.txt'))
d_invisus_orig = fold_sfs(read_input_sfs_original('../Analysis/Dialister_invisus_61905_downsampled_14/downsampled_syn_sfs.txt'))
e_eligens_orig = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_downsampled_14/downsampled_syn_sfs.txt'))
e_rectale_orig = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_downsampled_14/downsampled_syn_sfs.txt'))
f_prausnitzii_orig = fold_sfs(read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/downsampled_syn_sfs.txt'))
o_splanchnicus_orig = fold_sfs(read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/downsampled_syn_sfs.txt'))
o_sp_orig = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/downsampled_syn_sfs.txt'))
p_distasonis_orig = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/downsampled_syn_sfs.txt'))
p_merdae_orig = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/downsampled_syn_sfs.txt'))
p_sp_orig = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/downsampled_syn_sfs.txt'))
p_copri_orig = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/downsampled_syn_sfs.txt'))
r_bicirculans_orig = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/downsampled_syn_sfs.txt'))
r_bromii_orig = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/downsampled_syn_sfs.txt'))

a_muciniphila_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_syn_downsampled_sfs.txt'))
a_finegoldii_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_syn_downsampled_sfs.txt'))
a_onderdonkii_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_syn_downsampled_sfs.txt'))
a_putredinis_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_syn_downsampled_sfs.txt'))
a_shahii_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_bacterium_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_caccae_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_cellulosilyticus_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_fragilis_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_syn_downsampled_sfs.txt'))
# b_massiliensis_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_ovatus_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_stercoris_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_thetaiotaomicron_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_uniformis_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_vulgatus_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_xylanisolvens_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_syn_downsampled_sfs.txt'))
b_intestinihominis_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_syn_downsampled_sfs.txt'))
# c_sp_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Coprococcus_sp_62244_downsampled_14/empirical_syn_downsampled_sfs.txt'))
d_invisus_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Dialister_invisus_61905_downsampled_14/empirical_syn_downsampled_sfs.txt'))
e_eligens_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_syn_downsampled_sfs.txt'))
e_rectale_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_syn_downsampled_sfs.txt'))
f_prausnitzii_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_syn_downsampled_sfs.txt'))
o_splanchnicus_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_syn_downsampled_sfs.txt'))
o_sp_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_syn_downsampled_sfs.txt'))
p_distasonis_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_syn_downsampled_sfs.txt'))
p_merdae_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_syn_downsampled_sfs.txt'))
p_sp_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_syn_downsampled_sfs.txt'))
p_copri_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/empirical_syn_downsampled_sfs.txt'))
r_bicirculans_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_syn_downsampled_sfs.txt'))
r_bromii_hmp_qp = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_syn_downsampled_sfs.txt'))

one_epoch_14 = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/one_epoch_demography.txt')

compare_hmp_sfs(a_muciniphila_orig, a_muciniphila_hmp_qp, one_epoch_14) + ggtitle('A. muciniphila (Downsampled to 14)')
compare_hmp_sfs(a_finegoldii_orig, a_finegoldii_hmp_qp, one_epoch_14) + ggtitle('A. finegoldii (Downsampled to 14)')
compare_hmp_sfs(a_onderdonkii_orig, a_onderdonkii_hmp_qp, one_epoch_14) + ggtitle('A. onderdonkii (Downsampled to 14)')
compare_hmp_sfs(a_putredinis_orig, a_putredinis_hmp_qp, one_epoch_14) + ggtitle('A. putredinis (Downsampled to 14)')
compare_hmp_sfs(a_shahii_orig, a_shahii_hmp_qp, one_epoch_14) + ggtitle('A. shahii (Downsampled to 14)')
compare_hmp_sfs(b_bacterium_orig, b_bacterium_hmp_qp, one_epoch_14) + ggtitle('B. bacterium (Downsampled to 14)')
compare_hmp_sfs(b_caccae_orig, b_caccae_hmp_qp, one_epoch_14) + ggtitle('B. caccae (Downsampled to 14)')
compare_hmp_sfs(b_cellulosilyticus_orig, b_cellulosilyticus_hmp_qp, one_epoch_14) + ggtitle('B. cellulosilyticus (Downsampled to 14)')
compare_hmp_sfs(b_fragilis_orig, b_fragilis_hmp_qp, one_epoch_14) + ggtitle('B. fragilis (Downsampled to 14)')
compare_hmp_sfs(b_ovatus_orig, b_ovatus_hmp_qp, one_epoch_14) + ggtitle('B. ovatus (Downsampled to 14)')
compare_hmp_sfs(b_stercoris_orig, b_stercoris_hmp_qp, one_epoch_14) + ggtitle('B. stercoris (Downsampled to 14)')
compare_hmp_sfs(b_thetaiotaomicron_orig, b_thetaiotaomicron_hmp_qp, one_epoch_14) + ggtitle('B. thetaiotaomicron (Downsampled to 14)')
compare_hmp_sfs(b_uniformis_orig, b_uniformis_hmp_qp, one_epoch_14) + ggtitle('B. uniformis (Downsampled to 14)')
compare_hmp_sfs(b_vulgatus_orig, b_vulgatus_hmp_qp, one_epoch_14) + ggtitle('B. vulgatus (Downsampled to 14)')
compare_hmp_sfs(b_xylanisolvens_orig, b_xylanisolvens_hmp_qp, one_epoch_14) + ggtitle('B. xylanisolvens (Downsampled to 14)')
compare_hmp_sfs(b_intestinihominis_orig, b_intestinihominis_hmp_qp, one_epoch_14) + ggtitle('B. intestinihominis (Downsampled to 14)')
compare_hmp_sfs(d_invisus_orig, d_invisus_hmp_qp, one_epoch_14) + ggtitle('D. invisus (Downsampled to 14)')
compare_hmp_sfs(e_eligens_orig, e_eligens_hmp_qp, one_epoch_14) + ggtitle('E. eligens (Downsampled to 14)')
compare_hmp_sfs(e_rectale_orig, e_rectale_hmp_qp, one_epoch_14) + ggtitle('E. rectale (Downsampled to 14)')
compare_hmp_sfs(f_prausnitzii_orig, f_prausnitzii_hmp_qp, one_epoch_14) + ggtitle('F. prausnitzii (Downsampled to 14)')
compare_hmp_sfs(o_splanchnicus_orig, o_splanchnicus_hmp_qp, one_epoch_14) + ggtitle('O. splanchnicus (Downsampled to 14)')
compare_hmp_sfs(o_sp_orig, o_sp_hmp_qp, one_epoch_14) + ggtitle('Odoribacter sp. (Downsampled to 14)')
compare_hmp_sfs(p_distasonis_orig, p_distasonis_hmp_qp, one_epoch_14) + ggtitle('P. distasonis (Downsampled to 14)')
compare_hmp_sfs(p_merdae_orig, p_merdae_hmp_qp, one_epoch_14) + ggtitle('P. merdae (Downsampled to 14)')
compare_hmp_sfs(p_sp_orig, p_sp_hmp_qp, one_epoch_14) + ggtitle('Phascolarctobacterium sp. (Downsampled to 14)')
compare_hmp_sfs(p_copri_orig, p_copri_hmp_qp, one_epoch_14) + ggtitle('P. copri (Downsampled to 14)')
compare_hmp_sfs(r_bicirculans_orig, r_bicirculans_hmp_qp, one_epoch_14) + ggtitle('R. bicirculans (Downsampled to 14)')
compare_hmp_sfs(r_bromii_orig, r_bromii_hmp_qp, one_epoch_14) + ggtitle('R. bromii (Downsampled to 14)')

a_muciniphila_complete_two_epoch = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/complete_two_epoch_demography.txt')
a_finegoldii_complete_two_epoch = sfs_from_demography('../Analysis/Alistipes_finegoldii_56071_downsampled_14/complete_two_epoch_demography.txt') 
a_onderdonkii_complete_two_epoch = sfs_from_demography('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/complete_two_epoch_demography.txt') 
a_putredinis_complete_two_epoch = sfs_from_demography('../Analysis/Alistipes_putredinis_61533_downsampled_14/complete_two_epoch_demography.txt') 
a_shahii_complete_two_epoch = sfs_from_demography('../Analysis/Alistipes_shahii_62199_downsampled_14/complete_two_epoch_demography.txt') 
b_fragilis_complete_two_epoch = sfs_from_demography('../Analysis/Bacteroides_fragilis_54507_downsampled_14/complete_two_epoch_demography.txt') 
b_ovatus_complete_two_epoch = sfs_from_demography('../Analysis/Bacteroides_ovatus_58035_downsampled_14/complete_two_epoch_demography.txt') 
b_thetaiotaomicron_complete_two_epoch = sfs_from_demography('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/complete_two_epoch_demography.txt') 
b_xylanisolvens_complete_two_epoch = sfs_from_demography('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/complete_two_epoch_demography.txt') 
b_intestinihominis_complete_two_epoch = sfs_from_demography('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/complete_two_epoch_demography.txt') 
d_invisus_complete_two_epoch = sfs_from_demography('../Analysis/Dialister_invisus_61905_downsampled_14/complete_two_epoch_demography.txt') 
f_prausnitzii_complete_two_epoch = sfs_from_demography('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/complete_two_epoch_demography.txt') 
o_splanchnicus_complete_two_epoch = sfs_from_demography('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/complete_two_epoch_demography.txt') 
p_merdae_complete_two_epoch = sfs_from_demography('../Analysis/Parabacteroides_merdae_56972_downsampled_14/complete_two_epoch_demography.txt') 
p_copri_complete_two_epoch = sfs_from_demography('../Analysis/Prevotella_copri_61740_downsampled_14/complete_two_epoch_demography.txt') 
r_bromii_complete_two_epoch = sfs_from_demography('../Analysis/Ruminococcus_bromii_62047_downsampled_14/complete_two_epoch_demography.txt') 

compare_isolate_hmp_sfs(a_muciniphila_orig[-1], a_muciniphila_14_two_epoch, 
                        a_muciniphila_hmp_qp[-1], a_muciniphila_complete_two_epoch,
                        a_muciniphila_14_isolate[-1], a_muciniphila_isolate_two_epoch,
                        one_epoch_14) + ggtitle('A. muciniphila SFS comparison')

compare_isolate_hmp_sfs(a_finegoldii_orig[-1], a_finegoldii_14_two_epoch, 
                        a_finegoldii_hmp_qp[-1], a_finegoldii_complete_two_epoch,
                        a_finegoldii_14_isolate[-1], a_finegoldii_isolate_two_epoch,
                        one_epoch_14) + ggtitle('A. finegoldii SFS comparison')

compare_isolate_hmp_sfs(a_onderdonkii_orig[-1], a_onderdonkii_14_two_epoch, 
                        a_onderdonkii_hmp_qp[-1], a_onderdonkii_complete_two_epoch,
                        a_onderdonkii_14_isolate[-1], a_onderdonkii_isolate_two_epoch,
                        one_epoch_14) + ggtitle('A. onderdonkii SFS comparison')

compare_isolate_hmp_sfs(a_putredinis_orig[-1], a_putredinis_14_two_epoch, 
                        a_putredinis_hmp_qp[-1], a_putredinis_complete_two_epoch,
                        a_putredinis_14_isolate[-1], a_putredinis_isolate_two_epoch,
                        one_epoch_14) + ggtitle('A. putredinis SFS comparison')

compare_isolate_hmp_sfs(a_shahii_orig[-1], a_shahii_14_two_epoch, 
                        a_shahii_hmp_qp[-1], a_shahii_complete_two_epoch,
                        a_shahii_14_isolate[-1], a_shahii_isolate_two_epoch,
                        one_epoch_14) + ggtitle('A. shahii SFS comparison')

compare_isolate_hmp_sfs(b_fragilis_orig[-1], b_fragilis_14_two_epoch, 
                        b_fragilis_hmp_qp[-1], b_fragilis_complete_two_epoch,
                        b_fragilis_14_isolate[-1], b_fragilis_isolate_two_epoch,
                        one_epoch_14) + ggtitle('B. fragilis SFS comparison')

compare_isolate_hmp_sfs(b_ovatus_orig[-1], b_ovatus_14_two_epoch, 
                        b_ovatus_hmp_qp[-1], b_ovatus_complete_two_epoch,
                        b_ovatus_14_isolate[-1], b_ovatus_isolate_two_epoch,
                        one_epoch_14) + ggtitle('B. ovatus SFS comparison')

compare_isolate_hmp_sfs(b_thetaiotaomicron_orig[-1], b_thetaiotaomicron_14_two_epoch, 
                        b_thetaiotaomicron_hmp_qp[-1], b_thetaiotaomicron_complete_two_epoch,
                        b_thetaiotaomicron_14_isolate[-1], b_thetaiotaomicron_isolate_two_epoch,
                        one_epoch_14) + ggtitle('B. thetaiotaomicron SFS comparison')

compare_isolate_hmp_sfs(b_xylanisolvens_orig[-1], b_xylanisolvens_14_two_epoch, 
                        b_xylanisolvens_hmp_qp[-1], b_xylanisolvens_complete_two_epoch,
                        b_xylanisolvens_14_isolate[-1], b_xylanisolvens_isolate_two_epoch,
                        one_epoch_14) + ggtitle('B. xylanisolvens SFS comparison')

compare_isolate_hmp_sfs(b_intestinihominis_orig[-1], b_intestinihominis_14_two_epoch, 
                        b_intestinihominis_hmp_qp[-1], b_intestinihominis_complete_two_epoch,
                        b_intestinihominis_14_isolate[-1], b_intestinihominis_isolate_two_epoch,
                        one_epoch_14) + ggtitle('B. intestinihominis SFS comparison')

compare_isolate_hmp_sfs(d_invisus_orig[-1], d_invisus_14_two_epoch, 
                        d_invisus_hmp_qp[-1], d_invisus_complete_two_epoch,
                        d_invisus_14_isolate[-1], d_invisus_isolate_two_epoch,
                        one_epoch_14) + ggtitle('D. invisus SFS comparison')

compare_isolate_hmp_sfs(f_prausnitzii_orig[-1], f_prausnitzii_14_two_epoch, 
                        f_prausnitzii_hmp_qp[-1], f_prausnitzii_complete_two_epoch,
                        f_prausnitzii_14_isolate[-1], f_prausnitzii_isolate_two_epoch,
                        one_epoch_14) + ggtitle('F. prausnitzii SFS comparison')

compare_isolate_hmp_sfs(o_splanchnicus_orig[-1], o_splanchnicus_14_two_epoch, 
                        o_splanchnicus_hmp_qp[-1], o_splanchnicus_complete_two_epoch,
                        o_splanchnicus_14_isolate[-1], o_splanchnicus_isolate_two_epoch,
                        one_epoch_14) + ggtitle('O. splanchnicus SFS comparison')

compare_isolate_hmp_sfs(p_merdae_orig[-1], p_merdae_14_two_epoch, 
                        p_merdae_hmp_qp[-1], p_merdae_complete_two_epoch,
                        p_merdae_14_isolate[-1], p_merdae_isolate_two_epoch,
                        one_epoch_14) + ggtitle('P. merdae SFS comparison')

compare_isolate_hmp_sfs(p_copri_orig[-1], p_copri_14_two_epoch, 
                        p_copri_hmp_qp[-1], p_copri_complete_two_epoch,
                        p_copri_14_isolate[-1], p_copri_isolate_two_epoch,
                        one_epoch_14) + ggtitle('P. copri SFS comparison')


compare_isolate_hmp_sfs(r_bromii_orig[-1], r_bromii_14_two_epoch, 
                        r_bromii_hmp_qp[-1], r_bromii_complete_two_epoch,
                        r_bromii_14_isolate[-1], r_bromii_isolate_two_epoch,
                        one_epoch_14) + ggtitle('R. bromii SFS comparison')
