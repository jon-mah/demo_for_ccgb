library(ggplot2)
library(ggrepel)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
# library(dplyr)
library(fitdistrplus)
library(scales)
library(reshape2)
library(stringr)
library(ggridges)
library(forcats)
library("ggrepel")
library(patchwork)
library(ape)
library(ggtree)
library(treeio)
# install.packages('plotly')
library(plotly)
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
library(latex2exp)
library(ggvis)
library(pheatmap)


# BiocManager::install("treeio")
# BiocManager::install("ggtree")

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
  sfs_string = readLines(this_file)[8]
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
  species_surface = read.csv(input, header=TRUE)
  names(species_surface) = c('index', 'nu', 'tau', 'likelihood')

  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]

  MLE = max(species_surface$likelihood)
  MLE_minus_3 = MLE - 3
  MLE_minus_3_label = paste('<= ', str_trunc(toString(MLE_minus_3), 6, ellipsis=''), sep='')
  MLE_minus_1 = MLE - 1
  MLE_minus_1_label = paste(str_trunc(toString(MLE_minus_3), 6, ellipsis=''), ' <= ', str_trunc(toString(MLE_minus_1), 9, ellipsis=''), sep='')
  MLE_minus_half = MLE - 0.5
  MLE_minus_half_label  = paste(str_trunc(toString(MLE_minus_1), 6, ellipsis=''), ' <= ', str_trunc(toString(MLE_minus_half), 6, ellipsis=''), sep='')
  MLE_label = paste(str_trunc(toString(MLE_minus_half), 6, ellipsis=''), ' <= ', str_trunc(toString(MLE), 6, ellipsis=''), sep='')
  color_breakpoints = cut(species_surface$likelihood, c(-Inf, MLE_minus_3, MLE_minus_1, MLE_minus_half, MLE))
  species_surface_scatter = ggplot(data=species_surface, aes(x=nu, y=tau), color=likelihood) + 
    # geom_point(aes(colour = likelihood), size=1) +
    geom_point(aes(colour = color_breakpoints), size = 4, shape=15) +
    scale_color_manual(name='Log Likelihood',
                       values=c('#a6611a', '#dfc27d', '#80cdc1', '#018571'),
                       labels=c('(-Inf, -3]', '(-3, -1]', '(-1, -0.5', '(-0,5, 0]')) +
    geom_vline(xintercept=1.0, color='red', linewidth=2) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab('Nu') +
    ylab('Tau')

  return(species_surface_scatter)
}

plot_likelihood_surface_contour = function(input) {
  species_surface = read.csv(input, header=TRUE)
  names(species_surface) = c('index', 'nu', 'tau', 'likelihood')
  unique_nu = unique(species_surface$nu)
  unique_tau = unique(species_surface$tau)
  Z = matrix(data=NA, nrow=length(unique_nu), ncol=length(unique_tau))
  count = 1
  for (i in 1:length(unique_nu)) {
    for (j in 1:length(unique_tau)) {
      Z[i, j] = species_surface$likelihood[count]
      if (species_surface$nu[count] != unique_nu[i]) {
        print('break')
      } else if (species_surface$tau[count] != unique_tau[j]) {
        print('break')
      }
      count = count + 1
    }
 }
  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]
  best_params = c(species_surface$nu[1], species_surface$tau[1])
  print(best_params)
  MLE = max(species_surface$likelihood)
  print(MLE)
  species_surface$likelihood = species_surface$likelihood - MLE
  color_breakpoints = cut(species_surface$likelihood, c(-Inf, -3, -1, -0.5, 0))

  likelihood_surface_title = paste('MLE @ [', str_trunc(toString(best_params[1]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ', ', sep='')
  likelihood_surface_title = paste(likelihood_surface_title, str_trunc(toString(best_params[2]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ']', sep='')
  
  xlabel_text = expression(nu == frac(N[current], N[ancestral]))
  ylabel_text = expression(tau == frac(generations, 2 * N[ancestral]))
  fig = ggplot(species_surface) +
    geom_contour_filled(aes(x=nu, y=tau, z=likelihood), 
      # breaks = c(-Inf, -3, -1, -0.5, 0)) +
      breaks = c(0, -0.5, -1, -3, -Inf)) +
    scale_fill_brewer(palette = "YlGnBu", direction=1, name='Log Likelihood') +
    geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    annotate('point', x=best_params[1], y=best_params[2], color='orange', size=2) +
    xlab(xlabel_text)  +
    ylab(ylabel_text)
    # ggtitle(likelihood_surface_title)
  return(fig)
}

plot_likelihood_surface_contour_3C = function(input) {
  species_surface = read.csv(input, header=TRUE)
  names(species_surface) = c('index', 'nu', 'tau', 'likelihood')
  unique_nu = unique(species_surface$nu)
  unique_tau = unique(species_surface$tau)
  Z = matrix(data=NA, nrow=length(unique_nu), ncol=length(unique_tau))
  count = 1
  for (i in 1:length(unique_nu)) {
    for (j in 1:length(unique_tau)) {
      Z[i, j] = species_surface$likelihood[count]
      if (species_surface$nu[count] != unique_nu[i]) {
        print('break')
      } else if (species_surface$tau[count] != unique_tau[j]) {
        print('break')
      }
      count = count + 1
    }
 }
  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]
  best_params = c(species_surface$nu[1], species_surface$tau[1])
  print(best_params)
  MLE = max(species_surface$likelihood)
  species_surface$likelihood = species_surface$likelihood - MLE
  color_breakpoints = cut(species_surface$likelihood, c(-Inf, -3, -1, -0.5, 0))

  likelihood_surface_title = paste('MLE @ [', str_trunc(toString(best_params[1]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ', ', sep='')
  likelihood_surface_title = paste(likelihood_surface_title, str_trunc(toString(best_params[2]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ']', sep='')
  
  xlabel_text = expression(nu == frac(N[current], N[ancestral]))
  ylabel_text = expression(tau == frac(generations, 2 * N[ancestral]))
  fig = ggplot(species_surface) +
    geom_contour_filled(aes(x=nu, y=tau, z=likelihood), 
      # breaks = c(-Inf, -3, -1, -0.5, 0)) +
      breaks = c(0, -0.5, -1, -3, -Inf)) +
    scale_fill_brewer(palette = "YlGnBu", direction=1, name='Log Likelihood') +
    geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    annotate('point', x=best_params[1], y=best_params[2], color='orange', size=1) +
    theme(legend.position = c(0.77, 0.75)) +
    theme(legend.text=element_text(size=10)) +
    theme(axis.title.x = element_blank()) +
    ylab(ylabel_text) +
    theme(axis.text=element_text(size=12),
      axis.title=element_text(size=16))
  return(fig)
}

plot_likelihood_surface_contour_3D = function(input) {
  species_surface = read.csv(input, header=TRUE)
  names(species_surface) = c('index', 'nu', 'tau', 'likelihood')
  unique_nu = unique(species_surface$nu)
  unique_tau = unique(species_surface$tau)
  Z = matrix(data=NA, nrow=length(unique_nu), ncol=length(unique_tau))
  count = 1
  for (i in 1:length(unique_nu)) {
    for (j in 1:length(unique_tau)) {
      Z[i, j] = species_surface$likelihood[count]
      if (species_surface$nu[count] != unique_nu[i]) {
        print('break')
      } else if (species_surface$tau[count] != unique_tau[j]) {
        print('break')
      }
      count = count + 1
    }
 }
  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]
  best_params = c(species_surface$nu[1], species_surface$tau[1])
  print(best_params)
  MLE = max(species_surface$likelihood)
  species_surface$likelihood = species_surface$likelihood - MLE
  color_breakpoints = cut(species_surface$likelihood, c(-Inf, -3, -1, -0.5, 0))

  likelihood_surface_title = paste('MLE @ [', str_trunc(toString(best_params[1]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ', ', sep='')
  likelihood_surface_title = paste(likelihood_surface_title, str_trunc(toString(best_params[2]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ']', sep='')
  
  xlabel_text = expression(nu == frac(N[current], N[ancestral]))
  ylabel_text = expression(tau == frac(generations, 2 * N[ancestral]))
  fig = ggplot(species_surface) +
    geom_contour_filled(aes(x=nu, y=tau, z=likelihood), 
      breaks = c(0, -0.5, -1, -3, -Inf)) +
    scale_fill_brewer(palette = "YlGnBu", direction=1, name='Log Likelihood') +
    geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    annotate('point', x=best_params[1], y=best_params[2], color='orange', size=1) +
    theme(legend.position = "none") +
    xlab(xlabel_text) +
    ylab(ylabel_text) +
    theme(axis.text=element_text(size=12),
      axis.title=element_text(size=16))
    # ggtitle(likelihood_surface_title)
  return(fig)
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}


compare_hmp_sfs = function(complete, one_epoch, two_epoch, three_epoch, nonsyn, gamma, neugamma) {
  x_axis = 1:length(one_epoch)
  
  input_df = data.frame(proportional_sfs(complete[-1]),
                        proportional_sfs(one_epoch),
                        proportional_sfs(two_epoch),
                        proportional_sfs(three_epoch),
                        proportional_sfs(nonsyn[-1]),
                        proportional_sfs(gamma),
                        proportional_sfs(neugamma),
                        x_axis)
  
  names(input_df) = c('Synonymous SFS',
                      'One Epoch',
                      'Two Epoch',
                      'Three Epoch',
                      'Nonsynonymous SFS',
                      'Gamma DFE',
                      'Neu + Gamma DFE',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                               aes(x=x_axis, 
                                   y=value,
                                   fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("blue4", "dodgerblue3", "steelblue1",  "lightskyblue2",
      "goldenrod3", "goldenrod1", "yellow2"))
  
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(-0.5, length(x_axis) + 0.5)) +
    ylab('Number of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  return(p_input_comparison)
}

plot_empirical_sfs = function(input) {
  x_axis = 1:(length(input))
  
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
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(-0.5, length(x_axis) + 0.5)) +
    ylab('Number of Segregating Sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  return(p_input_comparison)
}

read_dfe_params = function(input_dfe_file) {
    ## Reads input DFE from output *inferred_DFE.txt
  this_file = file(input_dfe_file) # Open file
  on.exit(close(this_file)) # Close when done
  # Parse file and string manipulation
  param_string_high = readLines(this_file)[4]
  param_string_high = strsplit(param_string_high, split=': ')
  param_string_high = unlist(param_string_high)[2]
  param_string_high = str_sub(param_string_high, 2, -3)
  param_string_high = str_squish(param_string_high)
  param_string_high = unlist(strsplit(param_string_high, ' '))
  param_string_high = as.numeric(param_string_high)
  
  gamma_shape = param_string_high[1]
  gamma_scale_high = param_string_high[2]
  
  param_string_low = readLines(this_file)[5]
  param_string_low = strsplit(param_string_low, split=': ')
  param_string_low = unlist(param_string_low)[2]
  param_string_low = str_sub(param_string_low, 2, -3)
  param_string_low = str_squish(param_string_low)
  param_string_low = unlist(strsplit(param_string_low, ' '))
  param_string_low = as.numeric(param_string_low)
  
  gamma_scale_low = param_string_low[2]
  
  # gamma_dfe_bound_high = compute_dfe_height(shape=gamma_shape, scale=gamma_scale_high)
  #  gamma_dfe_bound_low = compute_dfe_height(shape=gamma_shape, scale=gamma_scale_low)
  
  gamma_dfe_dist_high = rgamma(100000, shape=gamma_shape, scale=gamma_scale_high)
  gamma_dfe_dist_low = rgamma(100000, shape=gamma_shape, scale=gamma_scale_low)
  
  param_string_high = readLines(this_file)[11]
  param_string_high = strsplit(param_string_high, split=': ')
  param_string_high = unlist(param_string_high)[2]
  param_string_high = str_sub(param_string_high, 2, -3)
  param_string_high = str_squish(param_string_high)
  param_string_high = unlist(strsplit(param_string_high, ' '))
  param_string_high = as.numeric(param_string_high)
  
  neugamma_proportion = param_string_high[1]
  neugamma_shape = param_string_high[2]
  neugamma_scale_high = param_string_high[3]
  
  param_string_low = readLines(this_file)[12]
  param_string_low = strsplit(param_string_low, split=': ')
  param_string_low = unlist(param_string_low)[2]
  param_string_low = str_sub(param_string_low, 2, -3)
  param_string_low = str_squish(param_string_low)
  param_string_low = unlist(strsplit(param_string_low, ' '))
  param_string_low = as.numeric(param_string_low)
  
  neugamma_scale_low = param_string_low[3]
  
  # neugamma_dfe_bound_high = compute_dfe_height(shape=neugamma_shape, scale=neugamma_scale_high)
  # neugamma_dfe_bound_low = compute_dfe_height(shape=neugamma_shape, scale=neugamma_scale_low)
  
  neugamma_dfe_dist_high = rgamma(10000, shape=neugamma_shape, scale=neugamma_scale_high)
  neugamma_dfe_dist_low = rgamma(10000, shape=neugamma_shape, scale=neugamma_scale_low)
  
  zeroed_sites = as.integer(10000 * neugamma_proportion)
  
  neugamma_dfe_dist_high[1:zeroed_sites] = 0 
  neugamma_dfe_dist_low[1:zeroed_sites] = 0
  
  dfe_df = data.frame(gamma_dfe_dist_high, 
                      gamma_dfe_dist_low,
                      neugamma_dfe_dist_high,
                      neugamma_dfe_dist_low)
  return(dfe_df)
}

read_dfe_dadi_params = function(input_dfe_file) {
  # Reads input DFE from output *inferred_DFE.txt
  this_file = file(input_dfe_file) # Open file
  on.exit(close(this_file)) # Close when done
  # Parse file and string manipulation
  param_string = readLines(this_file)[3]
  param_string = strsplit(param_string, split=': ')
  param_string = unlist(param_string)[2]
  param_string = str_sub(param_string, 2, -3)
  param_string = str_squish(param_string)
  param_string = unlist(strsplit(param_string, ' '))
  param_string = as.numeric(param_string)
  
  gamma_shape = param_string[1]
  gamma_scale = param_string[2]

  gamma_dfe_dist = rgamma(100000, shape=gamma_shape, scale=gamma_scale)

  param_string = readLines(this_file)[10]
  param_string = strsplit(param_string, split=': ')
  param_string = unlist(param_string)[2]
  param_string = str_sub(param_string, 2, -3)
  param_string = str_squish(param_string)
  param_string = unlist(strsplit(param_string, ' '))
  param_string = as.numeric(param_string)
  
  neugamma_proportion = param_string[1]
  neugamma_shape = param_string[2]
  neugamma_scale = param_string[3]

  neugamma_dfe_dist = rgamma(10000, shape=neugamma_shape, scale=neugamma_scale)

  zeroed_sites = as.integer(10000 * neugamma_proportion)
  
  neugamma_dfe_dist[1:zeroed_sites] = 0 

  dfe_df = data.frame(gamma_dfe_dist, 
                      neugamma_dfe_dist)
  return(dfe_df)
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
  
  # gamma_dfe_bound_high = compute_dfe_height(shape=gamma_shape, scale=gamma_scale_high)
  # gamma_dfe_bound_low = compute_dfe_height(shape=gamma_shape, scale=gamma_scale_low)
  
  gamma_dfe_dist_high = rgamma(100000, shape=gamma_shape, scale=gamma_scale_high)
  gamma_dfe_dist_low = rgamma(100000, shape=gamma_shape, scale=gamma_scale_low)
  
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
  
  # neugamma_dfe_bound_high = compute_dfe_height(shape=neugamma_shape, scale=neugamma_scale_high)
  # neugamma_dfe_bound_low = compute_dfe_height(shape=neugamma_shape, scale=neugamma_scale_low)
  
  neugamma_dfe_dist_high = rgamma(100000, shape=neugamma_shape, scale=neugamma_scale_high)
  neugamma_dfe_dist_low = rgamma(100000, shape=neugamma_shape, scale=neugamma_scale_low)
  
  zeroed_sites = as.integer(100000 * neugamma_proportion)
  
  neugamma_dfe_dist_high[1:zeroed_sites] = 1e-06* 1.1
  neugamma_dfe_dist_low[1:zeroed_sites] = 1e-06 * 1.1
  
  dfe_df = data.frame(gamma_dfe_dist_high, 
                      gamma_dfe_dist_low,
                      neugamma_dfe_dist_high,
                      neugamma_dfe_dist_low)
  dfe_df[dfe_df < 1e-9] = 1e-9
  
  names(dfe_df) = c('Gamma, mu=6.93E-10', 'Gamma, mu=4.08E-10',
                    'Neugamma, mu=6.93E-10', 'Neugamma, mu=4.08E-10')

  ggplot(melt(dfe_df), aes(x=value, y=..density.., fill=variable)) +
    geom_histogram(position='dodge',
                   breaks=c(0.000000001, 0.00000001,  0.0000001, 0.000001, 0.0001)) +
    scale_x_log10() +
    ylab('Proportion of sites') +
    xlab('Selective Effect') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    guides(fill=guide_legend(title="Estimated mutation rate"))
  return(dfe_df)
}

plot_best_fit_sfs = function(input_data) {
  input_data = data.frame(input_data)
  colnames(input_data) = c(
    'Empirical Synonymous', 
    'Model Synonymous',
    'Empirical Nonsynonymous',
    'Model Nonsynonymous',
    'Species',
    'X.axis')
  fig = ggplot(melt(input_data, id=c('Species', 'X.axis')), aes(x=X.axis, y=as.numeric(value), fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    xlab('Minor allele frequency') + 
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("blue4", "steelblue3", "goldenrod3", "goldenrod1")) +
    theme(legend.position="none") +
    theme(plot.title = element_text(face = "italic"))
  return(fig)
}

plot_best_fit_sfs_3A = function(input_data) {
  input_data = data.frame(input_data)
  colnames(input_data) = c(
    'Empirical Synonymous', 
    'MLE Synonymous',
    'Empirical Nonsynonymous',
    'MLE Nonsynonymous',
    'Species',
    'X.axis')
  fig = ggplot(melt(input_data, id=c('Species', 'X.axis')), aes(x=X.axis, y=as.numeric(value), fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("blue4", "steelblue3", "goldenrod3", "goldenrod1"), name='Site-frequency-spectra') +
    # scale_fill_manual(values=c("#cb181d", "#fb6a4a", "blue4", "steelblue3"), name='Site-frequency-spectra') +
    theme(legend.position = c(0.72, 0.75)) +
    theme(legend.text=element_text(size=10)) +    
    theme(plot.title = element_text(face = "italic", size=16)) +
    theme(axis.text=element_text(size=12),
      axis.title=element_text(size=16))
  return(fig)
}

plot_best_fit_sfs_3B = function(input_data) {
  input_data = data.frame(input_data)
  colnames(input_data) = c(
    'Empirical Synonymous', 
    'Model Synonymous',
    'Empirical Nonsynonymous',
    'Model Nonsynonymous',
    'Species',
    'X.axis')
  fig = ggplot(melt(input_data, id=c('Species', 'X.axis')), aes(x=X.axis, y=as.numeric(value), fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    xlab('Minor allele frequency') + 
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("blue4", "steelblue3", "goldenrod3", "goldenrod1")) +
    # scale_fill_manual(values=c("#cb181d", "#fb6a4a", "blue4", "steelblue3"), name='Site-frequency-spectra') +
    theme(legend.position="none") +
    theme(plot.title = element_text(face = "italic", size=16)) +
    theme(axis.text=element_text(size=12),
      axis.title=element_text(size=16))
  return(fig)
}

plot_dfe_grid = function(input) {
  species_surface = read.csv(input, header=TRUE)
  names(species_surface) = c('shape', 'scale', 'likelihood')
  unique_shape = unique(species_surface$shape)
  unique_scale = unique(species_surface$scale)
  Z = matrix(data=NA, nrow=length(unique_shape), ncol=length(unique_scale))
  count = 1
  for (i in 1:length(unique_shape)) {
    for (j in 1:length(unique_scale)) {
      Z[i, j] = species_surface$likelihood[count]
      if (species_surface$shape[count] != unique_shape[i]) {
        print('break')
      } else if (species_surface$scale[count] != unique_scale[j]) {
        print('break')
      }
      count = count + 1
    }
 }
  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]
  best_params = c(species_surface$shape[1], species_surface$scale[1])
  print(best_params)
  MLE = max(species_surface$likelihood)
  species_surface$likelihood = species_surface$likelihood - MLE
  color_breakpoints = cut(species_surface$likelihood, c(-Inf, -3, -1, -0.5, 0))

  likelihood_surface_title = paste('MLE @ [', str_trunc(toString(best_params[1]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ', ', sep='')
  likelihood_surface_title = paste(likelihood_surface_title, str_trunc(toString(best_params[2]), 8, ellipsis=''), sep='')
  likelihood_surface_title = paste(likelihood_surface_title, ']', sep='')
  
  fig = ggplot(species_surface) +
    geom_contour_filled(aes(x=shape, y=scale, z=likelihood), 
      breaks = c(0, -0.5, -1, -3, -Inf)) +
    scale_fill_brewer(palette = "YlGnBu", direction=1, name='Log Likelihood') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    annotate('point', x=best_params[1], y=best_params[2], color='orange', size=2) +
    xlab('Shape')  +
    ylab('Scale') +
    scale_y_log10()
    #ggtitle(likelihood_surface_title)
  return(fig)
}

find_dfe_mle = function(input) {
  species_surface = read.csv(input, header=TRUE)
  names(species_surface) = c('shape', 'scale', 'likelihood')
  unique_shape = unique(species_surface$shape)
  unique_scale = unique(species_surface$scale)
  Z = matrix(data=NA, nrow=length(unique_shape), ncol=length(unique_scale))
  count = 1
  species_surface = species_surface[order(species_surface$likelihood, decreasing=TRUE), ]
  best_params = c(species_surface$shape[1], species_surface$scale[1])
  print(best_params)
  MLE = max(species_surface$likelihood)
  print(MLE)
}

cross_species_dfe_comparison = function(input_A, input_B) {
  species_surface_A = read.csv(input_A, header=TRUE)
  names(species_surface_A) = c('shape', 'scale', 'likelihood')
  unique_shape_A = unique(species_surface_A$shape)
  unique_scale_A = unique(species_surface_A$scale)
  Z_A = matrix(data=NA, nrow=length(unique_shape_A), ncol=length(unique_scale_A))
  count = 1
  for (i in 1:length(unique_shape_A)) {
    for (j in 1:length(unique_scale_A)) {
      Z_A[i, j] = species_surface_A$likelihood[count]
      if (species_surface_A$shape[count] != unique_shape_A[i]) {
        print('break')
      } else if (species_surface_A$scale[count] != unique_scale_A[j]) {
        print('break')
      }
      count = count + 1
    }
 }
  temp_surface_A = species_surface_A[order(species_surface_A$likelihood, decreasing=TRUE), ]
  best_params_A = c(temp_surface_A$shape[1], temp_surface_A$scale[1])
  ML_A = temp_surface_A$likelihood[1]
  
  species_surface_B = read.csv(input_B, header=TRUE)
  names(species_surface_B) = c('shape', 'scale', 'likelihood')
  unique_shape_B = unique(species_surface_B$shape)
  unique_scale_B = unique(species_surface_B$scale)
  Z_B = matrix(data=NA, nrow=length(unique_shape_B), ncol=length(unique_scale_B))
  count = 1
  for (i in 1:length(unique_shape_B)) {
    for (j in 1:length(unique_scale_B)) {
      Z_B[i, j] = species_surface_B$likelihood[count]
      if (species_surface_B$shape[count] != unique_shape_B[i]) {
        print('break')
      } else if (species_surface_B$scale[count] != unique_scale_B[j]) {
        print('break')
      }
      count = count + 1
    }
 }
  temp_surface_B = species_surface_B[order(species_surface_B$likelihood, decreasing=TRUE), ]
  best_params_B = c(temp_surface_B$shape[1], temp_surface_B$scale[1])
  ML_B = temp_surface_B$likelihood[1]
  combined_likelihood = species_surface_A$likelihood + species_surface_B$likelihood
  comparison_surface = data.frame(species_surface_A$shape, species_surface_A$scale, combined_likelihood)
  temp_comparison_surface = comparison_surface[order(comparison_surface$combined_likelihood, decreasing=TRUE), ]
  best_params_comparison = c(temp_comparison_surface$species_surface_A.shape[1], temp_comparison_surface$species_surface_A.scale[1])
  ML_comparison = temp_comparison_surface$combined_likelihood[1]
  independent_sum = ML_A + ML_B
  return(ML_comparison - independent_sum)
}

compare_core_accessory_sfs = function(all, core, accessory) {
  x_axis = 1:length(all)

  input_df = data.frame(proportional_sfs(all),
                        proportional_sfs(core),
                        proportional_sfs(accessory),
                        x_axis)
  
  names(input_df) = c('All genes',
                      'Core genes',
                      'Accessory genes',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                                                     aes(x=x_axis, 
                                                         y=value,
                                                         fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}

compare_core_accessory_sfs_count = function(all, core, accessory) {
  x_axis = 1:length(all)

  input_df = data.frame(all,
                        core,
                        accessory,
                        x_axis)
  
  names(input_df) = c('All genes',
                      'Core genes',
                      'Accessory genes',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                                                     aes(x=x_axis, 
                                                         y=value,
                                                         fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}

compare_core_accessory_sfs_syn_ns = function(core_syn, core_nonsyn, accessory_syn, accessory_nonsyn) {
  x_axis = 1:length(core_syn)

  input_df = data.frame(proportional_sfs(core_syn),
                        proportional_sfs(core_nonsyn),
                        proportional_sfs(accessory_syn),
                        proportional_sfs(accessory_nonsyn),
                        x_axis)
  
  names(input_df) = c('Core genes (Syn)',
                      'Core genes (Nonsyn)',
                      'Accessory genes (Syn)',
                      'Accessory genes (Nonsyn)',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                                                     aes(x=x_axis, 
                                                         y=value,
                                                         fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    scale_fill_manual(values=c("blue4", "steelblue3", "goldenrod3", "goldenrod1"))
  
  return(p_input_comparison)
}


compare_core_sfs = function(all, core) {
  x_axis = 1:length(all)

  input_df = data.frame(proportional_sfs(all),
                        proportional_sfs(core),
                        x_axis)
  
  names(input_df) = c('All genes',
                      'Core genes',
                      'x_axis')
  
  p_input_comparison <- ggplot(data = melt(input_df, id='x_axis'),
                                                     aes(x=x_axis, 
                                                         y=value,
                                                         fill=variable)) +
    geom_bar(position='dodge2', stat='identity') +
    labs(x = "", fill = "") +
    scale_x_continuous(name='Minor allele frequency in Sample', breaks=x_axis, limits=c(0.5, length(x_axis) + 0.5)) +
    ylab('Proportion of segregating sites') +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ## scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}

extract_array_length <- function(input_string) {
  array_string <- str_extract(input_string, "\\[(.*?)\\]")
  num_elements <- length(strsplit(array_string, "[ ,]")[[1]])
  return(num_elements)
}

AIC_from_demography = function(input_file) {
  ## Reads input SFS from output *demography.txt
  if(grepl("one_epoch", input_file)) {
    k=2
  } else if(grepl("two_epoch", input_file)) {
    k=4
  } else {
    k=8
  }
  this_file = file(input_file)
  on.exit(close(this_file))
  ll_string = readLines(this_file)[2]
  loglik <- as.numeric(str_extract(ll_string, "-?\\d+\\.\\d+"))
  return(k - 2*loglik)
}



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 

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
oscillibacter_sp_orig = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/downsampled_syn_sfs.txt'))
p_distasonis_orig = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/downsampled_syn_sfs.txt'))
p_merdae_orig = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/downsampled_syn_sfs.txt'))
phascolarctobacterium_sp_orig = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/downsampled_syn_sfs.txt'))
p_copri_orig = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/downsampled_syn_sfs.txt'))
r_bicirculans_orig = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/downsampled_syn_sfs.txt'))
r_bromii_orig = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/downsampled_syn_sfs.txt'))

a_muciniphila_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
a_finegoldii_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
a_onderdonkii_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
a_putredinis_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
a_shahii_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_bacterium_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_caccae_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_cellulosilyticus_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_fragilis_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
# b_massiliensis_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_ovatus_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_stercoris_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_thetaiotaomicron_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_uniformis_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_vulgatus_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_xylanisolvens_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
b_intestinihominis_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
# c_sp_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Coprococcus_sp_62244_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
d_invisus_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
e_eligens_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
e_rectale_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
f_prausnitzii_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
o_splanchnicus_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
oscillibacter_sp_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
p_distasonis_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
p_merdae_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
phascolarctobacterium_sp_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
p_copri_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
r_bicirculans_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))
r_bromii_hmp_qp_syn = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_syn_downsampled_sfs.txt'))

a_muciniphila_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
a_finegoldii_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
a_onderdonkii_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
a_putredinis_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
a_shahii_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_bacterium_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_caccae_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_cellulosilyticus_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_fragilis_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
# b_massiliensis_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_ovatus_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_stercoris_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_thetaiotaomicron_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_uniformis_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_vulgatus_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_xylanisolvens_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
b_intestinihominis_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
# c_sp_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Coprococcus_sp_62244_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
d_invisus_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
e_eligens_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
e_rectale_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
f_prausnitzii_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
o_splanchnicus_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
oscillibacter_sp_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
p_distasonis_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
p_merdae_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
phascolarctobacterium_sp_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
p_copri_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
r_bicirculans_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))
r_bromii_hmp_qp_nonsyn = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt'))

a_muciniphila_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
a_finegoldii_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
a_onderdonkii_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
a_putredinis_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
a_shahii_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_bacterium_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_caccae_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_cellulosilyticus_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_fragilis_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
# b_massiliensis_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_ovatus_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_stercoris_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_thetaiotaomicron_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_uniformis_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_vulgatus_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_xylanisolvens_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
b_intestinihominis_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
# c_sp_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Coprococcus_sp_62244_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
d_invisus_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
e_eligens_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
e_rectale_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
f_prausnitzii_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
o_splanchnicus_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
oscillibacter_sp_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
p_distasonis_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
p_merdae_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
phascolarctobacterium_sp_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
p_copri_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
r_bicirculans_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))
r_bromii_hmp_qp_syn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt'))

a_muciniphila_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
a_finegoldii_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
a_onderdonkii_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
a_putredinis_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
a_shahii_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_bacterium_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_caccae_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_cellulosilyticus_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_fragilis_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
# b_massiliensis_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_ovatus_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_stercoris_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_thetaiotaomicron_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_uniformis_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_vulgatus_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_xylanisolvens_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
b_intestinihominis_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
# c_sp_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Coprococcus_sp_62244_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
d_invisus_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
e_eligens_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
e_rectale_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
f_prausnitzii_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
o_splanchnicus_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
oscillibacter_sp_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
p_distasonis_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
p_merdae_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
phascolarctobacterium_sp_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
p_copri_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
r_bicirculans_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))
r_bromii_hmp_qp_nonsyn_accessory = fold_sfs(read_input_sfs_original('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt'))

one_epoch_14 = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_one_epoch_demography.txt')

a_muciniphila_core_two_epoch = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_two_epoch_demography.txt')
a_finegoldii_core_two_epoch = sfs_from_demography('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_two_epoch_demography.txt') 
a_onderdonkii_core_two_epoch = sfs_from_demography('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_two_epoch_demography.txt') 
a_putredinis_core_two_epoch = sfs_from_demography('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_two_epoch_demography.txt') 
a_shahii_core_two_epoch = sfs_from_demography('../Analysis/Alistipes_shahii_62199_downsampled_14/core_two_epoch_demography.txt') 
b_bacterium_core_two_epoch = sfs_from_demography('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_two_epoch_demography.txt') 
b_caccae_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_two_epoch_demography.txt') 
b_cellulosilyticus_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_two_epoch_demography.txt') 
b_fragilis_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_two_epoch_demography.txt') 
b_ovatus_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_two_epoch_demography.txt') 
b_stercoris_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_two_epoch_demography.txt') 
b_thetaiotaomicron_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_two_epoch_demography.txt') 
b_uniformis_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_two_epoch_demography.txt') 
b_vulgatus_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_two_epoch_demography.txt') 
b_xylanisolvens_core_two_epoch = sfs_from_demography('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_two_epoch_demography.txt') 
b_intestinihominis_core_two_epoch = sfs_from_demography('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_two_epoch_demography.txt') 
d_invisus_core_two_epoch = sfs_from_demography('../Analysis/Dialister_invisus_61905_downsampled_14/core_two_epoch_demography.txt') 
e_eligens_core_two_epoch = sfs_from_demography('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_two_epoch_demography.txt') 
e_rectale_core_two_epoch = sfs_from_demography('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_two_epoch_demography.txt') 
f_prausnitzii_core_two_epoch = sfs_from_demography('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_two_epoch_demography.txt') 
o_splanchnicus_core_two_epoch = sfs_from_demography('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_two_epoch_demography.txt') 
oscillibacter_sp_core_two_epoch = sfs_from_demography('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_two_epoch_demography.txt') 
p_distasonis_core_two_epoch = sfs_from_demography('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_two_epoch_demography.txt') 
p_merdae_core_two_epoch = sfs_from_demography('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_two_epoch_demography.txt') 
phascolarctobacterium_sp_core_two_epoch = sfs_from_demography('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_two_epoch_demography.txt') 
p_copri_core_two_epoch = sfs_from_demography('../Analysis/Prevotella_copri_61740_downsampled_14/core_two_epoch_demography.txt') 
r_bicirculans_core_two_epoch = sfs_from_demography('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_two_epoch_demography.txt') 
r_bromii_core_two_epoch = sfs_from_demography('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_two_epoch_demography.txt') 

a_muciniphila_core_three_epoch = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_three_epoch_demography.txt')
a_finegoldii_core_three_epoch = sfs_from_demography('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_three_epoch_demography.txt') 
a_onderdonkii_core_three_epoch = sfs_from_demography('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_three_epoch_demography.txt') 
a_putredinis_core_three_epoch = sfs_from_demography('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_three_epoch_demography.txt') 
a_shahii_core_three_epoch = sfs_from_demography('../Analysis/Alistipes_shahii_62199_downsampled_14/core_three_epoch_demography.txt') 
b_bacterium_core_three_epoch = sfs_from_demography('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_three_epoch_demography.txt') 
b_caccae_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_three_epoch_demography.txt') 
b_cellulosilyticus_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_three_epoch_demography.txt') 
b_fragilis_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_three_epoch_demography.txt') 
b_ovatus_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_three_epoch_demography.txt') 
b_stercoris_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_three_epoch_demography.txt') 
b_thetaiotaomicron_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_three_epoch_demography.txt') 
b_uniformis_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_three_epoch_demography.txt') 
b_vulgatus_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_three_epoch_demography.txt') 
b_xylanisolvens_core_three_epoch = sfs_from_demography('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_three_epoch_demography.txt') 
b_intestinihominis_core_three_epoch = sfs_from_demography('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_three_epoch_demography.txt') 
d_invisus_core_three_epoch = sfs_from_demography('../Analysis/Dialister_invisus_61905_downsampled_14/core_three_epoch_demography.txt') 
e_eligens_core_three_epoch = sfs_from_demography('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_three_epoch_demography.txt') 
e_rectale_core_three_epoch = sfs_from_demography('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_three_epoch_demography.txt') 
f_prausnitzii_core_three_epoch = sfs_from_demography('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_three_epoch_demography.txt') 
o_splanchnicus_core_three_epoch = sfs_from_demography('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_three_epoch_demography.txt') 
oscillibacter_sp_core_three_epoch = sfs_from_demography('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_three_epoch_demography.txt') 
p_distasonis_core_three_epoch = sfs_from_demography('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_three_epoch_demography.txt') 
p_merdae_core_three_epoch = sfs_from_demography('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_three_epoch_demography.txt') 
phascolarctobacterium_sp_core_three_epoch = sfs_from_demography('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_three_epoch_demography.txt') 
p_copri_core_three_epoch = sfs_from_demography('../Analysis/Prevotella_copri_61740_downsampled_14/core_three_epoch_demography.txt') 
r_bicirculans_core_three_epoch = sfs_from_demography('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_three_epoch_demography.txt') 
r_bromii_core_three_epoch = sfs_from_demography('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_three_epoch_demography.txt') 

a_muciniphila_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/inferred_DFE.txt')
a_finegoldii_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_finegoldii_56071_downsampled_14/inferred_DFE.txt') 
a_onderdonkii_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/inferred_DFE.txt') 
a_putredinis_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_putredinis_61533_downsampled_14/inferred_DFE.txt') 
a_shahii_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_shahii_62199_downsampled_14/inferred_DFE.txt') 
b_bacterium_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/inferred_DFE.txt') 
b_caccae_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_caccae_53434_downsampled_14/inferred_DFE.txt') 
b_cellulosilyticus_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/inferred_DFE.txt') 
b_fragilis_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_fragilis_54507_downsampled_14/inferred_DFE.txt') 
b_ovatus_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_ovatus_58035_downsampled_14/inferred_DFE.txt') 
b_stercoris_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_stercoris_56735_downsampled_14/inferred_DFE.txt') 
b_thetaiotaomicron_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/inferred_DFE.txt') 
b_uniformis_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_uniformis_57318_downsampled_14/inferred_DFE.txt') 
b_vulgatus_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/inferred_DFE.txt') 
b_xylanisolvens_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/inferred_DFE.txt') 
b_intestinihominis_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/inferred_DFE.txt') 
d_invisus_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Dialister_invisus_61905_downsampled_14/inferred_DFE.txt') 
e_eligens_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Eubacterium_eligens_61678_downsampled_14/inferred_DFE.txt') 
e_rectale_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Eubacterium_rectale_56927_downsampled_14/inferred_DFE.txt') 
f_prausnitzii_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/inferred_DFE.txt') 
o_splanchnicus_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/inferred_DFE.txt') 
oscillibacter_sp_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Oscillibacter_sp_60799_downsampled_14/inferred_DFE.txt') 
p_distasonis_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/inferred_DFE.txt') 
p_merdae_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Parabacteroides_merdae_56972_downsampled_14/inferred_DFE.txt') 
phascolarctobacterium_sp_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/inferred_DFE.txt') 
p_copri_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Prevotella_copri_61740_downsampled_14/inferred_DFE.txt') 
r_bicirculans_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/inferred_DFE.txt') 
r_bromii_complete_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Ruminococcus_bromii_62047_downsampled_14/inferred_DFE.txt') 

a_muciniphila_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_inferred_DFE.txt')
a_finegoldii_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_inferred_DFE.txt') 
a_onderdonkii_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_inferred_DFE.txt') 
a_putredinis_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_inferred_DFE.txt') 
a_shahii_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Alistipes_shahii_62199_downsampled_14/core_inferred_DFE.txt') 
b_bacterium_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_inferred_DFE.txt') 
b_caccae_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_inferred_DFE.txt') 
b_cellulosilyticus_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_inferred_DFE.txt') 
b_fragilis_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_inferred_DFE.txt') 
b_ovatus_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_inferred_DFE.txt') 
b_stercoris_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_inferred_DFE.txt') 
b_thetaiotaomicron_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_inferred_DFE.txt') 
b_uniformis_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_inferred_DFE.txt') 
b_vulgatus_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_inferred_DFE.txt') 
b_xylanisolvens_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_inferred_DFE.txt') 
b_intestinihominis_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_inferred_DFE.txt') 
d_invisus_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Dialister_invisus_61905_downsampled_14/core_inferred_DFE.txt') 
e_eligens_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_inferred_DFE.txt') 
e_rectale_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_inferred_DFE.txt') 
f_prausnitzii_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_inferred_DFE.txt') 
o_splanchnicus_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_inferred_DFE.txt') 
oscillibacter_sp_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_inferred_DFE.txt') 
p_distasonis_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_inferred_DFE.txt') 
p_merdae_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_inferred_DFE.txt') 
phascolarctobacterium_sp_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_inferred_DFE.txt') 
p_copri_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Prevotella_copri_61740_downsampled_14/core_inferred_DFE.txt') 
r_bicirculans_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_inferred_DFE.txt') 
r_bromii_core_gamma_dfe = gamma_sfs_from_dfe('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_inferred_DFE.txt') 


a_muciniphila_accessory_two_epoch = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_two_epoch_demography.txt')
a_finegoldii_accessory_two_epoch = sfs_from_demography('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_two_epoch_demography.txt') 
a_onderdonkii_accessory_two_epoch = sfs_from_demography('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_two_epoch_demography.txt') 
a_putredinis_accessory_two_epoch = sfs_from_demography('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_two_epoch_demography.txt') 
a_shahii_accessory_two_epoch = sfs_from_demography('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_two_epoch_demography.txt') 
b_bacterium_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_two_epoch_demography.txt') 
b_caccae_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_two_epoch_demography.txt') 
b_cellulosilyticus_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_two_epoch_demography.txt') 
b_fragilis_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_two_epoch_demography.txt') 
b_ovatus_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_two_epoch_demography.txt') 
b_stercoris_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_two_epoch_demography.txt') 
b_thetaiotaomicron_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_two_epoch_demography.txt') 
b_uniformis_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_two_epoch_demography.txt') 
b_vulgatus_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_two_epoch_demography.txt') 
b_xylanisolvens_accessory_two_epoch = sfs_from_demography('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_two_epoch_demography.txt') 
b_intestinihominis_accessory_two_epoch = sfs_from_demography('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_two_epoch_demography.txt') 
d_invisus_accessory_two_epoch = sfs_from_demography('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_two_epoch_demography.txt') 
e_eligens_accessory_two_epoch = sfs_from_demography('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_two_epoch_demography.txt') 
e_rectale_accessory_two_epoch = sfs_from_demography('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_two_epoch_demography.txt') 
f_prausnitzii_accessory_two_epoch = sfs_from_demography('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_two_epoch_demography.txt') 
o_splanchnicus_accessory_two_epoch = sfs_from_demography('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_two_epoch_demography.txt') 
oscillibacter_sp_accessory_two_epoch = sfs_from_demography('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_two_epoch_demography.txt') 
p_distasonis_accessory_two_epoch = sfs_from_demography('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_two_epoch_demography.txt') 
p_merdae_accessory_two_epoch = sfs_from_demography('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_two_epoch_demography.txt') 
phascolarctobacterium_sp_accessory_two_epoch = sfs_from_demography('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_two_epoch_demography.txt') 
p_copri_accessory_two_epoch = sfs_from_demography('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_two_epoch_demography.txt') 
r_bicirculans_accessory_two_epoch = sfs_from_demography('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_two_epoch_demography.txt') 
r_bromii_accessory_two_epoch = sfs_from_demography('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_two_epoch_demography.txt') 

a_muciniphila_accessory_three_epoch = sfs_from_demography('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_three_epoch_demography.txt')
a_finegoldii_accessory_three_epoch = sfs_from_demography('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_three_epoch_demography.txt') 
a_onderdonkii_accessory_three_epoch = sfs_from_demography('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_three_epoch_demography.txt') 
a_putredinis_accessory_three_epoch = sfs_from_demography('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_three_epoch_demography.txt') 
a_shahii_accessory_three_epoch = sfs_from_demography('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_three_epoch_demography.txt') 
b_bacterium_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_three_epoch_demography.txt') 
b_caccae_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_three_epoch_demography.txt') 
b_cellulosilyticus_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_three_epoch_demography.txt') 
b_fragilis_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_three_epoch_demography.txt') 
b_ovatus_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_three_epoch_demography.txt') 
b_stercoris_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_three_epoch_demography.txt') 
b_thetaiotaomicron_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_three_epoch_demography.txt') 
b_uniformis_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_three_epoch_demography.txt') 
b_vulgatus_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_three_epoch_demography.txt') 
b_xylanisolvens_accessory_three_epoch = sfs_from_demography('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_three_epoch_demography.txt') 
b_intestinihominis_accessory_three_epoch = sfs_from_demography('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_three_epoch_demography.txt') 
d_invisus_accessory_three_epoch = sfs_from_demography('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_three_epoch_demography.txt') 
e_eligens_accessory_three_epoch = sfs_from_demography('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_three_epoch_demography.txt') 
e_rectale_accessory_three_epoch = sfs_from_demography('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_three_epoch_demography.txt') 
f_prausnitzii_accessory_three_epoch = sfs_from_demography('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_three_epoch_demography.txt') 
o_splanchnicus_accessory_three_epoch = sfs_from_demography('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_three_epoch_demography.txt') 
oscillibacter_sp_accessory_three_epoch = sfs_from_demography('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_three_epoch_demography.txt') 
p_distasonis_accessory_three_epoch = sfs_from_demography('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_three_epoch_demography.txt') 
p_merdae_accessory_three_epoch = sfs_from_demography('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_three_epoch_demography.txt') 
phascolarctobacterium_sp_accessory_three_epoch = sfs_from_demography('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_three_epoch_demography.txt') 
p_copri_accessory_three_epoch = sfs_from_demography('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_three_epoch_demography.txt') 
r_bicirculans_accessory_three_epoch = sfs_from_demography('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_three_epoch_demography.txt') 
r_bromii_accessory_three_epoch = sfs_from_demography('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_three_epoch_demography.txt') 

a_muciniphila_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/inferred_DFE.txt')
a_finegoldii_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_finegoldii_56071_downsampled_14/inferred_DFE.txt') 
a_onderdonkii_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/inferred_DFE.txt') 
a_putredinis_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_putredinis_61533_downsampled_14/inferred_DFE.txt') 
a_shahii_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_shahii_62199_downsampled_14/inferred_DFE.txt') 
b_bacterium_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/inferred_DFE.txt') 
b_caccae_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_caccae_53434_downsampled_14/inferred_DFE.txt') 
b_cellulosilyticus_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/inferred_DFE.txt') 
b_fragilis_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_fragilis_54507_downsampled_14/inferred_DFE.txt') 
b_ovatus_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_ovatus_58035_downsampled_14/inferred_DFE.txt') 
b_stercoris_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_stercoris_56735_downsampled_14/inferred_DFE.txt') 
b_thetaiotaomicron_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/inferred_DFE.txt') 
b_uniformis_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_uniformis_57318_downsampled_14/inferred_DFE.txt') 
b_vulgatus_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/inferred_DFE.txt') 
b_xylanisolvens_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/inferred_DFE.txt') 
b_intestinihominis_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/inferred_DFE.txt') 
d_invisus_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Dialister_invisus_61905_downsampled_14/inferred_DFE.txt') 
e_eligens_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Eubacterium_eligens_61678_downsampled_14/inferred_DFE.txt') 
e_rectale_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Eubacterium_rectale_56927_downsampled_14/inferred_DFE.txt') 
f_prausnitzii_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/inferred_DFE.txt') 
o_splanchnicus_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/inferred_DFE.txt') 
oscillibacter_sp_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Oscillibacter_sp_60799_downsampled_14/inferred_DFE.txt') 
p_distasonis_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/inferred_DFE.txt') 
p_merdae_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Parabacteroides_merdae_56972_downsampled_14/inferred_DFE.txt') 
phascolarctobacterium_sp_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/inferred_DFE.txt') 
p_copri_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Prevotella_copri_61740_downsampled_14/inferred_DFE.txt') 
r_bicirculans_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/inferred_DFE.txt') 
r_bromii_complete_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Ruminococcus_bromii_62047_downsampled_14/inferred_DFE.txt') 

a_muciniphila_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_inferred_DFE.txt')
a_finegoldii_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_inferred_DFE.txt') 
a_onderdonkii_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_inferred_DFE.txt') 
a_putredinis_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_inferred_DFE.txt') 
a_shahii_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Alistipes_shahii_62199_downsampled_14/core_inferred_DFE.txt') 
b_bacterium_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_inferred_DFE.txt') 
b_caccae_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_inferred_DFE.txt') 
b_cellulosilyticus_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_inferred_DFE.txt') 
b_fragilis_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_inferred_DFE.txt') 
b_ovatus_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_inferred_DFE.txt') 
b_stercoris_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_inferred_DFE.txt') 
b_thetaiotaomicron_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_inferred_DFE.txt') 
b_uniformis_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_inferred_DFE.txt') 
b_vulgatus_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_inferred_DFE.txt') 
b_xylanisolvens_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_inferred_DFE.txt') 
b_intestinihominis_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_inferred_DFE.txt') 
d_invisus_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Dialister_invisus_61905_downsampled_14/core_inferred_DFE.txt') 
e_eligens_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_inferred_DFE.txt') 
e_rectale_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_inferred_DFE.txt') 
f_prausnitzii_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_inferred_DFE.txt') 
o_splanchnicus_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_inferred_DFE.txt') 
oscillibacter_sp_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_inferred_DFE.txt') 
p_distasonis_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_inferred_DFE.txt') 
p_merdae_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_inferred_DFE.txt') 
phascolarctobacterium_sp_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_inferred_DFE.txt') 
p_copri_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Prevotella_copri_61740_downsampled_14/core_inferred_DFE.txt') 
r_bicirculans_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_inferred_DFE.txt') 
r_bromii_core_neugamma_dfe = neugamma_sfs_from_dfe('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_inferred_DFE.txt') 


compare_hmp_sfs(a_muciniphila_hmp_qp_syn, one_epoch_14, a_muciniphila_core_two_epoch, a_muciniphila_core_three_epoch, a_muciniphila_hmp_qp_nonsyn, a_muciniphila_core_gamma_dfe, a_muciniphila_core_neugamma_dfe) + ggtitle('A. muciniphila (Core genes, downsampled to 14)')
compare_hmp_sfs(a_finegoldii_hmp_qp_syn, one_epoch_14, a_finegoldii_core_two_epoch, a_finegoldii_core_three_epoch, a_finegoldii_hmp_qp_nonsyn, a_finegoldii_core_gamma_dfe, a_finegoldii_core_neugamma_dfe) + ggtitle('A. finegoldii (Core genes, downsampled to 14)')
compare_hmp_sfs(a_onderdonkii_hmp_qp_syn, one_epoch_14, a_onderdonkii_core_two_epoch, a_onderdonkii_core_three_epoch, a_onderdonkii_hmp_qp_nonsyn, a_onderdonkii_core_gamma_dfe, a_onderdonkii_core_neugamma_dfe) + ggtitle('A. onderdonkii (Core genes, downsampled to 14)')
compare_hmp_sfs(a_putredinis_hmp_qp_syn, one_epoch_14, a_putredinis_core_two_epoch, a_putredinis_core_three_epoch, a_putredinis_hmp_qp_nonsyn, a_putredinis_core_gamma_dfe, a_putredinis_core_neugamma_dfe) + ggtitle('A. putredinis (Core genes, downsampled to 14)')
compare_hmp_sfs(a_shahii_hmp_qp_syn, one_epoch_14, a_shahii_core_two_epoch, a_shahii_core_three_epoch, a_shahii_hmp_qp_nonsyn, a_shahii_core_gamma_dfe, a_shahii_core_neugamma_dfe) + ggtitle('Alistipes shahii (Core genes, downsampled to 14)')
compare_hmp_sfs(b_bacterium_hmp_qp_syn, one_epoch_14, b_bacterium_core_two_epoch, b_bacterium_core_three_epoch, b_bacterium_hmp_qp_nonsyn, b_bacterium_core_gamma_dfe, b_bacterium_core_neugamma_dfe) + ggtitle('B. bacterium (Core genes, downsampled to 14)')
compare_hmp_sfs(b_caccae_hmp_qp_syn, one_epoch_14, b_caccae_core_two_epoch, b_caccae_core_three_epoch, b_caccae_hmp_qp_nonsyn, b_caccae_core_gamma_dfe, b_caccae_core_neugamma_dfe) + ggtitle('B. caccae (Core genes, downsampled to 14)')
compare_hmp_sfs(b_cellulosilyticus_hmp_qp_syn, one_epoch_14, b_cellulosilyticus_core_two_epoch, b_cellulosilyticus_core_three_epoch, b_cellulosilyticus_hmp_qp_nonsyn, b_cellulosilyticus_core_gamma_dfe, b_cellulosilyticus_core_neugamma_dfe) + ggtitle('B. cellulosilyticus (Core genes, downsampled to 14)')
compare_hmp_sfs(b_fragilis_hmp_qp_syn, one_epoch_14, b_fragilis_core_two_epoch, b_fragilis_core_three_epoch, b_fragilis_hmp_qp_nonsyn, b_fragilis_core_gamma_dfe, b_fragilis_core_neugamma_dfe) + ggtitle('B. fragilis (Core genes, downsampled to 14)')
compare_hmp_sfs(b_ovatus_hmp_qp_syn, one_epoch_14, b_ovatus_core_two_epoch, b_ovatus_core_three_epoch, b_ovatus_hmp_qp_nonsyn, b_ovatus_core_gamma_dfe, b_ovatus_core_neugamma_dfe) + ggtitle('B. ovatus (Core genes, downsampled to 14)')
compare_hmp_sfs(b_stercoris_hmp_qp_syn, one_epoch_14, b_stercoris_core_two_epoch, b_stercoris_core_three_epoch, b_stercoris_hmp_qp_nonsyn, b_stercoris_core_gamma_dfe, b_stercoris_core_neugamma_dfe) + ggtitle('B. stercoris (Core genes, downsampled to 14)')
compare_hmp_sfs(b_thetaiotaomicron_hmp_qp_syn, one_epoch_14, b_thetaiotaomicron_core_two_epoch, b_thetaiotaomicron_core_three_epoch, b_thetaiotaomicron_hmp_qp_nonsyn, b_thetaiotaomicron_core_gamma_dfe, b_thetaiotaomicron_core_neugamma_dfe) + ggtitle('B. thetaiotaomicron (Core genes, downsampled to 14)')
compare_hmp_sfs(b_uniformis_hmp_qp_syn, one_epoch_14, b_uniformis_core_two_epoch, b_uniformis_core_three_epoch, b_uniformis_hmp_qp_nonsyn, b_uniformis_core_gamma_dfe, b_uniformis_core_neugamma_dfe) + ggtitle('B. uniformis (Core genes, downsampled, to 14)')
compare_hmp_sfs(b_vulgatus_hmp_qp_syn, one_epoch_14, b_vulgatus_core_two_epoch, b_vulgatus_core_three_epoch, b_vulgatus_hmp_qp_nonsyn, b_vulgatus_core_gamma_dfe, b_vulgatus_core_neugamma_dfe) + ggtitle('B. vulgatus (Core genes, downsampled to 14)')
compare_hmp_sfs(b_xylanisolvens_hmp_qp_syn, one_epoch_14, b_xylanisolvens_core_two_epoch, b_xylanisolvens_core_three_epoch, b_xylanisolvens_hmp_qp_nonsyn, b_xylanisolvens_core_gamma_dfe, b_xylanisolvens_core_neugamma_dfe) + ggtitle('B. xylanisolvens (Core genes, downsampled to 14)')
compare_hmp_sfs(b_intestinihominis_hmp_qp_syn, one_epoch_14, b_intestinihominis_core_two_epoch, b_intestinihominis_core_three_epoch, b_intestinihominis_hmp_qp_nonsyn, b_intestinihominis_core_gamma_dfe, b_intestinihominis_core_neugamma_dfe) + ggtitle('B. intestinihominis (Core genes, downsampled to 14)')
compare_hmp_sfs(d_invisus_hmp_qp_syn, one_epoch_14, d_invisus_core_two_epoch, d_invisus_core_three_epoch, d_invisus_hmp_qp_nonsyn, d_invisus_core_gamma_dfe, d_invisus_core_neugamma_dfe) + ggtitle('D. invisus (Core genes, downsampled to 14)')
compare_hmp_sfs(e_eligens_hmp_qp_syn, one_epoch_14, e_eligens_core_two_epoch, e_eligens_core_three_epoch, e_eligens_hmp_qp_nonsyn, e_eligens_core_gamma_dfe, e_eligens_core_neugamma_dfe) + ggtitle('E. eligens (Core genes, downsampled to 14)')
compare_hmp_sfs(e_rectale_hmp_qp_syn, one_epoch_14, e_rectale_core_two_epoch, e_rectale_core_three_epoch, e_rectale_hmp_qp_nonsyn, e_rectale_core_gamma_dfe, e_rectale_core_neugamma_dfe) + ggtitle('E. rectale (Core genes, downsampled to 14)')
compare_hmp_sfs(f_prausnitzii_hmp_qp_syn, one_epoch_14, f_prausnitzii_core_two_epoch, f_prausnitzii_core_three_epoch, f_prausnitzii_hmp_qp_nonsyn, f_prausnitzii_core_gamma_dfe, f_prausnitzii_core_neugamma_dfe) + ggtitle('F. prausnitzii (Core genes, downsampled to 14)')
compare_hmp_sfs(o_splanchnicus_hmp_qp_syn, one_epoch_14, o_splanchnicus_core_two_epoch, o_splanchnicus_core_three_epoch,  o_splanchnicus_hmp_qp_nonsyn, o_splanchnicus_core_gamma_dfe, o_splanchnicus_core_neugamma_dfe) + ggtitle('O. splanchnicus (Core genes, downsampled to 14)')
compare_hmp_sfs(oscillibacter_sp_hmp_qp_syn, one_epoch_14, oscillibacter_sp_core_two_epoch, oscillibacter_sp_core_three_epoch, oscillibacter_sp_hmp_qp_nonsyn, oscillibacter_sp_core_gamma_dfe, oscillibacter_sp_core_neugamma_dfe) + ggtitle('Oscillibacter sp. (Core genes, downsampled to 14)')
compare_hmp_sfs(p_distasonis_hmp_qp_syn, one_epoch_14, p_distasonis_core_two_epoch, p_distasonis_core_three_epoch, p_distasonis_hmp_qp_nonsyn, p_distasonis_core_gamma_dfe, p_distasonis_core_neugamma_dfe) + ggtitle('P. distasonis (Core genes, downsampled to 14)')
compare_hmp_sfs(p_merdae_hmp_qp_syn, one_epoch_14, p_merdae_core_two_epoch, p_merdae_core_three_epoch, p_merdae_hmp_qp_nonsyn, p_merdae_core_gamma_dfe, p_merdae_core_neugamma_dfe) + ggtitle('P. merdae (Core genes, downsampled to 14)')
compare_hmp_sfs(phascolarctobacterium_sp_hmp_qp_syn, one_epoch_14, phascolarctobacterium_sp_core_two_epoch, phascolarctobacterium_sp_core_three_epoch, phascolarctobacterium_sp_hmp_qp_nonsyn, phascolarctobacterium_sp_core_gamma_dfe, phascolarctobacterium_sp_core_neugamma_dfe) + ggtitle('Phascolarctobacterium sp. (Core genes, downsampled to 14)')
compare_hmp_sfs(p_copri_hmp_qp_syn, one_epoch_14, p_copri_core_two_epoch, p_copri_core_three_epoch, p_copri_hmp_qp_nonsyn, p_copri_core_gamma_dfe, p_copri_core_neugamma_dfe) + ggtitle('P. copri (Core genes, downsampled to 14)')
compare_hmp_sfs(r_bicirculans_hmp_qp_syn, one_epoch_14, r_bicirculans_core_two_epoch, r_bicirculans_core_three_epoch, r_bicirculans_hmp_qp_nonsyn, r_bicirculans_core_gamma_dfe, r_bicirculans_core_neugamma_dfe) + ggtitle('R. bicirculans (Core genes, downsampled to 14)')
compare_hmp_sfs(r_bromii_hmp_qp_syn, one_epoch_14, r_bromii_core_two_epoch, r_bromii_core_three_epoch, r_bromii_hmp_qp_nonsyn, r_bromii_core_gamma_dfe, r_bromii_core_neugamma_dfe) + ggtitle('R. bromii (Core genes, downsampled to 14)')

# DFE Comparison
a_muciniphila_dfe_params = read_dfe_params('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_inferred_DFE.txt')
a_muciniphila_dfe_params$species = 'Akkermansia muciniphila'

a_finegoldii_dfe_params = read_dfe_params('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_inferred_DFE.txt')
a_finegoldii_dfe_params$species = 'Alistipes finegoldii'

a_onderdonkii_dfe_params = read_dfe_params('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_inferred_DFE.txt')
a_onderdonkii_dfe_params$species = 'Alistipes onderdonkii'

a_putredinis_dfe_params = read_dfe_params('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_inferred_DFE.txt')
a_putredinis_dfe_params$species = 'Alistipes putredinis'

a_shahii_dfe_params = read_dfe_params('../Analysis/Alistipes_shahii_62199_downsampled_14/core_inferred_DFE.txt')
a_shahii_dfe_params$species = 'Alistipes shahii'

b_bacterium_dfe_params = read_dfe_params('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_inferred_DFE.txt')
b_bacterium_dfe_params$species = 'Bacteroidales bacterium'

b_caccae_dfe_params = read_dfe_params('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_inferred_DFE.txt')
b_caccae_dfe_params$species = 'Bacteroides caccae'

b_cellulosilyticus_dfe_params = read_dfe_params('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_inferred_DFE.txt')
b_cellulosilyticus_dfe_params$species = 'Bacteroides cellulosilyticus'

b_fragilis_dfe_params = read_dfe_params('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_inferred_DFE.txt')
b_fragilis_dfe_params$species = 'Bacteroides fragilis'

b_ovatus_dfe_params = read_dfe_params('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_inferred_DFE.txt')
b_ovatus_dfe_params$species = 'Bacteroides ovatus'

b_stercoris_dfe_params = read_dfe_params('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_inferred_DFE.txt')
b_stercoris_dfe_params$species = 'Bacteroides stercoris'

b_thetaiotaomicron_dfe_params = read_dfe_params('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_inferred_DFE.txt')
b_thetaiotaomicron_dfe_params$species = 'Bacteroides thetaiotaomicron'

b_uniformis_dfe_params = read_dfe_params('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_inferred_DFE.txt')
b_uniformis_dfe_params$species = 'Bacteroides uniformis'

b_vulgatus_dfe_params = read_dfe_params('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_inferred_DFE.txt')
b_vulgatus_dfe_params$species = 'Bacteroides vulgatus'

b_xylanisolvens_dfe_params = read_dfe_params('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_inferred_DFE.txt')
b_xylanisolvens_dfe_params$species = 'Bacteroides xylanisolvens'

b_intestinihominis_dfe_params = read_dfe_params('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_inferred_DFE.txt')
b_intestinihominis_dfe_params$species = 'Barnesiella intestinihominis'

d_invisus_dfe_params = read_dfe_params('../Analysis/Dialister_invisus_61905_downsampled_14/core_inferred_DFE.txt')
d_invisus_dfe_params$species = 'Dialister invisus'

e_eligens_dfe_params = read_dfe_params('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_inferred_DFE.txt')
e_eligens_dfe_params$species = 'Eubacterium eligens'

e_rectale_dfe_params = read_dfe_params('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_inferred_DFE.txt')
e_rectale_dfe_params$species = 'Eubacterium rectale'

f_prausnitzii_dfe_params = read_dfe_params('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_inferred_DFE.txt')
f_prausnitzii_dfe_params$species = 'Faecalibacterium prausnitzii'

o_splanchnicus_dfe_params = read_dfe_params('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_inferred_DFE.txt')
o_splanchnicus_dfe_params$species = 'Odoribacter splanchnicus'

oscillibacter_sp_dfe_params = read_dfe_params('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_inferred_DFE.txt')
oscillibacter_sp_dfe_params$species = 'Oscillibacter species'

p_distasonis_dfe_params = read_dfe_params('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_inferred_DFE.txt')
p_distasonis_dfe_params$species = 'Parabacteroides distasonis'

p_merdae_dfe_params = read_dfe_params('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_inferred_DFE.txt')
p_merdae_dfe_params$species = 'Parabacteroides merdae'

phascolarctobacterium_sp_dfe_params = read_dfe_params('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_inferred_DFE.txt')
phascolarctobacterium_sp_dfe_params$species = 'Phascolarctobacterium species'

p_copri_dfe_params = read_dfe_params('../Analysis/Prevotella_copri_61740_downsampled_14/core_inferred_DFE.txt')
p_copri_dfe_params$species = 'Prevotella copri'

r_bicirculans_dfe_params = read_dfe_params('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_inferred_DFE.txt')
r_bicirculans_dfe_params$species = 'Ruminococcus bicirculans'

r_bromii_dfe_params = read_dfe_params('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_inferred_DFE.txt')
r_bromii_dfe_params$species = 'Ruminococcus bromii'

# Dadi Scaling

a_muciniphila_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_inferred_DFE.txt')
a_muciniphila_dfe_dadi_params$species = 'Akkermansia muciniphila'

a_finegoldii_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_inferred_DFE.txt')
a_finegoldii_dfe_dadi_params$species = 'Alistipes finegoldii'

a_onderdonkii_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_inferred_DFE.txt')
a_onderdonkii_dfe_dadi_params$species = 'Alistipes onderdonkii'

a_putredinis_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_inferred_DFE.txt')
a_putredinis_dfe_dadi_params$species = 'Alistipes putredinis'

a_shahii_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Alistipes_shahii_62199_downsampled_14/core_inferred_DFE.txt')
a_shahii_dfe_dadi_params$species = 'Alistipes shahii'

b_bacterium_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_inferred_DFE.txt')
b_bacterium_dfe_dadi_params$species = 'Bacteroidales bacterium'

b_caccae_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_inferred_DFE.txt')
b_caccae_dfe_dadi_params$species = 'Bacteroides caccae'

b_cellulosilyticus_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_inferred_DFE.txt')
b_cellulosilyticus_dfe_dadi_params$species = 'Bacteroides cellulosilyticus'

b_fragilis_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_inferred_DFE.txt')
b_fragilis_dfe_dadi_params$species = 'Bacteroides fragilis'

b_ovatus_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_inferred_DFE.txt')
b_ovatus_dfe_dadi_params$species = 'Bacteroides ovatus'

b_stercoris_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_inferred_DFE.txt')
b_stercoris_dfe_dadi_params$species = 'Bacteroides stercoris'

b_thetaiotaomicron_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_inferred_DFE.txt')
b_thetaiotaomicron_dfe_dadi_params$species = 'Bacteroides thetaiotaomicron'

b_uniformis_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_inferred_DFE.txt')
b_uniformis_dfe_dadi_params$species = 'Bacteroides uniformis'

b_vulgatus_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_inferred_DFE.txt')
b_vulgatus_dfe_dadi_params$species = 'Bacteroides vulgatus'

b_xylanisolvens_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_inferred_DFE.txt')
b_xylanisolvens_dfe_dadi_params$species = 'Bacteroides xylanisolvens'

b_intestinihominis_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_inferred_DFE.txt')
b_intestinihominis_dfe_dadi_params$species = 'Barnesiella intestinihominis'

d_invisus_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Dialister_invisus_61905_downsampled_14/core_inferred_DFE.txt')
d_invisus_dfe_dadi_params$species = 'Dialister invisus'

e_eligens_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_inferred_DFE.txt')
e_eligens_dfe_dadi_params$species = 'Eubacterium eligens'

e_rectale_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_inferred_DFE.txt')
e_rectale_dfe_dadi_params$species = 'Eubacterium rectale'

f_prausnitzii_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_inferred_DFE.txt')
f_prausnitzii_dfe_dadi_params$species = 'Faecalibacterium prausnitzii'

o_splanchnicus_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_inferred_DFE.txt')
o_splanchnicus_dfe_dadi_params$species = 'Odoribacter splanchnicus'

oscillibacter_sp_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_inferred_DFE.txt')
oscillibacter_sp_dfe_dadi_params$species = 'Oscillibacter species'

p_distasonis_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_inferred_DFE.txt')
p_distasonis_dfe_dadi_params$species = 'Parabacteroides distasonis'

p_merdae_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_inferred_DFE.txt')
p_merdae_dfe_dadi_params$species = 'Parabacteroides merdae'

phascolarctobacterium_sp_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_inferred_DFE.txt')
phascolarctobacterium_sp_dfe_dadi_params$species = 'Phascolarctobacterium species'

p_copri_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Prevotella_copri_61740_downsampled_14/core_inferred_DFE.txt')
p_copri_dfe_dadi_params$species = 'Prevotella copri'

r_bicirculans_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_inferred_DFE.txt')
r_bicirculans_dfe_dadi_params$species = 'Ruminococcus bicirculans'

r_bromii_dfe_dadi_params = read_dfe_dadi_params('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_inferred_DFE.txt')
r_bromii_dfe_dadi_params$species = 'Ruminococcus bromii'

dfe_df = rbind(
  melt(a_muciniphila_dfe_params),
  melt(a_finegoldii_dfe_params),
  melt(a_onderdonkii_dfe_params),
  melt(a_putredinis_dfe_params),
  melt(a_shahii_dfe_params),
  melt(b_bacterium_dfe_params),
  melt(b_caccae_dfe_params),
  melt(b_cellulosilyticus_dfe_params),
  melt(b_fragilis_dfe_params),
  melt(b_ovatus_dfe_params),
  melt(b_stercoris_dfe_params),
  melt(b_thetaiotaomicron_dfe_params),
  melt(b_uniformis_dfe_params),
  melt(b_vulgatus_dfe_params),
  melt(b_xylanisolvens_dfe_params),
  melt(b_intestinihominis_dfe_params),
  melt(d_invisus_dfe_params),
  melt(e_eligens_dfe_params),
  melt(e_rectale_dfe_params),
  melt(f_prausnitzii_dfe_params),
  melt(o_splanchnicus_dfe_params),
  melt(oscillibacter_sp_dfe_params),
  melt(p_distasonis_dfe_params),
  melt(p_merdae_dfe_params),
  melt(phascolarctobacterium_sp_dfe_params),
  melt(p_copri_dfe_params),
  melt(r_bicirculans_dfe_params),
  melt(r_bromii_dfe_params)
)

dfe_dadi_df = rbind(
  melt(a_muciniphila_dfe_dadi_params),
  melt(a_finegoldii_dfe_dadi_params),
  melt(a_onderdonkii_dfe_dadi_params),
  melt(a_putredinis_dfe_dadi_params),
  melt(a_shahii_dfe_dadi_params),
  melt(b_bacterium_dfe_dadi_params),
  melt(b_caccae_dfe_dadi_params),
  melt(b_cellulosilyticus_dfe_dadi_params),
  melt(b_fragilis_dfe_dadi_params),
  melt(b_ovatus_dfe_dadi_params),
  melt(b_stercoris_dfe_dadi_params),
  melt(b_thetaiotaomicron_dfe_dadi_params),
  melt(b_uniformis_dfe_dadi_params),
  melt(b_vulgatus_dfe_dadi_params),
  melt(b_xylanisolvens_dfe_dadi_params),
  melt(b_intestinihominis_dfe_dadi_params),
  melt(d_invisus_dfe_dadi_params),
  melt(e_eligens_dfe_dadi_params),
  melt(e_rectale_dfe_dadi_params),
  melt(f_prausnitzii_dfe_dadi_params),
  melt(o_splanchnicus_dfe_dadi_params),
  melt(oscillibacter_sp_dfe_dadi_params),
  melt(p_distasonis_dfe_dadi_params),
  melt(p_merdae_dfe_dadi_params),
  melt(phascolarctobacterium_sp_dfe_dadi_params),
  melt(p_copri_dfe_dadi_params),
  melt(r_bicirculans_dfe_dadi_params),
  melt(r_bromii_dfe_dadi_params)
)
# dfe_df

## Due to Dadi internal scaling which considers genotype
## fitness as (1, 1 + 2sh, 1 + 2s), and the fact that
## we are working with a haploid sample, we
## wish to multiply the _inferred_ s by 2 to get the true
## selective coefficient of the haploid bacteria.
## However, s is for the heterozygote case.

phylogenetic_levels = c(
  'Bacteroidales bacterium',
  'Alistipes putredinis',
  'Alistipes finegoldii',
  'Alistipes onderdonkii',
  'Alistipes shahii',
  'Odoribacter splanchnicus',
  'Parabacteroides distasonis',
  'Parabacteroides merdae',
  'Prevotella copri',
  'Bacteroides fragilis',
  'Bacteroides cellulosilyticus',
  'Bacteroides stercoris',
  'Bacteroides uniformis',
  'Bacteroides thetaiotaomicron',
  'Bacteroides ovatus',
  'Bacteroides xylanisolvens',
  'Bacteroides caccae',
  # 'Bacteroides massiliensis',
  'Bacteroides vulgatus',
  'Barnesiella intestinihominis',
  'Akkermansia muciniphila',
  'Dialister invisus',
  'Phascolarctobacterium species',
  'Eubacterium eligens',
  'Eubacterium rectale',
  # 'Coprococcus species',
  'Oscillibacter species',
  'Ruminococcus bromii',
  'Ruminococcus bicirculans',
  'Faecalibacterium prausnitzii'
)


dfe_df$species = factor(dfe_df$species, levels=phylogenetic_levels)
dfe_dadi_df$species = factor(dfe_dadi_df$species, levels=phylogenetic_levels)

# dfe_df$value = dfe_df$value * 2
dfe_df$value[dfe_df$value <= 1e-12] = 1e-12
dfe_df$value[dfe_df$value >= 1] = 1

### Figure 4
# 600 x 800

# DFE Comparison

ggplot(dfe_df[dfe_df$variable == 'gamma_dfe_dist_low', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 0.95) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  ylab('Proportion of Sites') +
  theme(axis.text.y = element_text(face='italic')) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(axis.title.y.right = element_text('Test')) +
  theme(legend.position = "none") + 
  xlab('Selection Coefficient')

ggplot(dfe_df[dfe_df$variable == 'gamma_dfe_dist_high', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Gamma-Distributed DFE',
    subtitle = 'Assuming a mutation rate of mu=6.93E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.y = element_text(face='italic')) +
  theme(legend.position = "none") + 
  xlab('Selective Effect')

ggplot(dfe_df[dfe_df$variable == 'neugamma_dfe_dist_low', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Neutral + Gamma-Distributed DFE',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.y = element_text(face='italic')) +
  theme(legend.position = "none") + 
  xlab('Selective Effect')

ggplot(dfe_df[dfe_df$variable == 'neugamma_dfe_dist_high', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Neutral + Gamma-Distributed DFE',
    subtitle = 'Assuming a mutation rate of mu=6.93E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.text.y = element_text(face='italic')) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect')

dfe_dadi_df$value[dfe_dadi_df$value <= 1e-1] = 1e-1

ggplot(dfe_dadi_df[dfe_dadi_df$variable == 'gamma_dfe_dist', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Gamma-Distributed DFE',
    subtitle = 'Selective effect multiplied by N_anc'
  ) +
  theme_ridges() +
  scale_x_log10() +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect')

ggplot(dfe_dadi_df[dfe_dadi_df$variable == 'neugamma_dfe_dist', ], aes(x=value, y=fct_rev(species), fill=species)) +
  geom_density_ridges2(aes(fill = species), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Neutral + Gamma-Distributed DFE',
    subtitle = 'Selective effect multiplied by 2N_anc'
  ) +
  theme_ridges() +
  scale_x_log10() +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect')

a_shahii_dfe_df = melt(a_shahii_dfe_params)
# a_shahii_dfe_df$value = a_shahii_dfe_df$value * 2
a_shahii_dfe_df$value[a_shahii_dfe_df$value <= 1e-12] = 1e-12
a_shahii_dfe_df$value[a_shahii_dfe_df$value >= 1] = 1
# a_shahii_dfe_df = a_shahii_dfe_df[a_shahii_dfe_df$variable == 'gamma_dfe_dist_low' || a_shahii_dfe_df$variable == 'neugamma_dfe_dist_low', ]
a_shahii_dfe_df = rbind(
  a_shahii_dfe_df[a_shahii_dfe_df$variable == 'gamma_dfe_dist_low', ],
  a_shahii_dfe_df[a_shahii_dfe_df$variable == 'neugamma_dfe_dist_low',])

a_shahii_dfe_df$variable <- as.character(a_shahii_dfe_df$variable)

a_shahii_dfe_df$variable[a_shahii_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
a_shahii_dfe_df$variable[a_shahii_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

ggplot(a_shahii_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Alistipes shahii DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10() +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

o_splanchnicus_dfe_df = melt(o_splanchnicus_dfe_params)
# o_splanchnicus_dfe_df$value = o_splanchnicus_dfe_df$value * 2
o_splanchnicus_dfe_df$value[o_splanchnicus_dfe_df$value <= 1e-12] = 1e-12
o_splanchnicus_dfe_df$value[o_splanchnicus_dfe_df$value >= 1] = 1
# o_splanchnicus_dfe_df = o_splanchnicus_dfe_df[o_splanchnicus_dfe_df$variable == 'gamma_dfe_dist_low' || o_splanchnicus_dfe_df$variable == 'neugamma_dfe_dist_low', ]
o_splanchnicus_dfe_df = rbind(
  o_splanchnicus_dfe_df[o_splanchnicus_dfe_df$variable == 'gamma_dfe_dist_low', ],
  o_splanchnicus_dfe_df[o_splanchnicus_dfe_df$variable == 'neugamma_dfe_dist_low',])

o_splanchnicus_dfe_df$variable <- as.character(o_splanchnicus_dfe_df$variable)

o_splanchnicus_dfe_df$variable[o_splanchnicus_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
o_splanchnicus_dfe_df$variable[o_splanchnicus_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

o_splanchnicus_dfe_figure = ggplot(o_splanchnicus_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Odoribacter splanchnicus DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

e_eligens_dfe_df = melt(e_eligens_dfe_params)
# e_eligens_dfe_df$value = e_eligens_dfe_df$value * 2
e_eligens_dfe_df$value[e_eligens_dfe_df$value <= 1e-12] = 1e-12
e_eligens_dfe_df$value[e_eligens_dfe_df$value >= 1] = 1
# e_eligens_dfe_df = e_eligens_dfe_df[e_eligens_dfe_df$variable == 'gamma_dfe_dist_low' || e_eligens_dfe_df$variable == 'neugamma_dfe_dist_low', ]
e_eligens_dfe_df = rbind(
  e_eligens_dfe_df[e_eligens_dfe_df$variable == 'gamma_dfe_dist_low', ],
  e_eligens_dfe_df[e_eligens_dfe_df$variable == 'neugamma_dfe_dist_low',])

e_eligens_dfe_df$variable <- as.character(e_eligens_dfe_df$variable)

e_eligens_dfe_df$variable[e_eligens_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
e_eligens_dfe_df$variable[e_eligens_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

e_eligens_dfe_figure = ggplot(e_eligens_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Eubacterium eligens DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

b_cellulosilyticus_dfe_df = melt(b_cellulosilyticus_dfe_params)
b_cellulosilyticus_dfe_df$value[b_cellulosilyticus_dfe_df$value <= 1e-12] = 1e-12
b_cellulosilyticus_dfe_df$value[b_cellulosilyticus_dfe_df$value >= 1] = 1
# b_cellulosilyticus_dfe_df = b_cellulosilyticus_dfe_df[b_cellulosilyticus_dfe_df$variable == 'gamma_dfe_dist_low' || b_cellulosilyticus_dfe_df$variable == 'neugamma_dfe_dist_low', ]
b_cellulosilyticus_dfe_df = rbind(
  b_cellulosilyticus_dfe_df[b_cellulosilyticus_dfe_df$variable == 'gamma_dfe_dist_low', ],
  b_cellulosilyticus_dfe_df[b_cellulosilyticus_dfe_df$variable == 'neugamma_dfe_dist_low',])

b_cellulosilyticus_dfe_df$variable <- as.character(b_cellulosilyticus_dfe_df$variable)

b_cellulosilyticus_dfe_df$variable[b_cellulosilyticus_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
b_cellulosilyticus_dfe_df$variable[b_cellulosilyticus_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

b_cellulosilyticus_dfe_figure = ggplot(b_cellulosilyticus_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Bacteroides cellulosilyticus DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

b_stercoris_dfe_df = melt(b_stercoris_dfe_params)
b_stercoris_dfe_df$value[b_stercoris_dfe_df$value <= 1e-12] = 1e-12
b_stercoris_dfe_df$value[b_stercoris_dfe_df$value >= 1] = 1
# b_stercoris_dfe_df = b_stercoris_dfe_df[b_stercoris_dfe_df$variable == 'gamma_dfe_dist_low' || b_stercoris_dfe_df$variable == 'neugamma_dfe_dist_low', ]
b_stercoris_dfe_df = rbind(
  b_stercoris_dfe_df[b_stercoris_dfe_df$variable == 'gamma_dfe_dist_low', ],
  b_stercoris_dfe_df[b_stercoris_dfe_df$variable == 'neugamma_dfe_dist_low',])

b_stercoris_dfe_df$variable <- as.character(b_stercoris_dfe_df$variable)

b_stercoris_dfe_df$variable[b_stercoris_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
b_stercoris_dfe_df$variable[b_stercoris_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

b_stercoris_dfe_figure = ggplot(b_stercoris_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Bacteroides stercoris DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

a_finegoldii_dfe_df = melt(a_finegoldii_dfe_params)
a_finegoldii_dfe_df$value[a_finegoldii_dfe_df$value <= 1e-12] = 1e-12
a_finegoldii_dfe_df$value[a_finegoldii_dfe_df$value >= 1] = 1
# a_finegoldii_dfe_df = a_finegoldii_dfe_df[a_finegoldii_dfe_df$variable == 'gamma_dfe_dist_low' || a_finegoldii_dfe_df$variable == 'neugamma_dfe_dist_low', ]
a_finegoldii_dfe_df = rbind(
  a_finegoldii_dfe_df[a_finegoldii_dfe_df$variable == 'gamma_dfe_dist_low', ],
  a_finegoldii_dfe_df[a_finegoldii_dfe_df$variable == 'neugamma_dfe_dist_low',])

a_finegoldii_dfe_df$variable <- as.character(a_finegoldii_dfe_df$variable)

a_finegoldii_dfe_df$variable[a_finegoldii_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
a_finegoldii_dfe_df$variable[a_finegoldii_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

a_finegoldii_dfe_figure = ggplot(a_finegoldii_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Bacteroides stercoris DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))


a_onderdonkii_dfe_df = melt(a_onderdonkii_dfe_params)
a_onderdonkii_dfe_df$value[a_onderdonkii_dfe_df$value <= 1e-12] = 1e-12
a_onderdonkii_dfe_df$value[a_onderdonkii_dfe_df$value >= 1] = 1
# a_onderdonkii_dfe_df = a_onderdonkii_dfe_df[a_onderdonkii_dfe_df$variable == 'gamma_dfe_dist_low' || a_onderdonkii_dfe_df$variable == 'neugamma_dfe_dist_low', ]
a_onderdonkii_dfe_df = rbind(
  a_onderdonkii_dfe_df[a_onderdonkii_dfe_df$variable == 'gamma_dfe_dist_low', ],
  a_onderdonkii_dfe_df[a_onderdonkii_dfe_df$variable == 'neugamma_dfe_dist_low',])

a_onderdonkii_dfe_df$variable <- as.character(a_onderdonkii_dfe_df$variable)

a_onderdonkii_dfe_df$variable[a_onderdonkii_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
a_onderdonkii_dfe_df$variable[a_onderdonkii_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

a_onderdonkii_dfe_figure = ggplot(a_onderdonkii_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Bacteroides stercoris DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

e_eligens_dfe_df = melt(e_eligens_dfe_params)
e_eligens_dfe_df$value[e_eligens_dfe_df$value <= 1e-12] = 1e-12
e_eligens_dfe_df$value[e_eligens_dfe_df$value >= 1] = 1
# e_eligens_dfe_df = e_eligens_dfe_df[e_eligens_dfe_df$variable == 'gamma_dfe_dist_low' || e_eligens_dfe_df$variable == 'neugamma_dfe_dist_low', ]
e_eligens_dfe_df = rbind(
  e_eligens_dfe_df[e_eligens_dfe_df$variable == 'gamma_dfe_dist_low', ],
  e_eligens_dfe_df[e_eligens_dfe_df$variable == 'neugamma_dfe_dist_low',])

e_eligens_dfe_df$variable <- as.character(e_eligens_dfe_df$variable)

e_eligens_dfe_df$variable[e_eligens_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
e_eligens_dfe_df$variable[e_eligens_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

e_eligens_dfe_figure = ggplot(e_eligens_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Bacteroides stercoris DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

e_rectale_dfe_df = melt(e_rectale_dfe_params)
e_rectale_dfe_df$value[e_rectale_dfe_df$value <= 1e-12] = 1e-12
e_rectale_dfe_df$value[e_rectale_dfe_df$value >= 1] = 1
# e_rectale_dfe_df = e_rectale_dfe_df[e_rectale_dfe_df$variable == 'gamma_dfe_dist_low' || e_rectale_dfe_df$variable == 'neugamma_dfe_dist_low', ]
e_rectale_dfe_df = rbind(
  e_rectale_dfe_df[e_rectale_dfe_df$variable == 'gamma_dfe_dist_low', ],
  e_rectale_dfe_df[e_rectale_dfe_df$variable == 'neugamma_dfe_dist_low',])

e_rectale_dfe_df$variable <- as.character(e_rectale_dfe_df$variable)

e_rectale_dfe_df$variable[e_rectale_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
e_rectale_dfe_df$variable[e_rectale_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

e_rectale_dfe_figure = ggplot(e_rectale_dfe_df, aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 0.95) +
  labs(
    title = 'Bacteroides stercoris DFE Comparison',
    subtitle = 'Assuming a mutation rate of mu=4.08E-10'
  ) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selective Effect') +
  scale_fill_manual(values=c("goldenrod1", "yellow2"))

a_muciniphila_dfe_df = melt(a_muciniphila_dfe_params)
# a_muciniphila_dfe_df$value = a_muciniphila_dfe_df$value * 2
a_muciniphila_dfe_df$value[a_muciniphila_dfe_df$value <= 1e-12] = 1e-12
a_muciniphila_dfe_df$value[a_muciniphila_dfe_df$value >= 1] = 1
# a_muciniphila_dfe_df = a_muciniphila_dfe_df[a_muciniphila_dfe_df$variable == 'gamma_dfe_dist_low' || a_muciniphila_dfe_df$variable == 'neugamma_dfe_dist_low', ]
a_muciniphila_dfe_df = rbind(
  a_muciniphila_dfe_df[a_muciniphila_dfe_df$variable == 'gamma_dfe_dist_low', ],
  a_muciniphila_dfe_df[a_muciniphila_dfe_df$variable == 'neugamma_dfe_dist_low',])

a_muciniphila_dfe_df$variable <- as.character(a_muciniphila_dfe_df$variable)

a_muciniphila_dfe_df$variable[a_muciniphila_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
a_muciniphila_dfe_df$variable[a_muciniphila_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

r_bromii_dfe_df = melt(r_bromii_dfe_params)
# r_bromii_dfe_df$value = r_bromii_dfe_df$value * 2
r_bromii_dfe_df$value[r_bromii_dfe_df$value <= 1e-12] = 1e-12
r_bromii_dfe_df$value[r_bromii_dfe_df$value >= 1] = 1
# r_bromii_dfe_df = r_bromii_dfe_df[r_bromii_dfe_df$variable == 'gamma_dfe_dist_low' || r_bromii_dfe_df$variable == 'neugamma_dfe_dist_low', ]
r_bromii_dfe_df = rbind(
  r_bromii_dfe_df[r_bromii_dfe_df$variable == 'gamma_dfe_dist_low', ],
  r_bromii_dfe_df[r_bromii_dfe_df$variable == 'neugamma_dfe_dist_low',])

r_bromii_dfe_df$variable <- as.character(r_bromii_dfe_df$variable)

r_bromii_dfe_df$variable[r_bromii_dfe_df$variable == 'neugamma_dfe_dist_low'] <- 'Neutral + Gamma-Distributed DFE'
r_bromii_dfe_df$variable[r_bromii_dfe_df$variable == 'gamma_dfe_dist_low'] <- 'Gamma-Distributed DFE'

a_muciniphila_dfe_figure = ggplot(a_muciniphila_dfe_df[a_muciniphila_dfe_df$variable == 'Gamma-Distributed DFE', ], aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 2.5) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.x = element_blank()) + 
  ylab('Proportion of Sites') +
  theme(legend.position = "none") + 
  xlab('Selection Coefficient') +
  ggtitle('Akkermansia muciniphila') +
  theme(plot.title = element_text(face = "italic")) +
  scale_fill_manual(values=c("#3da1ff"))

r_bromii_dfe_figure = ggplot(r_bromii_dfe_df[r_bromii_dfe_df$variable == 'Gamma-Distributed DFE', ], aes(x=value, y=fct_rev(variable), fill=variable)) +
  geom_density_ridges2(aes(fill = variable), stat = "binline", binwidth = 1, scale = 2.5) +
  theme_ridges() +
  scale_x_log10(limits=c(1e-13, 1e2)) +
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none") + 
  xlab('Selection Coefficient') +
  ggtitle('Ruminococcus bromii') +
  theme(plot.title = element_text(face = "italic")) +
  scale_fill_manual(values=c("#fe61cf"))

a_muciniphila_dfe_figure + r_bromii_dfe_figure + plot_layout(ncol = 1)

# Weakly Deleterious

weakly_deleterious_s =  1E-6
moderately_deleterious_s = 1E-2
lethal_s = 0.99

DFE_cutoffs = c(-Inf, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, Inf)

a_muciniphila_gamma_dfe = a_muciniphila_dfe_df[a_muciniphila_dfe_df$variable=='Gamma-Distributed DFE', ]
r_bromii_gamma_dfe = r_bromii_dfe_df[r_bromii_dfe_df$variable=='Gamma-Distributed DFE', ]

a_muciniphila_gamma_dfe[a_muciniphila_gamma_dfe$value < 1e-13, ]= 1e-13
a_muciniphila_gamma_dfe[a_muciniphila_gamma_dfe$value > 1e2, ]= 1e2
a_muciniphila_gamma_dfe_bins = cut(a_muciniphila_gamma_dfe$value, breaks=DFE_cutoffs)

r_bromii_gamma_dfe[r_bromii_gamma_dfe$value < 1e-13, ]= 1e-13
r_bromii_gamma_dfe[r_bromii_gamma_dfe$value > 1e2, ]= 1e2
r_bromii_gamma_dfe_bins = cut(r_bromii_gamma_dfe$value, breaks=DFE_cutoffs)

a_muciniphila_weakly_deleterious = mean(a_muciniphila_gamma_dfe$value < weakly_deleterious_s)
a_muciniphila_weakly_deleterious
a_muciniphila_moderately_deleterious =  mean(weakly_deleterious_s < a_muciniphila_gamma_dfe$value & a_muciniphila_gamma_dfe$value < moderately_deleterious_s)
a_muciniphila_moderately_deleterious
a_muciniphila_highly_deleterious = mean(moderately_deleterious_s < a_muciniphila_gamma_dfe$value & a_muciniphila_gamma_dfe$value < lethal_s)
a_muciniphila_highly_deleterious
a_muciniphila_lethal = mean(a_muciniphila_gamma_dfe$value > lethal_s)
a_muciniphila_lethal

r_bromii_weakly_deleterious = mean(r_bromii_gamma_dfe$value < weakly_deleterious_s)
r_bromii_weakly_deleterious
r_bromii_moderately_deleterious =  mean(weakly_deleterious_s < r_bromii_gamma_dfe$value & r_bromii_gamma_dfe$value < moderately_deleterious_s)
r_bromii_moderately_deleterious
r_bromii_highly_deleterious = mean(moderately_deleterious_s < r_bromii_gamma_dfe$value & r_bromii_gamma_dfe$value < lethal_s)
r_bromii_highly_deleterious
r_bromii_lethal = mean(r_bromii_gamma_dfe$value > lethal_s)
r_bromii_lethal

b_cellulosilyticus_gamma_dfe = b_cellulosilyticus_dfe_df[b_cellulosilyticus_dfe_df$variable=='Gamma-Distributed DFE', ]
b_stercoris_gamma_dfe = b_stercoris_dfe_df[b_stercoris_dfe_df$variable=='Gamma-Distributed DFE', ]

b_cellulosilyticus_weakly_deleterious = mean(b_cellulosilyticus_gamma_dfe$value < weakly_deleterious_s)
b_cellulosilyticus_weakly_deleterious
b_cellulosilyticus_moderately_deleterious =  mean(weakly_deleterious_s < b_cellulosilyticus_gamma_dfe$value & b_cellulosilyticus_gamma_dfe$value < moderately_deleterious_s)
b_cellulosilyticus_moderately_deleterious
b_cellulosilyticus_highly_deleterious = mean(moderately_deleterious_s < b_cellulosilyticus_gamma_dfe$value & b_cellulosilyticus_gamma_dfe$value < lethal_s)
b_cellulosilyticus_highly_deleterious
b_cellulosilyticus_lethal = mean(b_cellulosilyticus_gamma_dfe$value > lethal_s)
b_cellulosilyticus_lethal

b_cellulosilyticus_gamma_dfe[b_cellulosilyticus_gamma_dfe$value < 1e-13, ]= 1e-13
b_cellulosilyticus_gamma_dfe[b_cellulosilyticus_gamma_dfe$value > 1e2, ]= 1e2
b_cellulosilyticus_gamma_dfe_bins = cut(b_cellulosilyticus_gamma_dfe$value, breaks=DFE_cutoffs)

b_stercoris_weakly_deleterious = mean(b_stercoris_gamma_dfe$value < weakly_deleterious_s)
b_stercoris_weakly_deleterious
b_stercoris_moderately_deleterious =  mean(weakly_deleterious_s < b_stercoris_gamma_dfe$value & b_stercoris_gamma_dfe$value < moderately_deleterious_s)
b_stercoris_moderately_deleterious
b_stercoris_highly_deleterious = mean(moderately_deleterious_s < b_stercoris_gamma_dfe$value & b_stercoris_gamma_dfe$value < lethal_s)
b_stercoris_highly_deleterious
b_stercoris_lethal = mean(b_stercoris_gamma_dfe$value > lethal_s)
b_stercoris_lethal

b_stercoris_gamma_dfe[b_stercoris_gamma_dfe$value < 1e-13, ]= 1e-13
b_stercoris_gamma_dfe[b_stercoris_gamma_dfe$value > 1e2, ]= 1e2
b_stercoris_gamma_dfe_bins = cut(b_stercoris_gamma_dfe$value, breaks=DFE_cutoffs)

e_eligens_gamma_dfe = e_eligens_dfe_df[e_eligens_dfe_df$variable=='Gamma-Distributed DFE', ]
e_rectale_gamma_dfe = e_rectale_dfe_df[e_rectale_dfe_df$variable=='Gamma-Distributed DFE', ]

e_eligens_weakly_deleterious = mean(e_eligens_gamma_dfe$value < weakly_deleterious_s)
e_eligens_weakly_deleterious
e_eligens_moderately_deleterious =  mean(weakly_deleterious_s < e_eligens_gamma_dfe$value & e_eligens_gamma_dfe$value < moderately_deleterious_s)
e_eligens_moderately_deleterious
e_eligens_highly_deleterious = mean(moderately_deleterious_s < e_eligens_gamma_dfe$value & e_eligens_gamma_dfe$value < lethal_s)
e_eligens_highly_deleterious
e_eligens_lethal = mean(e_eligens_gamma_dfe$value > lethal_s)
e_eligens_lethal

e_rectale_weakly_deleterious = mean(e_rectale_gamma_dfe$value < weakly_deleterious_s)
e_rectale_weakly_deleterious
e_rectale_moderately_deleterious =  mean(weakly_deleterious_s < e_rectale_gamma_dfe$value & e_rectale_gamma_dfe$value < moderately_deleterious_s)
e_rectale_moderately_deleterious
e_rectale_highly_deleterious = mean(moderately_deleterious_s < e_rectale_gamma_dfe$value & e_rectale_gamma_dfe$value < lethal_s)
e_rectale_highly_deleterious
e_rectale_lethal = mean(e_rectale_gamma_dfe$value > lethal_s)
e_rectale_lethal

b_cellulosilyticus_dfe_figure + b_stercoris_dfe_figure + plot_layout(ncol=1)

o_splanchnicus_dfe_figure + e_eligens_dfe_figure + b_cellulosilyticus_dfe_figure + plot_layout(ncol = 1)

# all_genes

all_genes_two_epoch_nu_tau = read.csv('../Summary/all_genes_two_epoch_demography_interpretation.csv')
all_genes_three_epoch_nu_tau = read.csv('../Summary/all_genes_three_epoch_demography_interpretation.csv', nrows=1)

all_genes_two_epoch_nu_tau$demography = 'Two Epoch'
all_genes_three_epoch_nu_tau$demography = 'Three Epoch'

all_genes_three_epoch_nu_tau = all_genes_three_epoch_nu_tau[-c(2,4:7)]

colnames(all_genes_two_epoch_nu_tau) = c('species', 'nu', 'time_low', 'time_high', 'demography')
colnames(all_genes_three_epoch_nu_tau) = c('species', 'nu', 'time_low', 'time_high', 'demography')
all_genes_demography_df =  rbind(all_genes_two_epoch_nu_tau, all_genes_three_epoch_nu_tau)

all_genes_demography_df$species = factor(all_genes_demography_df$species, levels=phylogenetic_levels)

all_genes_species_highlight = c('Alistipes shahii', 'Bacteroides vulgatus', 'Oscillibacter species')

all_genes_typeface = ifelse(all_genes_demography_df$species %in% all_genes_species_highlight, 4, 3)

all_genes_demography_df_highlight = all_genes_demography_df[all_genes_demography_df$species %in% all_genes_species_highlight, ]
options(ggrepel.max.overlaps = Inf)
x_label_text = expression(nu == frac(N[current], N[ancestral]))
all_genes_demography_scatter = ggscatter(all_genes_demography_df, x="nu", y="time_low", color='species', shape='demography', size=3) +
  ylab('Estimated time in years since most recent demographic event') +
  xlab(x_label_text) +
  geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
  scale_shape_manual(name = "Best-Fit Demographic Model",
                     labels = c("Three Epoch", "Two Epoch"),
                     values = c(17, 19)) +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=all_genes_typeface) +
  guides(color=guide_legend(title="Species")) +
  scale_x_log10(limits=c(1e-2, 2e4)) +
  scale_y_log10(limits=c(1e3, 1e7)) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none')  +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16))

all_genes_demography_scatter

two_epoch_nu_tau = read.csv('../Summary/two_epoch_demography_interpretation.csv')
three_epoch_nu_tau= read.csv('../Summary/three_epoch_demography_interpretation.csv')

two_epoch_nu_tau$demography = 'Two Epoch'
three_epoch_nu_tau$demography = 'Three Epoch'

three_epoch_nu_tau = three_epoch_nu_tau[-c(2,4:7)]

colnames(three_epoch_nu_tau) = c('species', 'nu', 'time_low', 'time_high', 'demography')
demography_df =  rbind(two_epoch_nu_tau, three_epoch_nu_tau)

# demography_df$time_low = as.numeric(demography_df$time_low)
# demography_df$nu = as.numeric(demography_df$nu)

demography_df$species = factor(demography_df$species, levels=phylogenetic_levels)
temp_demography_df = demography_df
temp_demography_df$species = factor(demography_df$species, levels=all_genes_demography_df$species)
temp_demography_df = na.omit(temp_demography_df)

temp_demography_df <- temp_demography_df[order(temp_demography_df$species), ]
row.names(temp_demography_df) = NULL

temp_demography_df$species
all_genes_demography_df$species

species_highlight = c('Akkermansia muciniphila', 'Ruminococcus bromii')

typeface = ifelse(demography_df$species %in% species_highlight, 5, 4)

demography_df_highlight = demography_df[demography_df$species %in% species_highlight, ]
options(ggrepel.max.overlaps = Inf)
x_label_text = expression(nu == frac(N[current], N[ancestral]))
demography_scatter = ggscatter(demography_df, x="nu", y="time_low", color="species", shape='demography', size=3) +
  ylab('Estimated time in years since most recent demographic event') +
  xlab(x_label_text) +
  geom_vline(xintercept=1.0, color='red', linewidth=1, linetype='dashed') +
  scale_shape_manual(name = "Best-Fit Demographic Model",
                     labels = c("Three Epoch", "Two Epoch"),
                     values = c(17, 19)) +
  geom_text_repel(aes(label = species, color=species, fontface = 'italic'), size=typeface) +
  guides(color=guide_legend(title="Species")) +
  scale_x_log10(limits=c(1e-2, 2e4)) +
  scale_y_log10(limits=c(1e3, 1e7)) +
  theme(legend.position = 'none') +
  guides(color = 'none') +
  guides(shape = 'none')  +
  theme(axis.text=element_text(size=12),
    axis.title=element_text(size=16))

demography_scatter

difference_plot = all_genes_demography_scatter +
  geom_point(data = temp_demography_df, color='green', size=3)  +
  geom_segment(aes(x = all_genes_demography_df$nu, y=all_genes_demography_df$time_low,
    xend = temp_demography_df$nu, yend=temp_demography_df$time_low), linejoin = 'round', lineend='round')

difference_plot

compare_hmp_sfs(b_cellulosilyticus_hmp_qp_syn, one_epoch_14, b_cellulosilyticus_core_two_epoch, b_cellulosilyticus_core_three_epoch, b_cellulosilyticus_hmp_qp_nonsyn, b_cellulosilyticus_complete_gamma_dfe, b_cellulosilyticus_complete_neugamma_dfe) + ggtitle('B. cellulosilyticus (Downsampled to 14)')
compare_hmp_sfs(e_eligens_hmp_qp_syn, one_epoch_14, e_eligens_core_two_epoch, e_eligens_core_three_epoch, e_eligens_hmp_qp_nonsyn, e_eligens_complete_gamma_dfe, e_eligens_complete_neugamma_dfe) + ggtitle('E. eligens (Downsampled to 14)')
compare_hmp_sfs(b_ovatus_hmp_qp_syn, one_epoch_14, b_ovatus_core_two_epoch, b_ovatus_core_three_epoch, b_ovatus_hmp_qp_nonsyn, b_ovatus_complete_gamma_dfe, b_ovatus_complete_neugamma_dfe) + ggtitle('B. ovatus (Downsampled to 14)')

x_axis = 1:length(one_epoch_14)

a_muciniphila_best_fit = cbind(
  proportional_sfs(a_muciniphila_hmp_qp_syn[-1]),
  proportional_sfs(a_muciniphila_core_two_epoch),
  proportional_sfs(a_muciniphila_hmp_qp_nonsyn[-1]),
  proportional_sfs(a_muciniphila_core_gamma_dfe),
  rep('A. muciniphila', length(a_muciniphila_hmp_qp_syn[-1])),
  x_axis
)

a_finegoldii_best_fit = cbind(
  proportional_sfs(a_finegoldii_hmp_qp_syn[-1]),
  proportional_sfs(a_finegoldii_core_two_epoch),
  proportional_sfs(a_finegoldii_hmp_qp_nonsyn[-1]),
  proportional_sfs(a_finegoldii_core_gamma_dfe),
  rep('A. finegoldii', length(a_finegoldii_hmp_qp_syn[-1])),
  x_axis
)

a_onderdonkii_best_fit = cbind(
  proportional_sfs(a_onderdonkii_hmp_qp_syn[-1]),
  proportional_sfs(a_onderdonkii_core_two_epoch),
  proportional_sfs(a_onderdonkii_hmp_qp_nonsyn[-1]),
  proportional_sfs(a_onderdonkii_core_gamma_dfe),
  rep('A. onderdonkii', length(a_onderdonkii_hmp_qp_syn[-1])),
  x_axis
)

a_putredinis_best_fit = cbind(
  proportional_sfs(a_putredinis_hmp_qp_syn[-1]),
  proportional_sfs(a_putredinis_core_two_epoch),
  proportional_sfs(a_putredinis_hmp_qp_nonsyn[-1]),
  proportional_sfs(a_putredinis_core_gamma_dfe),
  rep('A. putredinis', length(a_putredinis_hmp_qp_syn[-1])),
  x_axis
)

a_shahii_best_fit = cbind(
  proportional_sfs(a_shahii_hmp_qp_syn[-1]),
  proportional_sfs(a_shahii_core_two_epoch),
  proportional_sfs(a_shahii_hmp_qp_nonsyn[-1]),
  proportional_sfs(a_shahii_core_gamma_dfe),
  rep('A. shahii', length(a_shahii_hmp_qp_syn[-1])),
  x_axis
)

b_bacterium_best_fit = cbind(
  proportional_sfs(b_bacterium_hmp_qp_syn[-1]),
  proportional_sfs(b_bacterium_core_two_epoch),
  proportional_sfs(b_bacterium_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_bacterium_core_gamma_dfe),
  rep('B. bacterium', length(b_bacterium_hmp_qp_syn[-1])),
  x_axis
)

b_caccae_best_fit = cbind(
  proportional_sfs(b_caccae_hmp_qp_syn[-1]),
  proportional_sfs(b_caccae_core_two_epoch),
  proportional_sfs(b_caccae_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_caccae_core_gamma_dfe),
  rep('B. caccae', length(b_caccae_hmp_qp_syn[-1])),
  x_axis
)


b_cellulosilyticus_best_fit = cbind(
  proportional_sfs(b_cellulosilyticus_hmp_qp_syn[-1]),
  proportional_sfs(b_cellulosilyticus_core_two_epoch),
  proportional_sfs(b_cellulosilyticus_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_cellulosilyticus_core_gamma_dfe),
  rep('B. cellulosilyticus', length(b_cellulosilyticus_hmp_qp_syn[-1])),
  x_axis
)

b_fragilis_best_fit = cbind(
  proportional_sfs(b_fragilis_hmp_qp_syn[-1]),
  proportional_sfs(b_fragilis_core_two_epoch),
  proportional_sfs(b_fragilis_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_fragilis_core_gamma_dfe),
  rep('B. fragilis', length(b_fragilis_hmp_qp_syn[-1])),
  x_axis
)

b_ovatus_best_fit = cbind(
  proportional_sfs(b_ovatus_hmp_qp_syn[-1]),
  proportional_sfs(b_ovatus_core_two_epoch),
  proportional_sfs(b_ovatus_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_ovatus_core_gamma_dfe),
  rep('B. ovatus', length(b_ovatus_hmp_qp_syn[-1])),
  x_axis
)


b_stercoris_best_fit = cbind(
  proportional_sfs(b_stercoris_hmp_qp_syn[-1]),
  proportional_sfs(b_stercoris_core_two_epoch),
  proportional_sfs(b_stercoris_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_stercoris_core_gamma_dfe),
  rep('B. stercoris', length(b_stercoris_hmp_qp_syn[-1])),
  x_axis
)

b_thetaiotaomicron_best_fit = cbind(
  proportional_sfs(b_thetaiotaomicron_hmp_qp_syn[-1]),
  proportional_sfs(b_thetaiotaomicron_core_two_epoch),
  proportional_sfs(b_thetaiotaomicron_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_thetaiotaomicron_core_gamma_dfe),
  rep('B. thetaiotaomicron', length(b_thetaiotaomicron_hmp_qp_syn[-1])),
  x_axis
)

b_uniformis_best_fit = cbind(
  proportional_sfs(b_uniformis_hmp_qp_syn[-1]),
  proportional_sfs(b_uniformis_core_two_epoch),
  proportional_sfs(b_uniformis_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_uniformis_core_gamma_dfe),
  rep('B. uniformis', length(b_uniformis_hmp_qp_syn[-1])),
  x_axis
)


b_vulgatus_best_fit = cbind(
  proportional_sfs(b_vulgatus_hmp_qp_syn[-1]),
  proportional_sfs(b_vulgatus_core_two_epoch),
  proportional_sfs(b_vulgatus_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_vulgatus_core_gamma_dfe),
  rep('B. vulgatus', length(b_vulgatus_hmp_qp_syn[-1])),
  x_axis
)

b_xylanisolvens_best_fit = cbind(
  proportional_sfs(b_xylanisolvens_hmp_qp_syn[-1]),
  proportional_sfs(b_xylanisolvens_core_two_epoch),
  proportional_sfs(b_xylanisolvens_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_xylanisolvens_core_gamma_dfe),
  rep('B. xylanisolvens', length(b_xylanisolvens_hmp_qp_syn[-1])),
  x_axis
)

b_intestinihominis_best_fit = cbind(
  proportional_sfs(b_intestinihominis_hmp_qp_syn[-1]),
  proportional_sfs(b_intestinihominis_core_two_epoch),
  proportional_sfs(b_intestinihominis_hmp_qp_nonsyn[-1]),
  proportional_sfs(b_intestinihominis_core_gamma_dfe),
  rep('B. intestinihominis', length(b_intestinihominis_hmp_qp_syn[-1])),
  x_axis
)

d_invisus_best_fit = cbind(
  proportional_sfs(d_invisus_hmp_qp_syn[-1]),
  proportional_sfs(d_invisus_core_two_epoch),
  proportional_sfs(d_invisus_hmp_qp_nonsyn[-1]),
  proportional_sfs(d_invisus_core_gamma_dfe),
  rep('D. invisus', length(d_invisus_hmp_qp_syn[-1])),
  x_axis
)

e_eligens_best_fit = cbind(
  proportional_sfs(e_eligens_hmp_qp_syn[-1]),
  proportional_sfs(e_eligens_core_two_epoch),
  proportional_sfs(e_eligens_hmp_qp_nonsyn[-1]),
  proportional_sfs(e_eligens_core_gamma_dfe),
  rep('E. eligens', length(e_eligens_hmp_qp_syn[-1])),
  x_axis
)

e_rectale_best_fit = cbind(
  proportional_sfs(e_rectale_hmp_qp_syn[-1]),
  proportional_sfs(e_rectale_core_two_epoch),
  proportional_sfs(e_rectale_hmp_qp_nonsyn[-1]),
  proportional_sfs(e_rectale_core_gamma_dfe),
  rep('E. rectale', length(e_rectale_hmp_qp_syn[-1])),
  x_axis
)

f_prausnitzii_best_fit = cbind(
  proportional_sfs(f_prausnitzii_hmp_qp_syn[-1]),
  proportional_sfs(f_prausnitzii_core_two_epoch),
  proportional_sfs(f_prausnitzii_hmp_qp_nonsyn[-1]),
  proportional_sfs(f_prausnitzii_core_gamma_dfe),
  rep('F. prausnitzii', length(f_prausnitzii_hmp_qp_syn[-1])),
  x_axis
)

o_splanchnicus_best_fit = cbind(
  proportional_sfs(o_splanchnicus_hmp_qp_syn[-1]),
  proportional_sfs(o_splanchnicus_core_two_epoch),
  proportional_sfs(o_splanchnicus_hmp_qp_nonsyn[-1]),
  proportional_sfs(o_splanchnicus_core_gamma_dfe),
  rep('O. splanchnicus', length(o_splanchnicus_hmp_qp_syn[-1])),
  x_axis
)

oscillibacter_sp_best_fit = cbind(
  proportional_sfs(oscillibacter_sp_hmp_qp_syn[-1]),
  proportional_sfs(oscillibacter_sp_core_three_epoch),
  proportional_sfs(oscillibacter_sp_hmp_qp_nonsyn[-1]),
  proportional_sfs(oscillibacter_sp_core_gamma_dfe),
  rep('Oscilibacter sp', length(oscillibacter_sp_hmp_qp_syn[-1])),
  x_axis
)

p_distasonis_best_fit = cbind(
  proportional_sfs(p_distasonis_hmp_qp_syn[-1]),
  proportional_sfs(p_distasonis_core_two_epoch),
  proportional_sfs(p_distasonis_hmp_qp_nonsyn[-1]),
  proportional_sfs(p_distasonis_core_gamma_dfe),
  rep('P. distasonis', length(p_distasonis_hmp_qp_syn[-1])),
  x_axis
)

p_merdae_best_fit = cbind(
  proportional_sfs(p_merdae_hmp_qp_syn[-1]),
  proportional_sfs(p_merdae_core_two_epoch),
  proportional_sfs(p_merdae_hmp_qp_nonsyn[-1]),
  proportional_sfs(p_merdae_core_gamma_dfe),
  rep('P. merdae', length(p_merdae_hmp_qp_syn[-1])),
  x_axis
)

phascolarctobacterium_sp_best_fit = cbind(
  proportional_sfs(phascolarctobacterium_sp_hmp_qp_syn[-1]),
  proportional_sfs(phascolarctobacterium_sp_core_two_epoch),
  proportional_sfs(phascolarctobacterium_sp_hmp_qp_nonsyn[-1]),
  proportional_sfs(phascolarctobacterium_sp_core_gamma_dfe),
  rep('Phascolarctobacterium sp', length(phascolarctobacterium_sp_hmp_qp_syn[-1])),
  x_axis
)

p_copri_best_fit = cbind(
  proportional_sfs(p_copri_hmp_qp_syn[-1]),
  proportional_sfs(p_copri_core_two_epoch),
  proportional_sfs(p_copri_hmp_qp_nonsyn[-1]),
  proportional_sfs(p_copri_core_gamma_dfe),
  rep('P. copri', length(p_copri_hmp_qp_syn[-1])),
  x_axis
)

r_bicirculans_best_fit = cbind(
  proportional_sfs(r_bicirculans_hmp_qp_syn[-1]),
  proportional_sfs(r_bicirculans_core_two_epoch),
  proportional_sfs(r_bicirculans_hmp_qp_nonsyn[-1]),
  proportional_sfs(r_bicirculans_core_gamma_dfe),
  rep('R. bicirculans', length(r_bicirculans_hmp_qp_syn[-1])),
  x_axis
)

r_bromii_best_fit = cbind(
  proportional_sfs(r_bromii_hmp_qp_syn[-1]),
  proportional_sfs(r_bromii_core_two_epoch),
  proportional_sfs(r_bromii_hmp_qp_nonsyn[-1]),
  proportional_sfs(r_bromii_core_gamma_dfe),
  rep('R. bromii', length(r_bromii_hmp_qp_syn[-1])),
  x_axis
)

a_muciniphila_best_fit_accessory = cbind(
  proportional_sfs(a_muciniphila_hmp_qp_syn_accessory[-1]),
  proportional_sfs(a_muciniphila_accessory_two_epoch),
  proportional_sfs(a_muciniphila_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(a_muciniphila_complete_gamma_dfe),
  rep('A. muciniphila', length(a_muciniphila_hmp_qp_syn_accessory[-1])),
  x_axis
)

a_finegoldii_best_fit_accessory = cbind(
  proportional_sfs(a_finegoldii_hmp_qp_syn_accessory[-1]),
  proportional_sfs(a_finegoldii_accessory_two_epoch),
  proportional_sfs(a_finegoldii_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(a_finegoldii_complete_gamma_dfe),
  rep('A. finegoldii', length(a_finegoldii_hmp_qp_syn_accessory[-1])),
  x_axis
)

a_onderdonkii_best_fit_accessory = cbind(
  proportional_sfs(a_onderdonkii_hmp_qp_syn_accessory[-1]),
  proportional_sfs(a_onderdonkii_accessory_two_epoch),
  proportional_sfs(a_onderdonkii_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(a_onderdonkii_complete_gamma_dfe),
  rep('A. onderdonkii', length(a_onderdonkii_hmp_qp_syn_accessory[-1])),
  x_axis
)

a_putredinis_best_fit_accessory = cbind(
  proportional_sfs(a_putredinis_hmp_qp_syn_accessory[-1]),
  proportional_sfs(a_putredinis_accessory_two_epoch),
  proportional_sfs(a_putredinis_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(a_putredinis_complete_gamma_dfe),
  rep('A. putredinis', length(a_putredinis_hmp_qp_syn_accessory[-1])),
  x_axis
)

a_shahii_best_fit_accessory = cbind(
  proportional_sfs(a_shahii_hmp_qp_syn_accessory[-1]),
  proportional_sfs(a_shahii_accessory_two_epoch),
  proportional_sfs(a_shahii_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(a_shahii_complete_gamma_dfe),
  rep('A. shahii', length(a_shahii_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_bacterium_best_fit_accessory = cbind(
  proportional_sfs(b_bacterium_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_bacterium_accessory_two_epoch),
  proportional_sfs(b_bacterium_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_bacterium_complete_gamma_dfe),
  rep('B. bacterium', length(b_bacterium_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_caccae_best_fit_accessory = cbind(
  proportional_sfs(b_caccae_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_caccae_accessory_two_epoch),
  proportional_sfs(b_caccae_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_caccae_complete_gamma_dfe),
  rep('B. caccae', length(b_caccae_hmp_qp_syn_accessory[-1])),
  x_axis
)


b_cellulosilyticus_best_fit_accessory = cbind(
  proportional_sfs(b_cellulosilyticus_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_cellulosilyticus_accessory_two_epoch),
  proportional_sfs(b_cellulosilyticus_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_cellulosilyticus_complete_gamma_dfe),
  rep('B. cellulosilyticus', length(b_cellulosilyticus_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_fragilis_best_fit_accessory = cbind(
  proportional_sfs(b_fragilis_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_fragilis_accessory_two_epoch),
  proportional_sfs(b_fragilis_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_fragilis_complete_gamma_dfe),
  rep('B. fragilis', length(b_fragilis_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_ovatus_best_fit_accessory = cbind(
  proportional_sfs(b_ovatus_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_ovatus_accessory_two_epoch),
  proportional_sfs(b_ovatus_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_ovatus_complete_gamma_dfe),
  rep('B. ovatus', length(b_ovatus_hmp_qp_syn_accessory[-1])),
  x_axis
)


b_stercoris_best_fit_accessory = cbind(
  proportional_sfs(b_stercoris_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_stercoris_accessory_two_epoch),
  proportional_sfs(b_stercoris_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_stercoris_complete_gamma_dfe),
  rep('B. stercoris', length(b_stercoris_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_thetaiotaomicron_best_fit_accessory = cbind(
  proportional_sfs(b_thetaiotaomicron_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_thetaiotaomicron_accessory_two_epoch),
  proportional_sfs(b_thetaiotaomicron_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_thetaiotaomicron_complete_gamma_dfe),
  rep('B. thetaiotaomicron', length(b_thetaiotaomicron_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_uniformis_best_fit_accessory = cbind(
  proportional_sfs(b_uniformis_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_uniformis_accessory_two_epoch),
  proportional_sfs(b_uniformis_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_uniformis_complete_gamma_dfe),
  rep('B. uniformis', length(b_uniformis_hmp_qp_syn_accessory[-1])),
  x_axis
)


b_vulgatus_best_fit_accessory = cbind(
  proportional_sfs(b_vulgatus_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_vulgatus_accessory_two_epoch),
  proportional_sfs(b_vulgatus_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_vulgatus_complete_gamma_dfe),
  rep('B. vulgatus', length(b_vulgatus_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_xylanisolvens_best_fit_accessory = cbind(
  proportional_sfs(b_xylanisolvens_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_xylanisolvens_accessory_two_epoch),
  proportional_sfs(b_xylanisolvens_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_xylanisolvens_complete_gamma_dfe),
  rep('B. xylanisolvens', length(b_xylanisolvens_hmp_qp_syn_accessory[-1])),
  x_axis
)

b_intestinihominis_best_fit_accessory = cbind(
  proportional_sfs(b_intestinihominis_hmp_qp_syn_accessory[-1]),
  proportional_sfs(b_intestinihominis_accessory_two_epoch),
  proportional_sfs(b_intestinihominis_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(b_intestinihominis_complete_gamma_dfe),
  rep('B. intestinihominis', length(b_intestinihominis_hmp_qp_syn_accessory[-1])),
  x_axis
)

d_invisus_best_fit_accessory = cbind(
  proportional_sfs(d_invisus_hmp_qp_syn_accessory[-1]),
  proportional_sfs(d_invisus_accessory_two_epoch),
  proportional_sfs(d_invisus_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(d_invisus_complete_gamma_dfe),
  rep('D. invisus', length(d_invisus_hmp_qp_syn_accessory[-1])),
  x_axis
)

e_eligens_best_fit_accessory = cbind(
  proportional_sfs(e_eligens_hmp_qp_syn_accessory[-1]),
  proportional_sfs(e_eligens_accessory_two_epoch),
  proportional_sfs(e_eligens_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(e_eligens_complete_gamma_dfe),
  rep('E. eligens', length(e_eligens_hmp_qp_syn_accessory[-1])),
  x_axis
)

e_rectale_best_fit_accessory = cbind(
  proportional_sfs(e_rectale_hmp_qp_syn_accessory[-1]),
  proportional_sfs(e_rectale_accessory_two_epoch),
  proportional_sfs(e_rectale_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(e_rectale_complete_gamma_dfe),
  rep('E. rectale', length(e_rectale_hmp_qp_syn_accessory[-1])),
  x_axis
)

f_prausnitzii_best_fit_accessory = cbind(
  proportional_sfs(f_prausnitzii_hmp_qp_syn_accessory[-1]),
  proportional_sfs(f_prausnitzii_accessory_two_epoch),
  proportional_sfs(f_prausnitzii_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(f_prausnitzii_complete_gamma_dfe),
  rep('F. prausnitzii', length(f_prausnitzii_hmp_qp_syn_accessory[-1])),
  x_axis
)

o_splanchnicus_best_fit_accessory = cbind(
  proportional_sfs(o_splanchnicus_hmp_qp_syn_accessory[-1]),
  proportional_sfs(o_splanchnicus_accessory_two_epoch),
  proportional_sfs(o_splanchnicus_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(o_splanchnicus_complete_gamma_dfe),
  rep('O. splanchnicus', length(o_splanchnicus_hmp_qp_syn_accessory[-1])),
  x_axis
)

oscillibacter_sp_best_fit_accessory = cbind(
  proportional_sfs(oscillibacter_sp_hmp_qp_syn_accessory[-1]),
  proportional_sfs(oscillibacter_sp_accessory_three_epoch),
  proportional_sfs(oscillibacter_sp_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(oscillibacter_sp_complete_gamma_dfe),
  rep('Oscilibacter sp', length(oscillibacter_sp_hmp_qp_syn_accessory[-1])),
  x_axis
)

p_distasonis_best_fit_accessory = cbind(
  proportional_sfs(p_distasonis_hmp_qp_syn_accessory[-1]),
  proportional_sfs(p_distasonis_accessory_two_epoch),
  proportional_sfs(p_distasonis_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(p_distasonis_complete_gamma_dfe),
  rep('P. distasonis', length(p_distasonis_hmp_qp_syn_accessory[-1])),
  x_axis
)

p_merdae_best_fit_accessory = cbind(
  proportional_sfs(p_merdae_hmp_qp_syn_accessory[-1]),
  proportional_sfs(p_merdae_accessory_two_epoch),
  proportional_sfs(p_merdae_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(p_merdae_complete_gamma_dfe),
  rep('P. merdae', length(p_merdae_hmp_qp_syn_accessory[-1])),
  x_axis
)

phascolarctobacterium_sp_best_fit_accessory = cbind(
  proportional_sfs(phascolarctobacterium_sp_hmp_qp_syn_accessory[-1]),
  proportional_sfs(phascolarctobacterium_sp_accessory_two_epoch),
  proportional_sfs(phascolarctobacterium_sp_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(phascolarctobacterium_sp_complete_gamma_dfe),
  rep('Phascolarctobacterium sp', length(phascolarctobacterium_sp_hmp_qp_syn_accessory[-1])),
  x_axis
)

p_copri_best_fit_accessory = cbind(
  proportional_sfs(p_copri_hmp_qp_syn_accessory[-1]),
  proportional_sfs(p_copri_accessory_two_epoch),
  proportional_sfs(p_copri_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(p_copri_complete_gamma_dfe),
  rep('P. copri', length(p_copri_hmp_qp_syn_accessory[-1])),
  x_axis
)

r_bicirculans_best_fit_accessory = cbind(
  proportional_sfs(r_bicirculans_hmp_qp_syn_accessory[-1]),
  proportional_sfs(r_bicirculans_accessory_two_epoch),
  proportional_sfs(r_bicirculans_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(r_bicirculans_complete_gamma_dfe),
  rep('R. bicirculans', length(r_bicirculans_hmp_qp_syn_accessory[-1])),
  x_axis
)

r_bromii_best_fit_accessory = cbind(
  proportional_sfs(r_bromii_hmp_qp_syn_accessory[-1]),
  proportional_sfs(r_bromii_accessory_two_epoch),
  proportional_sfs(r_bromii_hmp_qp_nonsyn_accessory[-1]),
  proportional_sfs(r_bromii_complete_gamma_dfe),
  rep('R. bromii', length(r_bromii_hmp_qp_syn_accessory[-1])),
  x_axis
)


# 800 x 1200
# Phylogenetic Tree in APE

get_species_code_reference = function(code, reference) {
  if (code %in% reference$species_id) {
    return(reference[reference$species_id == code, ]$midas_number)
  } else if(code %in% reference$midas_number) {
    return(reference[reference$midas_number == code, ]$species_id)
  }
}

# Read the input TSV file
input_table <- read.table("../Data/midas_tree/midas_db_v1.2/species_info.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Extract the midas_number from the species_id column
input_table$midas_number <- sub(".+_(\\d+)$", "\\1", input_table$species_id)

# Create a new data frame with species_id and midas_number columns
output_table <- data.frame(species_id = input_table$species_id, midas_number = input_table$midas_number)

# Save the output as a CSV file
write.csv(output_table, file = "../Data/midas_tree/midas_db_v1.2/species_code_reference.txt", row.names = FALSE)

# Print the output data frame
print(output_table)

# Load the ape package
# library(ape)

# Read the Newick file
input_tree <- read.tree("../Data/midas_tree/midas_db_v1.2/species_tree.newick")

# Specify the tip labels for the subtree you want to extract
species_subtree = c(
  'Akkermansia_muciniphila_55290',
  'Alistipes_finegoldii_56071',
  'Alistipes_onderdonkii_55464',
  # 'Alistipes_putredinis_61533',
  'Alistipes_shahii_62199',
  'Bacteroidales_bacterium_58650',
  # 'Bacteroides_caccae_53434',
  'Bacteroides_cellulosilyticus_58046',
  'Bacteroides_fragilis_54507',
  # 'Bacteroides_massiliensis_44749',
  # 'Bacteroides_ovatus_58035',
  'Bacteroides_stercoris_56735',
  'Bacteroides_thetaiotaomicron_56941',
  # 'Bacteroides_uniformis_57318',
  'Bacteroides_vulgatus_57955',
  'Bacteroides_xylanisolvens_57185',
  'Barnesiella_intestinihominis_62208',
  # 'Coprococcus_sp_62244',
  # 'Dialister_invisus_61905',
  'Eubacterium_eligens_61678',
  'Eubacterium_rectale_56927',
  'Faecalibacterium_prausnitzii_57453',
  'Odoribacter_splanchnicus_62174',
  'Oscillibacter_sp_60799',
  'Parabacteroides_distasonis_56985',
  'Parabacteroides_merdae_56972',
  'Phascolarctobacterium_sp_59817',
  'Prevotella_copri_61740',
  'Ruminococcus_bicirculans_59300',
  'Ruminococcus_bromii_62047'
)

midas_code_subtree = c()

for (species in species_subtree) {
  midas_code_subtree = c(midas_code_subtree, get_species_code_reference(species, output_table))
}

print(midas_code_subtree)

subtree_tips = midas_code_subtree

# Extract the subtree
subtree <- keep.tip(input_tree, subtree_tips)

# Print the extracted subtree
print(subtree)

for (i in 1:length(subtree$tip.label)) {
  print(subtree$tip.label[i])
  new_tip = get_species_code_reference(subtree$tip.label[i], output_table)
  # new_tip = str_sub(new_tip, 1, str_length(new_tip)-6)
  subtree$tip.label[i] = new_tip
  print(subtree$tip.label[i])
}

plot(subtree)
write.tree(subtree, file='../Summary/good_species.newick')

# Likelihood Surfaces for Core Genes
plot_likelihood_surface_contour('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. finegoldii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. onderdonkii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_shahii_62199_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. shahii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. bacterium likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. caccae likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. cellulosilyticus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. fragilis likelihood surface')
# plot_likelihood_surface('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. ovatus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. stercoris likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. thetaiotaomicron likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. vulgatus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. xylanisolvens likelihood surface')
plot_likelihood_surface_contour('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_likelihood_surface.csv') + ggtitle('B. intestinihominis likelihood surface')
# plot_likelihood_surface('../Analysis/Coprococcus_sp_62244_downsampled_14/core_likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Dialister_invisus_61905_downsampled_14/core_likelihood_surface.csv') + ggtitle('D. invisus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_likelihood_surface.csv') + ggtitle('E. eligens likelihood surface')
plot_likelihood_surface_contour('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_likelihood_surface.csv') + ggtitle('E. rectale likelihood surface')
plot_likelihood_surface_contour('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_likelihood_surface.csv') + ggtitle('F. prausnitzii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_likelihood_surface.csv') + ggtitle('O. splanchnicus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_likelihood_surface.csv') + ggtitle('Oscillibacter species likelihood surface')
plot_likelihood_surface_contour('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_likelihood_surface.csv') + ggtitle('P. distasonis likelihood surface')
plot_likelihood_surface_contour('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_likelihood_surface.csv') + ggtitle('P. merdae likelihood surface')
plot_likelihood_surface_contour('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_likelihood_surface.csv') + ggtitle('Phascolarctobacterium species likelihood surface')
plot_likelihood_surface_contour('../Analysis/Prevotella_copri_61740_downsampled_14/core_likelihood_surface.csv') + ggtitle('P. copri likelihood surface')
plot_likelihood_surface_contour('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_likelihood_surface.csv') + ggtitle('R. bicirculans likelihood surface')
plot_likelihood_surface_contour('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_likelihood_surface.csv') + ggtitle('R. bromii likelihood surface')

# Likelihood Surfaces for accessory genes
plot_likelihood_surface_contour('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_finegoldii_56071_downsampled_14/likelihood_surface.csv') + ggtitle('A. finegoldii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/likelihood_surface.csv') + ggtitle('A. onderdonkii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_putredinis_61533_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Alistipes_shahii_62199_downsampled_14/likelihood_surface.csv') + ggtitle('A. shahii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/likelihood_surface.csv') + ggtitle('B. bacterium likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_caccae_53434_downsampled_14/likelihood_surface.csv') + ggtitle('B. caccae likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/likelihood_surface.csv') + ggtitle('B. cellulosilyticus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/likelihood_surface.csv') + ggtitle('B. cellulosilyticus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_fragilis_54507_downsampled_14/likelihood_surface.csv') + ggtitle('B. fragilis likelihood surface')
# plot_likelihood_surface('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_ovatus_58035_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_stercoris_56735_downsampled_14/likelihood_surface.csv') + ggtitle('B. stercoris likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/likelihood_surface.csv') + ggtitle('B. thetaiotaomicron likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_uniformis_57318_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/likelihood_surface.csv') + ggtitle('B. vulgatus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/likelihood_surface.csv') + ggtitle('B. xylanisolvens likelihood surface')
plot_likelihood_surface_contour('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/likelihood_surface.csv') + ggtitle('B. intestinihominis likelihood surface')
# plot_likelihood_surface('../Analysis/Coprococcus_sp_62244_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Dialister_invisus_61905_downsampled_14/likelihood_surface.csv') + ggtitle('A. muciniphila likelihood surface')
plot_likelihood_surface_contour('../Analysis/Eubacterium_eligens_61678_downsampled_14/likelihood_surface.csv') + ggtitle('E. eligens likelihood surface')
plot_likelihood_surface_contour('../Analysis/Eubacterium_eligens_61678_downsampled_14/likelihood_surface.csv') + ggtitle('E. eligens likelihood surface')
plot_likelihood_surface_contour('../Analysis/Eubacterium_rectale_56927_downsampled_14/likelihood_surface.csv') + ggtitle('E. rectale likelihood surface')
plot_likelihood_surface_contour('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/likelihood_surface.csv') + ggtitle('F. prausnitzii likelihood surface')
plot_likelihood_surface_contour('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/likelihood_surface.csv') + ggtitle('O. splanchnicus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/likelihood_surface.csv') + ggtitle('O. splanchnicus likelihood surface')
plot_likelihood_surface_contour('../Analysis/Oscillibacter_sp_60799_downsampled_14/likelihood_surface.csv') + ggtitle('Oscillibacter species likelihood surface')
plot_likelihood_surface_contour('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/likelihood_surface.csv') + ggtitle('P. distasonis likelihood surface')
plot_likelihood_surface_contour('../Analysis/Parabacteroides_merdae_56972_downsampled_14/likelihood_surface.csv') + ggtitle('P. merdae likelihood surface')
plot_likelihood_surface_contour('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/likelihood_surface.csv') + ggtitle('Phascolarctobacterium species likelihood surface')
plot_likelihood_surface_contour('../Analysis/Prevotella_copri_61740_downsampled_14/likelihood_surface.csv') + ggtitle('P. copri likelihood surface')
plot_likelihood_surface_contour('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/likelihood_surface.csv') + ggtitle('R. bicirculans likelihood surface')
plot_likelihood_surface_contour('../Analysis/Ruminococcus_bromii_62047_downsampled_14/likelihood_surface.csv') + ggtitle('R. bromii likelihood surface')

p1 = plot_best_fit_sfs(a_muciniphila_best_fit) + ggtitle('Akkermansia muciniphila')
p2 = plot_best_fit_sfs(a_finegoldii_best_fit) + ggtitle('Alistipes finegoldii')
p3 = plot_best_fit_sfs(a_onderdonkii_best_fit) + ggtitle('Alistipes onderdonkii')
p4 = plot_best_fit_sfs(a_putredinis_best_fit)  + ggtitle('Alistipes putredinis')
p5 = plot_best_fit_sfs(a_shahii_best_fit) + ggtitle('Alistipes shahii')
p6 = plot_best_fit_sfs(b_bacterium_best_fit) + ggtitle('Bacteroidales bacterium')
p7 = plot_best_fit_sfs(b_caccae_best_fit) + ggtitle('Bacteroides caccae')
p8 = plot_best_fit_sfs(b_cellulosilyticus_best_fit) + ggtitle('Bacteroides cellulosilyticus')
p9 = plot_best_fit_sfs(b_fragilis_best_fit) + ggtitle('Bacteroides fragilis')
# p10 = plot_best_fit_sfs(_best_fit)
p11 = plot_best_fit_sfs(b_ovatus_best_fit) + ggtitle('Bacteroides ovatus')
p12 = plot_best_fit_sfs(b_stercoris_best_fit) + ggtitle('Bacteroides stercoris')
p13 = plot_best_fit_sfs(b_thetaiotaomicron_best_fit) + ggtitle('Bacteroides thetaiotaomicron')
p14 = plot_best_fit_sfs(b_uniformis_best_fit) + ggtitle('Bacteroides uniformis')
p15 = plot_best_fit_sfs(b_vulgatus_best_fit) + ggtitle('Bacteroides vulgatus')
p16 = plot_best_fit_sfs(b_xylanisolvens_best_fit) + ggtitle('Bacteroides xylanisolvens')
p17 = plot_best_fit_sfs(b_intestinihominis_best_fit) + ggtitle('Barnesiella intestinihominis')
# p18 = plot_best_fit_sfs(_best_fit)
p19 = plot_best_fit_sfs(d_invisus_best_fit) + ggtitle('Dialister invisus')
p20 = plot_best_fit_sfs(e_eligens_best_fit) + ggtitle('Eubacterium eligens')
p21 = plot_best_fit_sfs(e_rectale_best_fit) + ggtitle('Eubacterium rectale')
p22 = plot_best_fit_sfs(f_prausnitzii_best_fit) + ggtitle('Faecalibacterium prausnitzii')
p23 = plot_best_fit_sfs(o_splanchnicus_best_fit) + ggtitle('Odoribacter splanchnicus')
p24 = plot_best_fit_sfs(oscillibacter_sp_best_fit) + ggtitle('Oscillibacter species')
p25 = plot_best_fit_sfs(p_distasonis_best_fit) + ggtitle('Parabacteroides distasonis')
p26 = plot_best_fit_sfs(p_merdae_best_fit) + ggtitle('Parabacteroides merdae')
p27 = plot_best_fit_sfs(phascolarctobacterium_sp_best_fit) + ggtitle('Phascolarctobacterium species')
p28 = plot_best_fit_sfs(p_copri_best_fit) + ggtitle('Prevotella copri')
p29 = plot_best_fit_sfs(r_bicirculans_best_fit) + ggtitle('Ruminococcus bicirculans')
p30 = plot_best_fit_sfs(r_bromii_best_fit) + ggtitle('Ruminococcus bromii')

p1_l = plot_likelihood_surface_contour('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_likelihood_surface.csv')
p2_l = plot_likelihood_surface_contour('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_likelihood_surface.csv')
p3_l = plot_likelihood_surface_contour('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_likelihood_surface.csv')
p4_l = plot_likelihood_surface_contour('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_likelihood_surface.csv')
p5_l = plot_likelihood_surface_contour('../Analysis/Alistipes_shahii_62199_downsampled_14/core_likelihood_surface.csv')
p6_l = plot_likelihood_surface_contour('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_likelihood_surface.csv')
p7_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_likelihood_surface.csv')
p8_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_likelihood_surface.csv')
p9_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_likelihood_surface.csv')
# p10 = plot_likelihood_surface_contour('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/core_likelihood_surface.csv')
p11_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_likelihood_surface.csv')
p12_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_likelihood_surface.csv')
p13_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_likelihood_surface.csv')
p14_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_likelihood_surface.csv')
p15_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_likelihood_surface.csv')
p16_l = plot_likelihood_surface_contour('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_likelihood_surface.csv')
p17_l = plot_likelihood_surface_contour('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_likelihood_surface.csv')
# p18 = plot_likelihood_surface_contour('../Analysis/Coprococcus_sp_62244_downsampled_14/core_likelihood_surface.csv')
p19_l = plot_likelihood_surface_contour('../Analysis/Dialister_invisus_61905_downsampled_14/core_likelihood_surface.csv')
p20_l = plot_likelihood_surface_contour('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_likelihood_surface.csv')
p21_l = plot_likelihood_surface_contour('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_likelihood_surface.csv')
p22_l = plot_likelihood_surface_contour('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_likelihood_surface.csv')
p23_l = plot_likelihood_surface_contour('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_likelihood_surface.csv')
p24_l = plot_likelihood_surface_contour('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_likelihood_surface.csv')
p25_l = plot_likelihood_surface_contour('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_likelihood_surface.csv')
p26_l = plot_likelihood_surface_contour('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_likelihood_surface.csv')
p27_l = plot_likelihood_surface_contour('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_likelihood_surface.csv')
p28_l = plot_likelihood_surface_contour('../Analysis/Prevotella_copri_61740_downsampled_14/core_likelihood_surface.csv')
p29_l = plot_likelihood_surface_contour('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_likelihood_surface.csv')
p30_l = plot_likelihood_surface_contour('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_likelihood_surface.csv')

p1a = plot_best_fit_sfs(a_muciniphila_best_fit_accessory) + ggtitle('Akkermansia muciniphila')
p2a = plot_best_fit_sfs(a_finegoldii_best_fit_accessory) + ggtitle('Alistipes finegoldii')
p3a = plot_best_fit_sfs(a_onderdonkii_best_fit_accessory) + ggtitle('Alistipes onderdonkii')
p4a = plot_best_fit_sfs(a_putredinis_best_fit_accessory)  + ggtitle('Alistipes putredinis')
p5a = plot_best_fit_sfs(a_shahii_best_fit_accessory) + ggtitle('Alistipes shahii')
p6a = plot_best_fit_sfs(b_bacterium_best_fit_accessory) + ggtitle('Bacteroidales bacterium')
p7a = plot_best_fit_sfs(b_caccae_best_fit_accessory) + ggtitle('Bacteroides caccae')
p8a = plot_best_fit_sfs(b_cellulosilyticus_best_fit_accessory) + ggtitle('Bacteroides cellulosilyticus')
p9a = plot_best_fit_sfs(b_fragilis_best_fit_accessory) + ggtitle('Bacteroides fragilis')
# p10a = plot_best_fit_sfs(_best_fit_accessory)
p11a = plot_best_fit_sfs(b_ovatus_best_fit_accessory) + ggtitle('Bacteroides ovatus')
p12a = plot_best_fit_sfs(b_stercoris_best_fit_accessory) + ggtitle('Bacteroides stercoris')
p13a = plot_best_fit_sfs(b_thetaiotaomicron_best_fit_accessory) + ggtitle('Bacteroides thetaiotaomicron')
p14a = plot_best_fit_sfs(b_uniformis_best_fit_accessory) + ggtitle('Bacteroides uniformis')
p15a = plot_best_fit_sfs(b_vulgatus_best_fit_accessory) + ggtitle('Bacteroides vulgatus')
p16a = plot_best_fit_sfs(b_xylanisolvens_best_fit_accessory) + ggtitle('Bacteroides xylanisolvens')
p17a = plot_best_fit_sfs(b_intestinihominis_best_fit_accessory) + ggtitle('Barnesiella intestinihominis')
# p18a = plot_best_fit_sfs(_best_fit_accessory)
p19a = plot_best_fit_sfs(d_invisus_best_fit_accessory) + ggtitle('Dialister invisus')
p20a = plot_best_fit_sfs(e_eligens_best_fit_accessory) + ggtitle('Eubacterium eligens')
p21a = plot_best_fit_sfs(e_rectale_best_fit_accessory) + ggtitle('Eubacterium rectale')
p22a = plot_best_fit_sfs(f_prausnitzii_best_fit_accessory) + ggtitle('Faecalibacterium prausnitzii')
p23a = plot_best_fit_sfs(o_splanchnicus_best_fit_accessory) + ggtitle('Odoribacter splanchnicus')
p24a = plot_best_fit_sfs(oscillibacter_sp_best_fit_accessory) + ggtitle('Oscillibacter species')
p25a = plot_best_fit_sfs(p_distasonis_best_fit_accessory) + ggtitle('Parabacteroides distasonis')
p26a = plot_best_fit_sfs(p_merdae_best_fit_accessory) + ggtitle('Parabacteroides merdae')
p27a = plot_best_fit_sfs(phascolarctobacterium_sp_best_fit_accessory) + ggtitle('Phascolarctobacterium species')
p28a = plot_best_fit_sfs(p_copri_best_fit_accessory) + ggtitle('Prevotella copri')
p29a = plot_best_fit_sfs(r_bicirculans_best_fit_accessory) + ggtitle('Ruminococcus bicirculans')
p30a = plot_best_fit_sfs(r_bromii_best_fit_accessory) + ggtitle('Ruminococcus bromii')


p1_l_accessory = plot_likelihood_surface_contour('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_likelihood_surface.csv')
p2_l_accessory = plot_likelihood_surface_contour('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_likelihood_surface.csv')
p3_l_accessory = plot_likelihood_surface_contour('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_likelihood_surface.csv')
p4_l_accessory = plot_likelihood_surface_contour('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_likelihood_surface.csv')
p5_l_accessory = plot_likelihood_surface_contour('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_likelihood_surface.csv')
p6_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_likelihood_surface.csv')
p7_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_likelihood_surface.csv')
p8_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_likelihood_surface.csv')
p9_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_likelihood_surface.csv')
# p10 = plot_likelihood_surface_contour('../Analysis/Bacteroides_massiliensis_44749_downsampled_14/accessory_likelihood_surface.csv')
p11_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_likelihood_surface.csv')
p12_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_likelihood_surface.csv')
p13_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_likelihood_surface.csv')
p14_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_likelihood_surface.csv')
p15_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_likelihood_surface.csv')
p16_l_accessory = plot_likelihood_surface_contour('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_likelihood_surface.csv')
p17_l_accessory = plot_likelihood_surface_contour('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_likelihood_surface.csv')
# p18 = plot_likelihood_surface_contour('../Analysis/Coprococcus_sp_62244_downsampled_14/accessory_likelihood_surface.csv')
p19_l_accessory = plot_likelihood_surface_contour('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_likelihood_surface.csv')
p20_l_accessory = plot_likelihood_surface_contour('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_likelihood_surface.csv')
p21_l_accessory = plot_likelihood_surface_contour('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_likelihood_surface.csv')
p22_l_accessory = plot_likelihood_surface_contour('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_likelihood_surface.csv')
p23_l_accessory = plot_likelihood_surface_contour('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_likelihood_surface.csv')
p24_l_accessory = plot_likelihood_surface_contour('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_likelihood_surface.csv')
p25_l_accessory = plot_likelihood_surface_contour('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_likelihood_surface.csv')
p26_l_accessory = plot_likelihood_surface_contour('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_likelihood_surface.csv')
p27_l_accessory = plot_likelihood_surface_contour('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_likelihood_surface.csv')
p28_l_accessory = plot_likelihood_surface_contour('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_likelihood_surface.csv')
p29_l_accessory = plot_likelihood_surface_contour('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_likelihood_surface.csv')
p30_l_accessory = plot_likelihood_surface_contour('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_likelihood_surface.csv')

phylogenetic_levels = c(
  'Bacteroidales bacterium',
  'Alistipes putredinis',
  'Alistipes finegoldii',
  'Alistipes onderdonkii',
  'Alistipes shahii',
  'Odoribacter splanchnicus',
  'Parabacteroides distasonis',
  'Parabacteroides merdae',
  'Prevotella copri',
  'Bacteroides fragilis',
  'Bacteroides cellulosilyticus',
  'Bacteroides stercoris',
  'Bacteroides uniformis',
  'Bacteroides thetaiotaomicron',
  'Bacteroides ovatus',
  'Bacteroides xylanisolvens',
  'Bacteroides caccae',
  # 'Bacteroides massiliensis',
  'Bacteroides vulgatus',
  'Barnesiella intestinihominis',
  'Akkermansia muciniphila',
  'Dialister invivus',
  'Phascolarctobacterium species',
  'Eubacterium eligens',
  'Eubacterium rectale',
  # 'Coprococcus species',
  'Oscillibacter species',
  'Ruminococcus bromii',
  'Ruminococcus bicirculans',
  'Faecalibacterium prausnitzii'
)


# 1600 x 16000
sfs_and_likelihood = p6 + p6_l +
  p4 + p4_l +
  p2 + p2_l + 
  p3 + p3_l + 
  p5 + p5_l +
  p23 + p23_l +
  p25 + p25_l +
  p26 + p26_l  +
  p28 + p28_l +
  p9 + p9_l +
  p8 + p8_l +
  p12 + p12_l +
  p14 + p14_l +
  p13 + p13_l +
  p11 + p11_l +
  p16 + p16_l +
  p7 + p7_l +
  p15 + p15_l +
  p17 + p17_l +
  p1 + p1_l +
  p19 + p19_l +
  p27 + p27_l +
  p20 + p20_l +
  p21 + p21_l +
  p24 + p24_l +
  p30 + p30_l +
  p29 + p29_l +
  p22 + p22_l +
  plot_layout(ncol=2) 

sfs_and_likelihood

sfs_and_likelihood_accessory = p6a + p6_l_accessory +
  p4a + p4_l_accessory +
  p2a + p2_l_accessory + 
  p3a + p3_l_accessory + 
  p5a + p5_l_accessory +
  p23a + p23_l_accessory +
  p25a + p25_l_accessory +
  p26a + p26_l_accessory  +
  p28a + p28_l_accessory +
  p9a + p9_l_accessory +
  p8a + p8_l_accessory +
  p12a + p12_l_accessory +
  p14a + p14_l_accessory +
  p13a + p13_l_accessory +
  p11a + p11_l_accessory +
  p16a + p16_l_accessory +
  p7a + p7_l_accessory +
  p15a + p15_l_accessory +
  p17a + p17_l_accessory +
  p1a + p1_l_accessory +
  p19a + p19_l_accessory +
  p27a + p27_l_accessory +
  p20a + p20_l_accessory +
  p21a + p21_l_accessory +
  p24a + p24_l_accessory +
  p30a + p30_l_accessory +
  p29a + p29_l_accessory +
  p22a + p22_l_accessory +
  plot_layout(ncol=2) 

sfs_and_likelihood_accessory

# 3200 x 16000

# Reorder area distribution
# Take out E. eligens
# Rescale legends to match font size
# Legend in upper right corner of figure

### Figure 3

design = c(
  area(1, 1, 1, 1),
  area(1, 2, 1, 2),
  area(2, 1, 2, 1),
  area(2, 2, 2, 2),
  area(1, 3, 2, 5)
)

p1 = plot_best_fit_sfs_3A(a_muciniphila_best_fit) + ggtitle('Akkermansia muciniphila')
p1_l = plot_likelihood_surface_contour_3C('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_likelihood_surface.csv')

p30 = plot_best_fit_sfs_3B(r_bromii_best_fit) + ggtitle('Ruminococcus bromii')
p30_l = plot_likelihood_surface_contour_3D('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_likelihood_surface.csv')

# 2000 x 900
p1 + p1_l + # A. muciniphila
  p30 + p30_l + #R. bicirculans
  demography_scatter +
  plot_layout(design=design)

# Nucleotide diversity

pi_summary_df = data.frame(read.csv('../Analysis/summarized_pi.csv', header=TRUE))
names(pi_summary_df) = c('species', 'Cohort', 'average_pi', 'num_sites', 
                         'num_samples', 'aggregate_across_pi', 'pairwise_across_pi')

list_iid = c('Bifidobacterium_longum_57796',
             'Blautia_wexlerae_56130',
             'Butyrivibrio_crossotus_61674',
             'Escherichia_coli_58110',
             'Eubacterium_eligens_61678',
             'Faecalibacterium_cf_62236',
             'Faecalibacterium_prausnitzii_57453',
             'Faecalibacterium_prausnitzii_61481',
             'Faecalibacterium_prausnitzii_62201',
             'Oscillibacter_sp_60799',
             'Prevotella_copri_61740',
             'Roseburia_inulinivorans_61943',
             'Ruminococcus_bromii_62047')

over_iid_df = subset(pi_summary_df, species %in% list_iid)

position_jitterdodge(
  jitter.width = NULL,
  jitter.height = 0,
  dodge.width = 0.5,
  seed = NA
)

# pairwise_pi_comparison_10 + stat_compare_means(label = "p.signif", method = "t.test")

over_iid_df$ordered_pi = 0

over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' HMP', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$ordered_pi = 
  mean(over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

unique(over_iid_df$species)
over_iid_df$species[over_iid_df$species == 'Bifidobacterium_longum_57796'] = 'Bifidobacterium longum'
over_iid_df$species[over_iid_df$species == 'Blautia_wexlerae_56130'] = 'Blautia wexlerae'
over_iid_df$species[over_iid_df$species == 'Butyrivibrio_crossotus_61674'] = 'Butyrivibrio crossotus'
over_iid_df$species[over_iid_df$species == 'Eubacterium_eligens_61678'] = 'Eubacterium eligens'
over_iid_df$species[over_iid_df$species == 'Escherichia_coli_58110'] = 'Escherichia coli'
over_iid_df$species[over_iid_df$species == 'Faecalibacterium_prausnitzii_61481'] = 'Faecalibacterium prausnitzii (61481)'
over_iid_df$species[over_iid_df$species == 'Faecalibacterium_prausnitzii_57453'] = 'Faecalibacterium prausnitzii (57453)'
over_iid_df$species[over_iid_df$species == 'Faecalibacterium_prausnitzii_62201'] = 'Faecalibacterium prausnitzii (62201)'
over_iid_df$species[over_iid_df$species == 'Faecalibacterium_cf_62236'] = 'Faecalibacterium cf'
over_iid_df$species[over_iid_df$species == 'Oscillibacter_sp_60799'] = 'Oscillibacter species'
over_iid_df$species[over_iid_df$species == 'Prevotella_copri_61740'] = 'Prevotella copri'
over_iid_df$species[over_iid_df$species == 'Roseburia_inulinivorans_61943'] = 'Roseburia inulinovrans'
over_iid_df$species[over_iid_df$species == 'Ruminococcus_bromii_62047'] = 'Ruminococcus bromii'

better_pi_comparison_iid <- ggplot(data=over_iid_df, aes(x=reorder(species, ordered_pi), y=average_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  geom_point(aes(x=species, y=pairwise_across_pi, color=Cohort), size=4, shape=16, position=position_dodge(width=0.75)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(size=rel(1.5), angle=40, vjust=1.0, hjust=1)) +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size=rel(1.5))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x,)),
                limits = c(0.0001, 0.2)) +
  scale_fill_manual(values=c('dodgerblue3', 'goldenrod1')) +
  scale_color_manual(values=c('dodgerblue3', 'goldenrod1')) +
  theme(axis.text.x=element_text(face="italic")) +
  theme(legend.position = "none") +
  xlab('Species') + 
  ylab('Nucleotide diversity') +
  # ggtitle('Distribution of within and between-host nucleotide diversity across cohorts') +
  # stat_compare_means(method='wilcox.test', label='p.signif', size=6)
  stat_compare_means(method='wilcox.test', size=2)
better_pi_comparison_iid

distinct_iid_df = distinct(over_iid_df, pairwise_across_pi, .keep_all=TRUE)
distinct_df = distinct(pi_summary_df, pairwise_across_pi, .keep_all=TRUE)
HMP_distinct = distinct_df[distinct_df$Cohort==' HMP', ]$pairwise_across_pi
African_distinct = distinct_df[distinct_df$Cohort==' African', ]$pairwise_across_pi

HMP_distinct_pairwise = distinct_iid_df[distinct_iid_df$Cohort==' HMP', ]$pairwise_across_pi
African_distinct_pairwise = distinct_iid_df[distinct_iid_df$Cohort==' African', ]$pairwise_across_pi

wilcox.test(HMP_distinct_pairwise, African_distinct_pairwise, paired=TRUE, exact=TRUE)
wilcox.test(HMP_distinct, African_distinct, paired=FALSE, exact=TRUE)

hmp_summary_df = pi_summary_df[pi_summary_df$Cohort==' HMP', ]
afr_summary_df = pi_summary_df[pi_summary_df$Cohort==' African', ]

# Number of species in each cohort
length(unique(hmp_summary_df$species))
length(unique(afr_summary_df$species))

# Number of shared species
sum(unique(afr_summary_df$species) %in% unique(hmp_summary_df$species))

afr_species_freq = data.frame(table(afr_summary_df$species))
hmp_species_freq = data.frame(table(hmp_summary_df$species))

over_5_afr_species = afr_species_freq[afr_species_freq$Freq > 5, ]$Var1
over_5_hmp_species = hmp_species_freq[hmp_species_freq$Freq > 5, ]$Var1

over_5_species = unique(c(over_5_afr_species, over_5_hmp_species))

over_5_species_df = pi_summary_df[pi_summary_df$species %in% over_5_species, ]

# over_5_species_df = drop_duplicates(over_5_species_df, keep=False)
over_5_species_df = over_5_species_df[!duplicated(over_5_species_df$pairwise_across_pi), ]

over_5_species_df = over_5_species_df[order(over_5_species_df$species), ]
over_5_species_df

compare_iid_over_5_means <- ggplot(data=over_5_species_df, aes(x=Cohort, y=pairwise_across_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(size=rel(1.5), angle=40, vjust=1.0, hjust=1)) +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=rel(1.5))) +
  theme(legend.title = element_text(size=rel(1.5))) +
  scale_fill_manual(values=c('dodgerblue3', 'goldenrod1')) +
  scale_color_manual(values=c('dodgerblue3', 'goldenrod1')) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x,)),
                limits = c(0.0001, 0.2)) +
  ylab('Mean Nucleotide diversity per Species') +
  # ylim(0, 0.03) +
  stat_compare_means(method='wilcox.test', label='p.signif', size=6)
  # ggtitle('Mean Nucleotide diversity per Species between Cohorts')
compare_iid_over_5_means

### Figure 1

better_pi_comparison_iid + compare_iid_over_5_means + plot_layout(widths = c(3, 1))
# 1500 x 900 dimensions for saved image



# Plot DFE Grid

DFE_grid_file_list = c(
  '../Analysis/cross_species_dfe/Bacteroidales_bacterium_58650_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Alistipes_finegoldii_56071_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Alistipes_onderdonkii_55464_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Alistipes_shahii_62199_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Odoribacter_splanchnicus_62174_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Parabacteroides_distasonis_56985_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Parabacteroides_merdae_56972_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Prevotella_copri_61740_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Bacteroides_fragilis_54507_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Bacteroides_cellulosilyticus_58046_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Bacteroides_stercoris_56735_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Bacteroides_thetaiotaomicron_56941_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Bacteroides_xylanisolvens_57185_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Bacteroides_vulgatus_57955_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Barnesiella_intestinihominis_62208_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Akkermansia_muciniphila_55290_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Phascolarctobacterium_sp_59817_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Eubacterium_eligens_61678_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Eubacterium_rectale_56927_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Oscillibacter_sp_60799_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Ruminococcus_bromii_62047_likelihood_surface.csv',
  '../Analysis/cross_species_dfe/Ruminococcus_bicirculans_59300_likelihood_surface.csv', 
  '../Analysis/cross_species_dfe/Faecalibacterium_prausnitzii_57453_likelihood_surface.csv')

for (species in DFE_grid_file_list) {
  print(species)
  find_dfe_mle(species)
}

# test
cross_species_dfe_comparison(
  '../Analysis/cross_species_dfe/Akkermansia_muciniphila_55290_likelihood_surface.csv',
  '../Analysis/cross_species_dfe/Alistipes_finegoldii_56071_likelihood_surface.csv')

# dfe_comparison_matrix = matrix(, nrow=23, ncol=23)

#for (i in 1:23) {
#  for (j in 1:23) {
#    print(DFE_grid_file_list[i])
#    print(DFE_grid_file_list[j])
#    comparison = cross_species_dfe_comparison(DFE_grid_file_list[i], DFE_grid_file_list[j])
#    print(comparison)
#    dfe_comparison_matrix[i, j] = comparison
#  }
#}

#row.names(dfe_comparison_matrix) = phylogenetic_levels
#colnames(dfe_comparison_matrix) = phylogenetic_levels
#dfe_comparison_matrix

# write.csv(dfe_comparison_matrix, '../Analysis/cross_species_dfe/dfe_comparison_matrix.csv')

dfe_comparison_matrix = read.csv('../Analysis/cross_species_dfe/dfe_comparison_matrix.csv',
  header=TRUE)

# dfe_comparison_matrix = dfe_comparison_matrix[, -1]
# rownames(dfe_comparison_matrix) = phylogenetic_levels
# colnames(dfe_comparison_matrix) = phylogenetic_levels
# dfe_comparison_matrix

# pheatmap(dfe_comparison_matrix, 
#   cluster_rows = F, cluster_cols = F, fontsize_number = 10,
#   display_numbers = T,
#   show_colnames     = FALSE,
#   show_rownames     = FALSE,
#   color = colorRampPalette(c('red','orange', 'yellow', 'white'), bias=0.05)(100),
#   legend=TRUE)

# Core vs. Accessory genes
## Proportional

b_bacterium_all = read_input_sfs('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_syn_downsampled_sfs.txt')
b_bacterium_core = read_input_sfs('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_bacterium_accessory = read_input_sfs('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(b_bacterium_all,
  b_bacterium_core,
  b_bacterium_accessory) + ggtitle('B. bacterium synonymous')

a_finegoldii_all = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_syn_downsampled_sfs.txt')
a_finegoldii_core = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_finegoldii_accessory = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_finegoldii_all,
  a_finegoldii_core,
  a_finegoldii_accessory) + ggtitle('A. finegoldii synonymous')

a_onderdonkii_all = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_syn_downsampled_sfs.txt')
a_onderdonkii_core = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_onderdonkii_accessory = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_onderdonkii_all,
  a_onderdonkii_core,
  a_onderdonkii_accessory) + ggtitle('A. onderdonkii synonymous')

a_shahii_all = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_syn_downsampled_sfs.txt')
a_shahii_core = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_shahii_accessory = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_shahii_all,
  a_shahii_core,
  a_shahii_accessory) + ggtitle('A. shahii synonymous')

o_splanchnicus_all = read_input_sfs('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_syn_downsampled_sfs.txt') 
o_splanchnicus_core = read_input_sfs('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
o_splanchnicus_accessory = read_input_sfs('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(o_splanchnicus_all,
  o_splanchnicus_core,
  o_splanchnicus_accessory) + ggtitle('O. splanchnicus synonymous')

p_distasonis_all = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_syn_downsampled_sfs.txt') 
p_distasonis_core = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
p_distasonis_accessory = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_distasonis_all,
  p_distasonis_core,
  p_distasonis_accessory) + ggtitle('P. distasonis synonymous')

p_merdae_all = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_syn_downsampled_sfs.txt') 
p_merdae_core = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
p_merdae_accessory = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_merdae_all,
  p_merdae_core,
  p_merdae_accessory) + ggtitle('P. merdae synonymous')

p_copri_all = read_input_sfs('../Analysis/Prevotella_copri_61740_downsampled_14/empirical_syn_downsampled_sfs.txt') 
p_copri_core = read_input_sfs('../Analysis/Prevotella_copri_61740_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
p_copri_accessory = read_input_sfs('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_copri_all,
  p_copri_core,
  p_copri_accessory) + ggtitle('P. copri synonymous')

b_fragilis_all = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_fragilis_core = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_fragilis_accessory = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_fragilis_all,
  b_fragilis_core,
  b_fragilis_accessory) + ggtitle('B. fragilis synonymous')

b_cellulosilyticus_all = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_cellulosilyticus_core = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_cellulosilyticus_accessory = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_cellulosilyticus_all,
  b_cellulosilyticus_core,
  b_cellulosilyticus_accessory) + ggtitle('B. cellulosilyticus synonymous')

b_stercoris_all = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_stercoris_core = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_stercoris_accessory = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_stercoris_all,
  b_stercoris_core,
  b_stercoris_accessory) + ggtitle('B. stercoris synonymous')

b_thetaiotaomicron_all = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_thetaiotaomicron_core = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_thetaiotaomicron_accessory = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_thetaiotaomicron_all,
  b_thetaiotaomicron_core,
  b_thetaiotaomicron_accessory) + ggtitle('B. thetaiotaomicron synonymous')

b_xylanisolvens_all = read_input_sfs('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_xylanisolvens_core = read_input_sfs('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_xylanisolvens_accessory = read_input_sfs('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_xylanisolvens_all,
  b_xylanisolvens_core,
  b_xylanisolvens_accessory) + ggtitle('B. xylanisolvens synonymous')

b_vulgatus_all = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_vulgatus_core = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_vulgatus_accessory = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_vulgatus_all,
  b_vulgatus_core,
  b_vulgatus_accessory) + ggtitle('B. vulgatus synonymous')

b_intestinihominis_all = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_syn_downsampled_sfs.txt') 
b_intestinihominis_core = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
b_intestinihominis_accessory = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_intestinihominis_all,
  b_intestinihominis_core,
  b_intestinihominis_accessory) + ggtitle('B. intestinihominis')

a_muciniphila_all = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_syn_downsampled_sfs.txt')
a_muciniphila_core = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_muciniphila_accessory = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_muciniphila_all,
  a_muciniphila_core,
  a_muciniphila_accessory)  + ggtitle('A. muciniphila synonymous')

p_sp_all = read_input_sfs('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_syn_downsampled_sfs.txt') 
p_sp_core = read_input_sfs('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
p_sp_accessory = read_input_sfs('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_sp_all,
  p_sp_core,
  p_sp_accessory) + ggtitle('Phascolarctobacterium species synonymous')

e_eligens_all = read_input_sfs('../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_syn_downsampled_sfs.txt') 
e_eligens_core = read_input_sfs('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
e_eligens_accessory = read_input_sfs('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(e_eligens_all,
  e_eligens_core,
  e_eligens_accessory) + ggtitle('E. eligens synonymous')

e_rectale_all = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_syn_downsampled_sfs.txt') 
e_rectale_core = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
e_rectale_accessory = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(e_rectale_all,
  e_rectale_core,
  e_rectale_accessory) + ggtitle('E. rectale synonymous')

o_sp_all = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_syn_downsampled_sfs.txt') 
o_sp_core = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
o_sp_accessory = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(o_sp_all,
  o_sp_core,
  o_sp_accessory) + ggtitle('Oscillibacter species synonymous')

r_bromii_all = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_syn_downsampled_sfs.txt')
r_bromii_core = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
r_bromii_accessory = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(r_bromii_all,
  r_bromii_core,
  r_bromii_accessory) + ggtitle('Ruminococcus bromii synonymous')

r_bicirculans_all = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_syn_downsampled_sfs.txt') 
r_bicirculans_core = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_syn_downsampled_sfs.txt') 
r_bicirculans_accessory = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt') 

compare_core_accessory_sfs(r_bicirculans_all,
  r_bicirculans_core,
  r_bicirculans_accessory) + ggtitle('Ruminococcus bicirculans synonymous')

f_prausnitzii_all = read_input_sfs('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_syn_downsampled_sfs.txt')
f_prausnitzii_core = read_input_sfs('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
f_prausnitzii_accessory = read_input_sfs('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

compare_core_accessory_sfs(f_prausnitzii_all,
  f_prausnitzii_core,
  f_prausnitzii_accessory) + ggtitle('F. prausnitzii synonymous')

# Nonsyn

b_bacterium_all_ns = read_input_sfs('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
b_bacterium_core_ns = read_input_sfs('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
b_bacterium_accessory_ns = read_input_sfs('../Analysis/Bacteroidales_bacterium_58650_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(b_bacterium_all_ns,
  b_bacterium_core_ns,
  b_bacterium_accessory_ns) + ggtitle('B. bacterium nonsynonymous')

a_finegoldii_all_ns = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
a_finegoldii_core_ns = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
a_finegoldii_accessory_ns = read_input_sfs('../Analysis/Alistipes_finegoldii_56071_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_finegoldii_all_ns,
  a_finegoldii_core_ns,
  a_finegoldii_accessory_ns) + ggtitle('A. finegoldii nonsynonymous')

a_onderdonkii_all_ns = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
a_onderdonkii_core_ns = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
a_onderdonkii_accessory_ns = read_input_sfs('../Analysis/Alistipes_onderdonkii_55464_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_onderdonkii_all_ns,
  a_onderdonkii_core_ns,
  a_onderdonkii_accessory_ns) + ggtitle('A. onderdonkii nonsynonymous')

a_shahii_all_ns = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
a_shahii_core_ns = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
a_shahii_accessory_ns = read_input_sfs('../Analysis/Alistipes_shahii_62199_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_shahii_all_ns,
  a_shahii_core_ns,
  a_shahii_accessory_ns) + ggtitle('A. shahii nonsynonymous')

o_splanchnicus_all_ns = read_input_sfs('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
o_splanchnicus_core_ns = read_input_sfs('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
o_splanchnicus_accessory_ns = read_input_sfs('../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(o_splanchnicus_all_ns,
  o_splanchnicus_core_ns,
  o_splanchnicus_accessory_ns) + ggtitle('O. splanchnicus nonsynonymous')

p_distasonis_all_ns = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
p_distasonis_core_ns = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
p_distasonis_accessory_ns = read_input_sfs('../Analysis/Parabacteroides_distasonis_56985_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_distasonis_all_ns,
  p_distasonis_core_ns,
  p_distasonis_accessory_ns) + ggtitle('P. distasonis nonsynonymous')

p_merdae_all_ns = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
p_merdae_core_ns = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
p_merdae_accessory_ns = read_input_sfs('../Analysis/Parabacteroides_merdae_56972_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_merdae_all_ns,
  p_merdae_core_ns,
  p_merdae_accessory_ns) + ggtitle('P. merdae nonsynonymous')

p_copri_all_ns = read_input_sfs('../Analysis/Prevotella_copri_61740_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
p_copri_core_ns = read_input_sfs('../Analysis/Prevotella_copri_61740_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
p_copri_accessory_ns = read_input_sfs('../Analysis/Prevotella_copri_61740_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_copri_all_ns,
  p_copri_core_ns,
  p_copri_accessory_ns) + ggtitle('P. copri nonsynonymous')

b_fragilis_all_ns = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_fragilis_core_ns = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_fragilis_accessory_ns = read_input_sfs('../Analysis/Bacteroides_fragilis_54507_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_fragilis_all_ns,
  b_fragilis_core_ns,
  b_fragilis_accessory_ns) + ggtitle('B. fragilis nonsynonymous')

b_cellulosilyticus_all_ns = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_cellulosilyticus_core_ns = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_cellulosilyticus_accessory_ns = read_input_sfs('../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_cellulosilyticus_all_ns,
  b_cellulosilyticus_core_ns,
  b_cellulosilyticus_accessory_ns) + ggtitle('B. cellulosilyticus nonsynonymous')

b_stercoris_all_ns = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_stercoris_core_ns = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_stercoris_accessory_ns = read_input_sfs('../Analysis/Bacteroides_stercoris_56735_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_stercoris_all_ns,
  b_stercoris_core_ns,
  b_stercoris_accessory_ns) + ggtitle('B. stercoris nonsynonymous')

b_thetaiotaomicron_all_ns = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_thetaiotaomicron_core_ns = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_thetaiotaomicron_accessory_ns = read_input_sfs('../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_thetaiotaomicron_all_ns,
  b_thetaiotaomicron_core_ns,
  b_thetaiotaomicron_accessory_ns) + ggtitle('B. thetaiotaomicron nonsynonymous')

b_xylanisolvens_all_ns = read_input_sfs('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_xylanisolvens_core_ns = read_input_sfs('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_xylanisolvens_accessory_ns = read_input_sfs('../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_xylanisolvens_all_ns,
  b_xylanisolvens_core_ns,
  b_xylanisolvens_accessory_ns) + ggtitle('B. xylanisolvens nonsynonymous')

b_vulgatus_all_ns = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_vulgatus_core_ns = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_vulgatus_accessory_ns = read_input_sfs('../Analysis/Bacteroides_vulgatus_57955_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_vulgatus_all_ns,
  b_vulgatus_core_ns,
  b_vulgatus_accessory_ns) + ggtitle('B. vulgatus nonsynonymous')

b_intestinihominis_all_ns = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
b_intestinihominis_core_ns = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
b_intestinihominis_accessory_ns = read_input_sfs('../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(b_intestinihominis_all_ns,
  b_intestinihominis_core_ns,
  b_intestinihominis_accessory_ns) + ggtitle('B. intestinihominis nonsynonymous')

a_muciniphila_all_ns = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
a_muciniphila_core_ns = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
a_muciniphila_accessory_ns = read_input_sfs('../Analysis/Akkermansia_muciniphila_55290_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(a_muciniphila_all_ns,
  a_muciniphila_core_ns,
  a_muciniphila_accessory_ns)  + ggtitle('A. muciniphila nonsynonymous')

p_sp_all_ns = read_input_sfs('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
p_sp_core_ns = read_input_sfs('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
p_sp_accessory_ns = read_input_sfs('../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(p_sp_all_ns,
  p_sp_core_ns,
  p_sp_accessory_ns) + ggtitle('Phascolarctobacterium species nonsynonymous')

e_eligens_all_ns = read_input_sfs('../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
e_eligens_core_ns = read_input_sfs('../Analysis/Eubacterium_eligens_61678_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
e_eligens_accessory_ns = read_input_sfs('../Analysis/Eubacterium_eligens_61678_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(e_eligens_all_ns,
  e_eligens_core_ns,
  e_eligens_accessory_ns) + ggtitle('E. eligens nonsynonymous')

e_rectale_all_ns = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
e_rectale_core_ns = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
e_rectale_accessory_ns = read_input_sfs('../Analysis/Eubacterium_rectale_56927_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(e_rectale_all_ns,
  e_rectale_core_ns,
  e_rectale_accessory_ns) + ggtitle('E. rectale nonsynonymous')

o_sp_all_ns = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
o_sp_core_ns = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
o_sp_accessory_ns = read_input_sfs('../Analysis/Oscillibacter_sp_60799_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(o_sp_all_ns,
  o_sp_core_ns,
  o_sp_accessory_ns) + ggtitle('Oscillibacter species nonsynonymous')

r_bromii_all_ns = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
r_bromii_core_ns = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
r_bromii_accessory_ns = read_input_sfs('../Analysis/Ruminococcus_bromii_62047_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(r_bromii_all_ns,
  r_bromii_core_ns,
  r_bromii_accessory_ns) + ggtitle('Ruminococcus bromii nonsynonymous')

r_bicirculans_all_ns = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/empirical_nonsyn_downsampled_sfs.txt') 
r_bicirculans_core_ns = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt') 
r_bicirculans_accessory_ns = read_input_sfs('../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt') 

compare_core_accessory_sfs(r_bicirculans_all_ns,
  r_bicirculans_core_ns,
  r_bicirculans_accessory_ns) + ggtitle('Ruminococcus bicirculans nonsynonymous')

f_prausnitzii_all_ns = read_input_sfs('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
f_prausnitzii_core_ns = read_input_sfs('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
f_prausnitzii_accessory_ns = read_input_sfs('../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs(f_prausnitzii_all_ns,
  f_prausnitzii_core_ns,
  f_prausnitzii_accessory_ns) + ggtitle('F. prausnitzii nonsynonymous')

## Raw Count

compare_core_accessory_sfs_count(b_bacterium_all,
  b_bacterium_core,
  b_bacterium_accessory) + ggtitle('B. bacterium synonymous')

compare_core_accessory_sfs_count(a_finegoldii_all,
  a_finegoldii_core,
  a_finegoldii_accessory) + ggtitle('A. finegoldii synonymous')

compare_core_accessory_sfs_count(a_onderdonkii_all,
  a_onderdonkii_core,
  a_onderdonkii_accessory) + ggtitle('A. onderdonkii synonymous')

compare_core_accessory_sfs_count(a_shahii_all,
  a_shahii_core,
  a_shahii_accessory) + ggtitle('A. shahii synonymous')

compare_core_accessory_sfs_count(o_splanchnicus_all,
  o_splanchnicus_core,
  o_splanchnicus_accessory) + ggtitle('O. splanchnicus synonymous')

compare_core_accessory_sfs_count(p_distasonis_all,
  p_distasonis_core,
  p_distasonis_accessory) + ggtitle('P. distasonis synonymous')

compare_core_accessory_sfs_count(p_merdae_all,
  p_merdae_core,
  p_merdae_accessory) + ggtitle('P. merdae synonymous')

compare_core_accessory_sfs_count(p_copri_all,
  p_copri_core,
  p_copri_accessory) + ggtitle('P. copri synonymous')

compare_core_accessory_sfs_count(b_fragilis_all,
  b_fragilis_core,
  b_fragilis_accessory) + ggtitle('B. fragilis synonymous')

compare_core_accessory_sfs_count(b_cellulosilyticus_all,
  b_cellulosilyticus_core,
  b_cellulosilyticus_accessory) + ggtitle('B. cellulosilyticus synonymous')

compare_core_accessory_sfs_count(b_stercoris_all,
  b_stercoris_core,
  b_stercoris_accessory) + ggtitle('B. stercoris synonymous')

compare_core_accessory_sfs_count(b_thetaiotaomicron_all,
  b_thetaiotaomicron_core,
  b_thetaiotaomicron_accessory) + ggtitle('B. thetaiotaomicron synonymous')

compare_core_accessory_sfs_count(b_xylanisolvens_all,
  b_xylanisolvens_core,
  b_xylanisolvens_accessory) + ggtitle('B. xylanisolvens synonymous')

compare_core_accessory_sfs_count(b_vulgatus_all,
  b_vulgatus_core,
  b_vulgatus_accessory) + ggtitle('B. vulgatus synonymous')

compare_core_accessory_sfs_count(b_intestinihominis_all,
  b_intestinihominis_core,
  b_intestinihominis_accessory) + ggtitle('B. intestinihominis')

compare_core_accessory_sfs_count(a_muciniphila_all,
  a_muciniphila_core,
  a_muciniphila_accessory)  + ggtitle('A. muciniphila synonymous')

compare_core_accessory_sfs_count(p_sp_all,
  p_sp_core,
  p_sp_accessory) + ggtitle('Phascolarctobacterium species synonymous')

compare_core_accessory_sfs_count(e_eligens_all,
  e_eligens_core,
  e_eligens_accessory) + ggtitle('E. eligens synonymous')

compare_core_accessory_sfs_count(e_rectale_all,
  e_rectale_core,
  e_rectale_accessory) + ggtitle('E. rectale synonymous')

compare_core_accessory_sfs_count(o_sp_all,
  o_sp_core,
  o_sp_accessory) + ggtitle('Oscillibacter species synonymous')

compare_core_accessory_sfs_count(r_bromii_all,
  r_bromii_core,
  r_bromii_accessory) + ggtitle('Ruminococcus bromii synonymous')

compare_core_accessory_sfs_count(r_bicirculans_all,
  r_bicirculans_core,
  r_bicirculans_accessory) + ggtitle('Ruminococcus bicirculans synonymous')

compare_core_accessory_sfs_count(f_prausnitzii_all,
  f_prausnitzii_core,
  f_prausnitzii_accessory) + ggtitle('F. prausnitzii synonymous')

# Nonsyn

compare_core_accessory_sfs_count(b_bacterium_all_ns,
  b_bacterium_core_ns,
  b_bacterium_accessory_ns) + ggtitle('B. bacterium nonsynonymous')

compare_core_accessory_sfs_count(a_finegoldii_all_ns,
  a_finegoldii_core_ns,
  a_finegoldii_accessory_ns) + ggtitle('A. finegoldii nonsynonymous')

compare_core_accessory_sfs_count(a_onderdonkii_all_ns,
  a_onderdonkii_core_ns,
  a_onderdonkii_accessory_ns) + ggtitle('A. onderdonkii nonsynonymous')

compare_core_accessory_sfs_count(a_shahii_all_ns,
  a_shahii_core_ns,
  a_shahii_accessory_ns) + ggtitle('A. shahii nonsynonymous')

compare_core_accessory_sfs_count(o_splanchnicus_all_ns,
  o_splanchnicus_core_ns,
  o_splanchnicus_accessory_ns) + ggtitle('O. splanchnicus nonsynonymous')

compare_core_accessory_sfs_count(p_distasonis_all_ns,
  p_distasonis_core_ns,
  p_distasonis_accessory_ns) + ggtitle('P. distasonis nonsynonymous')

compare_core_accessory_sfs_count(p_merdae_all_ns,
  p_merdae_core_ns,
  p_merdae_accessory_ns) + ggtitle('P. merdae nonsynonymous')

compare_core_accessory_sfs_count(p_copri_all_ns,
  p_copri_core_ns,
  p_copri_accessory_ns) + ggtitle('P. copri nonsynonymous')

compare_core_accessory_sfs_count(b_fragilis_all_ns,
  b_fragilis_core_ns,
  b_fragilis_accessory_ns) + ggtitle('B. fragilis nonsynonymous')

compare_core_accessory_sfs_count(b_cellulosilyticus_all_ns,
  b_cellulosilyticus_core_ns,
  b_cellulosilyticus_accessory_ns) + ggtitle('B. cellulosilyticus nonsynonymous')

compare_core_accessory_sfs_count(b_stercoris_all_ns,
  b_stercoris_core_ns,
  b_stercoris_accessory_ns) + ggtitle('B. stercoris nonsynonymous')

compare_core_accessory_sfs_count(b_thetaiotaomicron_all_ns,
  b_thetaiotaomicron_core_ns,
  b_thetaiotaomicron_accessory_ns) + ggtitle('B. thetaiotaomicron nonsynonymous')

compare_core_accessory_sfs_count(b_xylanisolvens_all_ns,
  b_xylanisolvens_core_ns,
  b_xylanisolvens_accessory_ns) + ggtitle('B. xylanisolvens nonsynonymous')

compare_core_accessory_sfs_count(b_vulgatus_all_ns,
  b_vulgatus_core_ns,
  b_vulgatus_accessory_ns) + ggtitle('B. vulgatus nonsynonymous')

compare_core_accessory_sfs_count(b_intestinihominis_all_ns,
  b_intestinihominis_core_ns,
  b_intestinihominis_accessory_ns) + ggtitle('B. intestinihominis nonsynonymous')

compare_core_accessory_sfs_count(a_muciniphila_all_ns,
  a_muciniphila_core_ns,
  a_muciniphila_accessory_ns)  + ggtitle('A. muciniphila nonsynonymous')

compare_core_accessory_sfs_count(p_sp_all_ns,
  p_sp_core_ns,
  p_sp_accessory_ns) + ggtitle('Phascolarctobacterium species nonsynonymous')

compare_core_accessory_sfs_count(e_eligens_all_ns,
  e_eligens_core_ns,
  e_eligens_accessory_ns) + ggtitle('E. eligens nonsynonymous')

compare_core_accessory_sfs_count(e_rectale_all_ns,
  e_rectale_core_ns,
  e_rectale_accessory_ns) + ggtitle('E. rectale nonsynonymous')

compare_core_accessory_sfs_count(o_sp_all_ns,
  o_sp_core_ns,
  o_sp_accessory_ns) + ggtitle('Oscillibacter species nonsynonymous')

compare_core_accessory_sfs_count(r_bromii_all_ns,
  r_bromii_core_ns,
  r_bromii_accessory_ns) + ggtitle('Ruminococcus bromii nonsynonymous')

compare_core_accessory_sfs_count(r_bicirculans_all_ns,
  r_bicirculans_core_ns,
  r_bicirculans_accessory_ns) + ggtitle('Ruminococcus bicirculans nonsynonymous')

compare_core_accessory_sfs_count(f_prausnitzii_all_ns,
  f_prausnitzii_core_ns,
  f_prausnitzii_accessory_ns) + ggtitle('F. prausnitzii nonsynonymous')

# syn vs. nonsyn

a_putredinis_all = read_input_sfs('../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_syn_downsampled_sfs.txt')
a_putredinis_core = read_input_sfs('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
a_putredinis_accessory = read_input_sfs('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

a_putredinis_all_ns = read_input_sfs('../Analysis/Alistipes_putredinis_61533_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
a_putredinis_core_ns = read_input_sfs('../Analysis/Alistipes_putredinis_61533_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
a_putredinis_accessory_ns = read_input_sfs('../Analysis/Alistipes_putredinis_61533_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

b_uniformis_all = read_input_sfs('../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_syn_downsampled_sfs.txt')
b_uniformis_core = read_input_sfs('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_uniformis_accessory = read_input_sfs('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

b_uniformis_all_ns = read_input_sfs('../Analysis/Bacteroides_uniformis_57318_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
b_uniformis_core_ns = read_input_sfs('../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
b_uniformis_accessory_ns = read_input_sfs('../Analysis/Bacteroides_uniformis_57318_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

b_ovatus_all = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_syn_downsampled_sfs.txt')
b_ovatus_core = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_ovatus_accessory = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

b_ovatus_all_ns = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
b_ovatus_core_ns = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
b_ovatus_accessory_ns = read_input_sfs('../Analysis/Bacteroides_ovatus_58035_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

b_caccae_all = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_syn_downsampled_sfs.txt')
b_caccae_core = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
b_caccae_accessory = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

b_caccae_all_ns = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
b_caccae_core_ns = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
b_caccae_accessory_ns = read_input_sfs('../Analysis/Bacteroides_caccae_53434_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

d_invisus_all = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/empirical_syn_downsampled_sfs.txt')
d_invisus_core = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_syn_downsampled_sfs.txt')
d_invisus_accessory = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_empirical_syn_downsampled_sfs.txt')

d_invisus_all_ns = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/empirical_nonsyn_downsampled_sfs.txt')
d_invisus_core_ns = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_nonsyn_downsampled_sfs.txt')
d_invisus_accessory_ns = read_input_sfs('../Analysis/Dialister_invisus_61905_downsampled_14/accessory_empirical_nonsyn_downsampled_sfs.txt')

compare_core_accessory_sfs_syn_ns(b_bacterium_core,
  b_bacterium_core_ns,
  b_bacterium_accessory,
  b_bacterium_accessory_ns) + ggtitle('B. bacterium')

compare_core_accessory_sfs_syn_ns(a_putredinis_core,
  a_putredinis_core_ns,
  a_putredinis_accessory,
  a_putredinis_accessory_ns) + ggtitle('A. putredinis')

compare_core_accessory_sfs_syn_ns(a_finegoldii_core,
  a_finegoldii_core_ns,
  a_finegoldii_accessory,
  a_finegoldii_accessory_ns) + ggtitle('A. finegoldii')

compare_core_accessory_sfs_syn_ns(a_onderdonkii_core,
  a_onderdonkii_core_ns,
  a_onderdonkii_accessory,
  a_onderdonkii_accessory_ns) + ggtitle('A. onderdonkii')

compare_core_accessory_sfs_syn_ns(a_shahii_core,
  a_shahii_core_ns,
  a_shahii_accessory,
  a_shahii_accessory_ns) + ggtitle('A. shahii')

compare_core_accessory_sfs_syn_ns(o_splanchnicus_core,
  o_splanchnicus_core_ns,
  o_splanchnicus_accessory,
  o_splanchnicus_accessory_ns) + ggtitle('O. splanchnicus')

compare_core_accessory_sfs_syn_ns(p_distasonis_core,
  p_distasonis_core_ns,
  p_distasonis_accessory,
  p_distasonis_accessory_ns) + ggtitle('P. distasonis')

compare_core_accessory_sfs_syn_ns(p_merdae_core,
  p_merdae_core_ns,
  p_merdae_accessory,
  p_merdae_accessory_ns) + ggtitle('P. merdae')

compare_core_accessory_sfs_syn_ns(p_copri_core,
  p_copri_core_ns,
  p_copri_accessory,
  p_copri_accessory_ns) + ggtitle('P. copri')

compare_core_accessory_sfs_syn_ns(b_fragilis_core,
  b_fragilis_core_ns,
  b_fragilis_accessory,
  b_fragilis_accessory_ns) + ggtitle('B. fragilis')

compare_core_accessory_sfs_syn_ns(b_cellulosilyticus_core,
  b_cellulosilyticus_core_ns,
  b_cellulosilyticus_accessory,
  b_cellulosilyticus_accessory_ns) + ggtitle('B. cellulosilyticus')

compare_core_accessory_sfs_syn_ns(b_stercoris_core,
  b_stercoris_core_ns,
  b_stercoris_accessory,
  b_stercoris_accessory_ns) + ggtitle('B. stercoris')

compare_core_accessory_sfs_syn_ns(b_uniformis_core,
  b_uniformis_core_ns,
  b_vulgatus_accessory,
  b_vulgatus_accessory_ns) +  ggtitle('B. uniformis')

compare_core_accessory_sfs_syn_ns(b_thetaiotaomicron_core,
  b_thetaiotaomicron_core_ns,
  b_thetaiotaomicron_accessory,
  b_thetaiotaomicron_accessory_ns) + ggtitle('B. thetaiotaomicron')

compare_core_accessory_sfs_syn_ns(b_ovatus_core,
  b_ovatus_core_ns,
  b_ovatus_accessory,
  b_ovatus_accessory_ns) + ggtitle('B. ovatus')

compare_core_accessory_sfs_syn_ns(b_xylanisolvens_core,
  b_xylanisolvens_core_ns,
  b_xylanisolvens_accessory,
  b_xylanisolvens_accessory_ns) + ggtitle('B. xylanisolvens')

compare_core_accessory_sfs_syn_ns(b_caccae_core,
  b_caccae_core_ns,
  b_caccae_accessory,
  b_caccae_accessory_ns) + ggtitle('B. caccae')

compare_core_accessory_sfs_syn_ns(b_vulgatus_core,
  b_vulgatus_core_ns,
  b_vulgatus_accessory,
  b_vulgatus_accessory_ns) + ggtitle('B. vulgatus')

compare_core_accessory_sfs_syn_ns(b_intestinihominis_core,
  b_intestinihominis_core_ns,
  b_intestinihominis_accessory,
  b_intestinihominis_accessory_ns) + ggtitle('B. intestinihominis')

compare_core_accessory_sfs_syn_ns(a_muciniphila_core,
  a_muciniphila_core_ns,
  a_muciniphila_accessory,
  a_muciniphila_accessory_ns)  + ggtitle('A. muciniphila')

compare_core_accessory_sfs_syn_ns(d_invisus_core,
  d_invisus_core_ns,
  d_invisus_accessory,
  d_invisus_accessory_ns) + ggtitle('D. invisus')

compare_core_accessory_sfs_syn_ns(p_sp_core,
  p_sp_core_ns,
  p_sp_accessory,
  p_sp_accessory_ns) + ggtitle('Phascolarctobacterium species')

compare_core_accessory_sfs_syn_ns(e_eligens_core,
  e_eligens_core_ns,
  e_eligens_accessory,
  e_eligens_accessory_ns) + ggtitle('E. eligens')

compare_core_accessory_sfs_syn_ns(e_rectale_core,
  e_rectale_core_ns,
  e_rectale_accessory,
  e_rectale_accessory_ns) + ggtitle('E. rectale')

compare_core_accessory_sfs_syn_ns(o_sp_core,
  o_sp_core_ns,
  o_sp_accessory,
  o_sp_accessory_ns) + ggtitle('Oscillibacter species')

compare_core_accessory_sfs_syn_ns(r_bromii_core,
  r_bromii_core_ns,
  r_bromii_accessory,
  r_bromii_accessory_ns) + ggtitle('Ruminococcus bromii')

compare_core_accessory_sfs_syn_ns(r_bicirculans_core,
  r_bicirculans_core_ns,
  r_bicirculans_accessory,
  r_bicirculans_accessory_ns) + ggtitle('Ruminococcus bicirculans')

compare_core_accessory_sfs_syn_ns(f_prausnitzii_core,
  f_prausnitzii_core_ns,
  f_prausnitzii_accessory,
  f_prausnitzii_accessory_ns) + ggtitle('F. prausnitzii')

one_epoch_file_list = c(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Alistipes_shahii_62199_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Dialister_invisus_61905_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Prevotella_copri_61740_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_one_epoch_demography.txt',
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_one_epoch_demography.txt'
)

two_epoch_file_list = c(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Alistipes_shahii_62199_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Dialister_invisus_61905_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Prevotella_copri_61740_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_two_epoch_demography.txt',
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_two_epoch_demography.txt'
)

three_epoch_file_list = c(
  '../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Alistipes_putredinis_61533_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Alistipes_shahii_62199_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_caccae_53434_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_ovatus_58035_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Dialister_invisus_61905_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Eubacterium_rectale_56927_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Oscillibacter_sp_60799_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Prevotella_copri_61740_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_three_epoch_demography.txt',
  '../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_three_epoch_demography.txt'
)

species_list = c(
  'Akkermansia_muciniphila_55290',
  'Alistipes_finegoldii_56071',
  'Alistipes_onderdonkii_55464',
  'Alistipes_putredinis_61533',
  'Alistipes_shahii_62199',
  'Bacteroidales_bacterium_58650',
  'Bacteroides_caccae_53434',
  'Bacteroides_cellulosilyticus_58046',
  'Bacteroides_fragilis_54507',
  'Bacteroides_ovatus_58035',
  'Bacteroides_stercoris_56735',
  'Bacteroides_thetaiotaomicron_56941',
  'Bacteroides_uniformis_57318',
  'Bacteroides_vulgatus_57955',
  'Bacteroides_xylanisolvens_57185',
  'Barnesiella_intestinihominis_62208',
  'Dialister_invisus_61905',
  'Eubacterium_eligens_61678',
  'Eubacterium_rectale_56927',
  'Faecalibacterium_prausnitzii_57453',
  'Odoribacter_splanchnicus_62174',
  'Oscillibacter_sp_60799',
  'Parabacteroides_distasonis_56985',
  'Parabacteroides_merdae_56972',
  'Phascolarctobacterium_sp_59817',
  'Prevotella_copri_61740',
  'Ruminococcus_bicirculans_59300',
  'Ruminococcus_bromii_62047'
)

AIC_matrix = data.frame(
  one_epoch = numeric(28),
  two_epoch = numeric(28), 
  three_epoch = numeric(28), 
  delta_AIC = numeric(28))
row.names(AIC_matrix) = species_list


for (i in 1:length(species_list)) {
  AIC_matrix[i, 1] = AIC_from_demography(one_epoch_file_list[i])
  AIC_matrix[i, 2] = AIC_from_demography(two_epoch_file_list[i])
  AIC_matrix[i, 3] = AIC_from_demography(three_epoch_file_list[i])
  AIC_matrix[i, 4] = abs(min(AIC_matrix[i, 1], AIC_matrix[i, 3]) - AIC_matrix[i, 2])
}

AIC_matrix

write.csv(AIC_matrix,  file='../Summary/AIC_matrix.csv')
