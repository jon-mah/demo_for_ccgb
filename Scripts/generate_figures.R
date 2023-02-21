library(ggplot2)
library(ggrepel)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(fitdistrplus)
library(scales)
library(reshape2)

compute_pi = function(input) {
  return(input * runif(1, min=0.5, max=0.65))
}

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
  # Reads input SFS in Dadi Format
  this_file = file(input_file)
  sfs_string = readLines(this_file)[2]
  output_sfs = as.numeric(unlist(strsplit(sfs_string, ' ')))
  output_sfs = output_sfs[-1] # Remove 0-tons
  output_sfs = fold_sfs(output_sfs)
  return(output_sfs)
}

sfs_from_demography = function(input_file) {
  # Reads input SFS from output *demography.txt
  this_file = file(input_file)
  sfs_string = readLines(this_file)[7]
  output_sfs = strsplit(sfs_string, '-- ')
  output_sfs = unlist(output_sfs)[2]
  output_sfs = unlist(strsplit(output_sfs, ' '))
  output_sfs = output_sfs[-length(output_sfs)]
  # output_sfs = output_sfs[-1]
  output_sfs = as.numeric(output_sfs)
  output_sfs = fold_sfs(output_sfs)
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
  
  species_surface_scatter = ggplot(data=species_surface, aes(x=nu, y=tau)) + 
    geom_point(aes(color=likelihood)) +
    scale_fill_brewer(palette = "Accent") +
    scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') +
    geom_vline(xintercept=1.0, color='red') +
    theme_bw()
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
    # scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))
  
  return(p_input_comparison)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# A. finegoldii
plot_likelihood_surface('../Analysis/qp_gut_14/a_finegoldii_14.csv')
a_finegoldii_14_empirical =  read_input_sfs(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/empirical_sfs.txt'
)
a_finegoldii_14_one_epoch = sfs_from_demography(
  '../Analysis/Alistipes_finegoldii_56071_downsampled_14/one_epoch_demography.txt'
)
a_finegoldii_14_two_epoch = sfs_from_demography(
  '..Analysis/Alistipes_finegoldii_56071_downsampled_14/two_epoch_demography.txt'
)

# A. muciniphila
plot_likelihood_surface('../Analysis/qp_gut_14/a_muciniphila_14.csv')
a_muciniphila_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
a_muciniphila_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
a_muciniphila_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_muciniphila_14_empirical,
            a_muciniphila_14_one_epoch,
            a_muciniphila_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(a_muciniphila_14_empirical),
            proportional_sfs(a_muciniphila_14_one_epoch),
            proportional_sfs(a_muciniphila_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# A. onderdonkii
plot_likelihood_surface('../Analysis/qp_gut_14/a_onderdonkii_14.csv')
a_onderdonkii_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
a_onderdonkii_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
a_onderdonkii_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_onderdonkii_14_empirical,
            a_onderdonkii_14_one_epoch,
            a_onderdonkii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(a_onderdonkii_14_empirical),
            proportional_sfs(a_onderdonkii_14_one_epoch),
            proportional_sfs(a_onderdonkii_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# A. putredinis
plot_likelihood_surface('../Analysis/qp_gut_14/a_putredinis_14.csv')
a_putredinis_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
a_putredinis_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
a_putredinis_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_putredinis_14_empirical,
            a_putredinis_14_one_epoch,
            a_putredinis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(a_putredinis_14_empirical),
            proportional_sfs(a_putredinis_14_one_epoch),
            proportional_sfs(a_putredinis_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')



# A. shahii
plot_likelihood_surface('../Analysis/qp_gut_14/a_shahii_14.csv')
a_shahii_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
a_shahii_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
a_shahii_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(a_shahii_14_empirical,
            a_shahii_14_one_epoch,
            a_shahii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(a_shahii_14_empirical),
            proportional_sfs(a_shahii_14_one_epoch),
            proportional_sfs(a_shahii_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# B. bacterium
plot_likelihood_surface('../Analysis/qp_gut_14/b_bacterium_14.csv')
b_bacterium_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_bacterium_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_bacterium_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_bacterium_14_empirical,
            b_bacterium_14_one_epoch,
            b_bacterium_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_bacterium_14_empirical),
            proportional_sfs(b_bacterium_14_one_epoch),
            proportional_sfs(b_bacterium_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')

# B. caccae
plot_likelihood_surface('../Analysis/qp_gut_14/b_caccae_14.csv')
b_caccae_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_caccae_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_caccae_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_caccae_14_empirical,
            b_caccae_14_one_epoch,
            b_caccae_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_caccae_14_empirical),
            proportional_sfs(b_caccae_14_one_epoch),
            proportional_sfs(b_caccae_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')

# B. cellulosilyticus
plot_likelihood_surface('../Analysis/qp_gut_14/b_cellulosilyticus_14.csv')
b_cellulosilyticus_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_cellulosilyticus_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_cellulosilyticus_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_cellulosilyticus_14_empirical,
            b_cellulosilyticus_14_one_epoch,
            b_cellulosilyticus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_cellulosilyticus_14_empirical),
            proportional_sfs(b_cellulosilyticus_14_one_epoch),
            proportional_sfs(b_cellulosilyticus_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


#  B. fragilis
plot_likelihood_surface('../Analysis/qp_gut_14/b_fragilis_14.csv')
b_fragilis_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_fragilis_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_fragilis_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_fragilis_14_empirical,
            b_fragilis_14_one_epoch,
            b_fragilis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_fragilis_14_empirical),
            proportional_sfs(b_fragilis_14_one_epoch),
            proportional_sfs(b_fragilis_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# B. intestinihominis
plot_likelihood_surface('../Analysis/qp_gut_14/b_intestinihominis_14.csv')
b_intestinihominis_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_intestinihominis_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_intestinihominis_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_intestinihominis_14_empirical,
            b_intestinihominis_14_one_epoch,
            b_intestinihominis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_intestinihominis_14_empirical),
            proportional_sfs(b_intestinihominis_14_one_epoch),
            proportional_sfs(b_intestinihominis_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')



# B. ovatus
plot_likelihood_surface('../Analysis/qp_gut_14/b_ovatus_14.csv')
b_ovatus_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_ovatus_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_ovatus_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_ovatus_14_empirical,
            b_ovatus_14_one_epoch,
            b_ovatus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_ovatus_14_empirical),
            proportional_sfs(b_ovatus_14_one_epoch),
            proportional_sfs(b_ovatus_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# B. thetaiotaomicron
plot_likelihood_surface('../Analysis/qp_gut_14/b_thetaiotaomicron_14.csv')
b_thetaiotaomicron_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_thetaiotaomicron_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_thetaiotaomicron_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_thetaiotaomicron_14_empirical,
            b_thetaiotaomicron_14_one_epoch,
            b_thetaiotaomicron_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_thetaiotaomicron_14_empirical),
            proportional_sfs(b_thetaiotaomicron_14_one_epoch),
            proportional_sfs(b_thetaiotaomicron_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# B. uniformis
plot_likelihood_surface('../Analysis/qp_gut_14/b_uniformis_14.csv')
b_uniformis_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_uniformis_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_uniformis_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_uniformis_14_empirical,
            b_uniformis_14_one_epoch,
            b_uniformis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_uniformis_14_empirical),
            proportional_sfs(b_uniformis_14_one_epoch),
            proportional_sfs(b_uniformis_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# B. vulgatus
plot_likelihood_surface('../Analysis/qp_gut_14/b_vulgatus_14.csv')
b_vulgatus_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_vulgatus_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_vulgatus_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_vulgatus_14_empirical,
            b_vulgatus_14_one_epoch,
            b_vulgatus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_vulgatus_14_empirical),
            proportional_sfs(b_vulgatus_14_one_epoch),
            proportional_sfs(b_vulgatus_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')



# B. xylanisolvens
plot_likelihood_surface('../Analysis/qp_gut_14/b_xylanisolvens_14.csv')
b_xylanisolvens_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
b_xylanisolvens_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
b_xylanisolvens_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(b_xylanisolvens_14_empirical,
            b_xylanisolvens_14_one_epoch,
            b_xylanisolvens_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(b_xylanisolvens_14_empirical),
            proportional_sfs(b_xylanisolvens_14_one_epoch),
            proportional_sfs(b_xylanisolvens_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')



# D. invisus
plot_likelihood_surface('../Analysis/qp_gut_14/d_invisus_14.csv')
d_invisus_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
d_invisus_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
d_invisus_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(d_invisus_14_empirical,
            d_invisus_14_one_epoch,
            d_invisus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(d_invisus_14_empirical),
            proportional_sfs(d_invisus_14_one_epoch),
            proportional_sfs(d_invisus_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# E. eligens
plot_likelihood_surface('../Analysis/qp_gut_14/e_eligens_14.csv')
e_eligens_14_empirical =  read_input_sfs(
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
e_rectale_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
e_rectale_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
e_rectale_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(e_rectale_14_empirical,
            e_rectale_14_one_epoch,
            e_rectale_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(e_rectale_14_empirical),
            proportional_sfs(e_rectale_14_one_epoch),
            proportional_sfs(e_rectale_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# F. prausnitzii
plot_likelihood_surface('../Analysis/qp_gut_14/f_prausnitzii_14.csv')
f_prausnitzii_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
f_prausnitzii_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
f_prausnitzii_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(f_prausnitzii_14_empirical,
            f_prausnitzii_14_one_epoch,
            f_prausnitzii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(f_prausnitzii_14_empirical),
            proportional_sfs(f_prausnitzii_14_one_epoch),
            proportional_sfs(f_prausnitzii_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# Oscillibacter sp.
plot_likelihood_surface('../Analysis/qp_gut_14/o_sp_14.csv')
o_sp_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
o_sp_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
o_sp_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(o_sp_14_empirical,
            o_sp_14_one_epoch,
            o_sp_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(o_sp_14_empirical),
            proportional_sfs(o_sp_14_one_epoch),
            proportional_sfs(o_sp_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# o. splanchnicus
plot_likelihood_surface('../Analysis/qp_gut_14/o_splanchnicus_14.csv')
o_splanchnicus_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
o_splanchnicus_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
o_splanchnicus_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(o_splanchnicus_14_empirical,
            o_splanchnicus_14_one_epoch,
            o_splanchnicus_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(o_splanchnicus_14_empirical),
            proportional_sfs(o_splanchnicus_14_one_epoch),
            proportional_sfs(o_splanchnicus_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# P. copri
plot_likelihood_surface('../Analysis/qp_gut_14/p_copri_14.csv')
p_copri_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
p_copri_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
p_copri_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_copri_14_empirical,
            p_copri_14_one_epoch,
            p_copri_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(p_copri_14_empirical),
            proportional_sfs(p_copri_14_one_epoch),
            proportional_sfs(p_copri_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# P. distasonis
plot_likelihood_surface('../Analysis/qp_gut_14/p_distasonis_14.csv')
p_distasonis_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
p_distasonis_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
p_distasonis_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_distasonis_14_empirical,
            p_distasonis_14_one_epoch,
            p_distasonis_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(p_distasonis_14_empirical),
            proportional_sfs(p_distasonis_14_one_epoch),
            proportional_sfs(p_distasonis_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# P. merdae
plot_likelihood_surface('../Analysis/qp_gut_14/p_merdae_14.csv')
p_merdae_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
p_merdae_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
p_merdae_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_merdae_14_empirical,
            p_merdae_14_one_epoch,
            p_merdae_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(p_merdae_14_empirical),
            proportional_sfs(p_merdae_14_one_epoch),
            proportional_sfs(p_merdae_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# Phascolarctobacterium sp.
plot_likelihood_surface('../Analysis/qp_gut_14/p_sp_14.csv')
p_sp_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
p_sp_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
p_sp_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(p_sp_14_empirical,
            p_sp_14_one_epoch,
            p_sp_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(p_sp_14_empirical),
            proportional_sfs(p_sp_14_one_epoch),
            proportional_sfs(p_sp_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')


# R. bicirculans
plot_likelihood_surface('../Analysis/qp_gut_14/r_bicirculans_14.csv')
r_bicirculans_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
r_bicirculans_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
r_bicirculans_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(r_bicirculans_14_empirical,
            r_bicirculans_14_one_epoch,
            r_bicirculans_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(r_bicirculans_14_empirical),
            proportional_sfs(r_bicirculans_14_one_epoch),
            proportional_sfs(r_bicirculans_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')

# R. bromii
plot_likelihood_surface('../Analysis/qp_gut_14/r_bromii_14.csv')
r_bromii_14_empirical =  read_input_sfs(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/empirical_sfs.txt'
)
r_bromii_14_one_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/one_epoch_demography.txt'
)
r_bromii_14_two_epoch = sfs_from_demography(
  '../Analysis/Eubacterium_eligens_61678_downsampled_14/two_epoch_demography.txt'
)
compare_sfs(r_bromii_14_empirical,
            r_bromii_14_one_epoch,
            r_bromii_14_two_epoch) +
  ylab('Raw Count of Segregating Sites') +
  ggtitle('E. eligens Downsampled to 14')

compare_sfs(proportional_sfs(r_bromii_14_empirical),
            proportional_sfs(r_bromii_14_one_epoch),
            proportional_sfs(r_bromii_14_two_epoch)) +
  ggtitle('E. eligens Downsampled to 14')

