library(ggplot2)
library(ggrepel)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(fitdistrplus)
library(scales)
library(reshape2)

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
a_finegoldii_14_empirical =  read_input_sfs(
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
a_muciniphila_14_empirical =  read_input_sfs(
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
a_onderdonkii_14_empirical =  read_input_sfs(
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
a_putredinis_14_empirical =  read_input_sfs(
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
a_shahii_14_empirical =  read_input_sfs(
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
b_bacterium_14_empirical =  read_input_sfs(
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
b_caccae_14_empirical =  read_input_sfs(
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
b_cellulosilyticus_14_empirical =  read_input_sfs(
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
b_fragilis_14_empirical =  read_input_sfs(
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
b_intestinihominis_14_empirical =  read_input_sfs(
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
b_ovatus_14_empirical =  read_input_sfs(
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
b_thetaiotaomicron_14_empirical =  read_input_sfs(
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
b_uniformis_14_empirical =  read_input_sfs(
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
b_vulgatus_14_empirical =  read_input_sfs(
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
b_xylanisolvens_14_empirical =  read_input_sfs(
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
d_invisus_14_empirical =  read_input_sfs(
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
f_prausnitzii_14_empirical =  read_input_sfs(
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
o_sp_14_empirical =  read_input_sfs(
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
o_splanchnicus_14_empirical =  read_input_sfs(
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
p_copri_14_empirical =  read_input_sfs(
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
p_distasonis_14_empirical =  read_input_sfs(
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
p_merdae_14_empirical =  read_input_sfs(
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
p_sp_14_empirical =  read_input_sfs(
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
r_bicirculans_14_empirical =  read_input_sfs(
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
r_bromii_14_empirical =  read_input_sfs(
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

plot_original_empirical_sfs(a_muciniphila_original_empirical) + ggtitle('A. muciniphila full empirical SFS (unfolded)')

plot_original_empirical_sfs(fold_sfs(a_muciniphila_original_empirical)) + ggtitle('A. mucinaphila full empirical SFS (folded)')
a_muciniphila_garud_good_empirical = read_input_sfs_original('../Data/Akkermansia_muciniphila_55290_syn.sfs')
plot_original_empirical_sfs(fold_sfs(a_muciniphila_garud_good_empirical)) + ggtitle('A. muciniphila folded SFS (Garud/Good code base)')

temp_x_axis = 0:(length(fold_sfs(a_muciniphila_original_empirical))-1)

compare_sfs(temp_x_axis,
            fold_sfs(a_muciniphila_original_empirical),
            fold_sfs(c(a_muciniphila_garud_good_empirical, 0, 0, 0, 0, 0, 0)))

plot_original_empirical_sfs(a_finegoldii_original_empirical) + ggtitle('A. finegoldii full empirical SFS (unfolded)')
plot_original_empirical_sfs(a_onderdonkii_original_empirical) + ggtitle('A. onderdonkii full empirical SFS (unfolded)')


b_fragilis_1 = read_input_sfs_original('../Data/UHGG_SFS/1__output_sfs.txt')
b_fragilis_2 = read_input_sfs_original('../Data/UHGG_SFS/2__output_sfs.txt')
b_fragilis_3 = read_input_sfs_original('../Data/UHGG_SFS/3__output_sfs.txt')
b_fragilis_4 = read_input_sfs_original('../Data/UHGG_SFS/4__output_sfs.txt')
b_fragilis_5 = read_input_sfs_original('../Data/UHGG_SFS/5__output_sfs.txt')
b_fragilis = b_fragilis_1 + b_fragilis_2 + b_fragilis_3 + b_fragilis_4 + b_fragilis_5

plot_original_empirical_sfs(b_fragilis) + xlim(-1.5, 20) + ggtitle('B. fragilis synonymous SFS (no clade control)')

plot_original_empirical_sfs(b_fragilis) + xlim(-1.5, 50) + ggtitle('B. fragilis synonymous SFS (no clade control)')


plot_original_empirical_sfs(proportional_sfs(b_fragilis)) + ggtitle('B. fragilis synonymous SFS (no clade control)') +
  ylab('Proportion of Segregating Sites')


plot_original_empirical_sfs(b_fragilis_1) + xlim(-0.5, 20) + ggtitle('B. fragilis synonymous SFS (Contig 1)')
plot_original_empirical_sfs(b_fragilis_2) + xlim(-0.5, 20) + ggtitle('B. fragilis synonymous SFS (Contig 2)')
plot_original_empirical_sfs(b_fragilis_3) + xlim(-0.5, 20) + ggtitle('B. fragilis synonymous SFS (Contig 3)')
plot_original_empirical_sfs(b_fragilis_4) + xlim(-0.5, 20) + ggtitle('B. fragilis synonymous SFS (Contig 4)')
plot_original_empirical_sfs(b_fragilis_5) + xlim(-0.5, 20) + ggtitle('B. fragilis synonymous SFS (Contig 5)')







