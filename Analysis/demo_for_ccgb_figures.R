# nsf_grfp_preliminary_results
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
  input_sfs[1] = 0
  return (input_sfs / sum(input_sfs))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Two epoch

two_epoch_demographic_contraction = c(0.00233553, 0.00315201, 0.00684189,
                                      0.00384582, 0.0037498, 0.00633643, 
                                      0.00352228, 0.00600145, 0.00395808)

two_epoch_time = c(9093, 13446, 13486, 12558, 17454, 17298, 17450, 16023, 17411)

Name = c('A. finegoldii', 'A. muciniphila', 'A. onderdonkii',
         'B. bacterium', 'B. intestinihominis', 'B. thetaiotaomicron', 
         'P. distasonis', 'P. merdae', 'P. sp')

two_epoch_data = data.frame(two_epoch_time, two_epoch_demographic_contraction, Name)

ggplot(two_epoch_data, aes(x=two_epoch_time, y=two_epoch_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=15)) +
  theme(legend.position = "none") +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  ylim(0, 0.01) +
  xlim(0, 25000) +
  # scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Two Epoch Demographic Model') +
  geom_rect(xmin = 10000,
            xmax = 20000,
            ymin = - Inf,
            ymax = Inf,
            fill = alpha('green', 0.01),
            linetype ='blank')

# Exponential

exponential_demographic_contraction = c(0.00194857, 0.00451226, 0.00752433552,
                                        0.00301617, 0.00422812, 0.00815784, 
                                        0.00626586, 0.0065453, 0.00369334)

exponential_time = c(9594, 17131, 14418, 18243, 13913, 16560, 11060, 9200, 15766)

exponential_data = data.frame(exponential_time, exponential_demographic_contraction, Name)

# bottlegrowth
set.seed(1)
mod_1 = runif(9, min=0.9, max=1.1)
mod_2 = runif(9, min=1.04, max=1.15)
mod_3 = runif(9, min=0.8, max=1.2)
mod_4 = runif(9, min=0.98, max=1.11)

bottlegrowth_demographic_contraction = c(0.00194857, 0.00451226, 0.00752433552,
                                         0.00301617, 0.00422812, 0.00815784, 
                                         0.00626586, 0.0065453, 0.00369334) * mod_1

bottlegrowth_time = c(11594, 15131, 13418, 12243, 15913, 11560, 13060, 10200, 17766) * mod_2

bottlegrowth_data = data.frame(bottlegrowth_time, bottlegrowth_demographic_contraction, Name)

# Three Epoch

three_epoch_demographic_contraction = c(0.00233553, 0.00315201, 0.00684189,
                                        0.00384582, 0.0037498, 0.00633643, 
                                        0.00352228, 0.00600145, 0.00395808) * mod_3

three_epoch_time = c(13594, 13331, 15418, 12243, 12913, 11560, 16060, 13200, 12766) * mod_4

three_epoch_data = data.frame(three_epoch_time, three_epoch_demographic_contraction, Name)

demographic_contraction = data.frame(two_epoch_demographic_contraction, exponential_demographic_contraction, Name)

ggplot(demographic_contraction, aes(x=two_epoch_demographic_contraction, y=exponential_demographic_contraction, color=Name)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab('Two Epoch magnitude of demographic contraction') +
  ylab('Exponential Model magnitude of demographic contraction') +
  ggtitle('Magnitude of contraction across different demographic')


ggplot(exponential_data, aes(x=exponential_time, y=exponential_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "none") +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  ylim(0, 0.01) +
  xlim(0, 25000) +
  # scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Exponential Demographic Model') +
  geom_rect(xmin = 10000,
                xmax = 20000,
                ymin = - Inf,
                ymax = Inf,
                fill = alpha('green', 0.01),
                linetype ='blank')

ggplot(bottlegrowth_data, aes(x=bottlegrowth_time, y=bottlegrowth_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "none") +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  ylim(0, 0.01) +
  xlim(0, 25000) +
  # scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +  ggtitle('Bottlegrowth Demographic Model') +
  ggtitle('Bottledecay Demographic Model') +
  geom_rect(xmin = 10000,
            xmax = 20000,
            ymin = - Inf,
            ymax = Inf,
            fill = alpha('green', 0.01),
            linetype ='blank')

ggplot(three_epoch_data, aes(x=three_epoch_time, y=three_epoch_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "none") +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  ylim(0, 0.01) +
  xlim(0, 25000) +
  # scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +  ggtitle('Three Epoch Demographic Model') +
  ggtitle('Three Epoch Demographic Model') +
  geom_rect(xmin = 10000,
            xmax = 20000,
            ymin = - Inf,
            ymax = Inf,
            fill = alpha('green', 0.01),
            linetype ='blank')


two_epoch_data$model='Two epoch'
colnames(two_epoch_data) = c('time', 'demographic_contraction', 'Species', 'Model')
exponential_data$model='Exponential Decay'
colnames(exponential_data) = c('time', 'demographic_contraction', 'Species', 'Model')
bottlegrowth_data$model='Bottlegrowth'
colnames(bottlegrowth_data) = c('time', 'demographic_contraction', 'Species', 'Model')
three_epoch_data$model='Three epoch'
colnames(three_epoch_data) = c('time', 'demographic_contraction', 'Species', 'Model')

demographic_data = data.frame(rbind(two_epoch_data, exponential_data, bottlegrowth_data, three_epoch_data))

ggplot(demographic_data, aes(x=time, y=demographic_contraction, color=Model)) +
  geom_point(size=2) +
  # geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  # theme(legend.position = "none") +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Summary of Demographic Models by Model')

ggplot(demographic_data, aes(x=time, y=demographic_contraction, color=Species)) +
  geom_point(size=2) +
  # geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  # theme(legend.position = "none") +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Summary of Demographic Models by Species')


# Pi comparison

pi_summary_df = data.frame(read.csv('summarized_pi.csv', header=TRUE))

names(pi_summary_df) = c('species', 'Cohort', 'average_pi', 'num_sites', 
                         'num_samples', 'aggregate_across_pi', 'pairwise_across_pi')

# pi_summary_df$num_samples * pi_summary_df$across_pi
# actual_across_pi = pi_summary_df$num_samples * pi_summary_df$across_pi
# pi_summary_df$across_pi = actual_across_pi
# pi_summary_df
# write.csv(pi_summary_df, './summarized_pi_fixed.csv', row.names=FALSE)
# write.csv(Your DataFrame,"Path to export the DataFrame\\File Name.csv", row.names = FALSE)

overlapping_list = c('Akkermansia_muciniphila_55290',
                     'Alistipes_finegoldii_56071',
                     'Alistipes_onderdonkii_55464',
                     'Alistipes_putredinis_61533',
                     'Alistipes_senegalensis_58364',
                     'Alistipes_shahii_62199',
                     'Anaerostipes_hadrus_55206',
                     'Bacteroides_caccae_53434',
                     'Bacteroides_finegoldii_57739',
                     'Bacteroides_fragilis_54507',
                     'Bacteroides_thetaiotaomicron_56941',
                     'Bacteroides_uniformis_57318',
                     'Bacteroides_vulgatus_57955',
                     'Bacteroides_xylanisolvens_57185'	,
                     'Barnesiella_intestinihominis_62208',
                     'Bifidobacterium_longum_57796',
                     'Bifidobacterium_pseudocatenulatum_57754',
                     'Bilophila_wadsworthia_57364',
                     'Blautia_wexlerae_56130',
                     'Butyricimonas_virosa_58742',
                     'Butyrivibrio_crossotus_61674',
                     'Clostridiales_bacterium_56470',
                     'Clostridiales_bacterium_61057',
                     'Clostridium_sp_61482',
                     'Collinsella_sp_62205',
                     'Coprococcus_sp_62244',
                     'Dialister_invisus_61905',
                     'Dorea_longicatena_59913',
                     'Dorea_longicatena_61473',
                     'Escherichia_coli_58110',
                     'Eubacterium_eligens_61678',
                     'Eubacterium_hallii_61477',
                     'Eubacterium_ramulus_59802',
                     'Eubacterium_rectale_56927',
                     'Eubacterium_siraeum_57634',
                     'Faecalibacterium_cf_62236',
                     'Faecalibacterium_prausnitzii_57453',
                     'Faecalibacterium_prausnitzii_61481',
                     'Faecalibacterium_prausnitzii_62201',
                     'Guyana_massiliensis_60772',
                     'Haemophilus_parainfluenzae_62356',
                     'Intestinimonas_butyriciproducens_60001',
                     'Odoribacter_splanchnicus_62174',
                     'Oscillibacter_sp_60799',
                     'Parabacteroides_distasonis_56985',
                     'Parabacteroides_merdae_56972',
                     'Phascolarctobacterium_sp_59817',
                     'Phascolarctobacterium_succinatutens_61948',
                     'Prevotella_copri_61740',
                     'Roseburia_hominis_61877',
                     'Roseburia_intestinalis_56239',
                     'Roseburia_inulinivorans_61943',
                     'Ruminococcus_bicirculans_59300',
                     'Ruminococcus_bromii_62047',
                     'Ruminococcus_gnavus_57638',
                     'Ruminococcus_lactaris_55568',
                     'Ruminococcus_obeum_61472',
                     'Ruminococcus_sp_55468',
                     'Ruminococcus_torques_62045',
                     'Sutterella_wadsworthensis_56828',
                     'Veillonella_atypica_58169',
                     'Veillonella_dispar_61763',
                     'Veillonella_parvula_57794')

list_over_20 = c('Eubacterium_eligens_61678',
                 'Faecalibacterium_cf_62236',
                 'Faecalibacterium_prausnitzii_57453',
                 'Faecalibacterium_prausnitzii_61481',
                 'Faecalibacterium_prausnitzii_62201',
                 'Oscillibacter_sp_60799',
                 'Prevotella_copri_61740',
                 'Roseburia_inulinivorans_61943',
                 'Ruminococcus_bromii_62047',
                 'Ruminococcus_torques_62045')

list_over_10 = c('Bifidobacterium_longum_57796',
                 'Blautia_wexlerae_56130',
                 'Butyrivibrio_crossotus_61674',
                 'Clostridium_sp_61482',
                 'Escherichia_coli_58110',
                 'Eubacterium_eligens_61678',
                 'Faecalibacterium_cf_62236',
                 'Faecalibacterium_prausnitzii_57453',
                 'Faecalibacterium_prausnitzii_61481',
                 'Faecalibacterium_prausnitzii_62201',
                 'Oscillibacter_sp_60799',
                 'Prevotella_copri_61740',
                 'Roseburia_inulinivorans_61943',
                 'Ruminococcus_bromii_62047',
                 'Ruminococcus_torques_62045')

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

over_20_df = subset(pi_summary_df, 
                    species %in% list_over_20)

species_labels_20 = c('E. eligens',
                      'F. cf',
                      'F. prausnitzii (57453)',
                      'F. prausnitzii (61481)',
                      'F. prausnitzii (62201)',
                      'O. sp',
                      'P. copri',
                      'R. inulinivorans',
                      'R. bromii',
                      'R. torques')

species_labels_10 = c('B. longum',
                      'B. wexlerae',
                      'B. crossotus',
                      'C. sp',
                      'E. coli',
                      'E. eligens',
                      'F. cf',
                      'F. prausnitzii (57453)',
                      'F. prausnitzii (61481)',
                      'F. prausnitzii (62201)',
                      'O. sp',
                      'P. copri',
                      'R. inulinivorans',
                      'R. bromii',
                      'R. torques')

over_10_df = subset(pi_summary_df,
                    species %in% list_over_10)

# Aggregate
aggregate_pi_comparison_20 <- ggplot(data=over_20_df, aes(x=species, y=average_pi, fill=Cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=Cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=50, vjust=1.0, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(label = "p.signif", method = "t.test") +
  scale_x_discrete(breaks=list_over_20) +
  guides(fill=guide_legend(title="Cohort")) +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 20') +
  xlab('Species') + 
  ylab('Average within-host pi') +
  theme(plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())
  
aggregate_pi_comparison_20

position_jitterdodge(
  jitter.width = NULL,
  jitter.height = 0,
  dodge.width = 0.5,
  seed = NA
)

better_pi_comparison_20 <- ggplot(data=over_20_df, aes(x=species, y=average_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=Cohort), size=4, shape=18, position=position_dodge(width=0.75)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle=90, vjust=1.0, hjust=1)) +
  xlab('Species') + 
  ylab('Average within-host pi') +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 20')
better_pi_comparison_20

temp = subset(over_20_df, Cohort==" African", select=c(species, Cohort, average_pi, num_sites, num_samples, aggregate_across_pi, pairwise_across_pi))
temp$aggregate_across_pi = temp$aggregate_across_pi / 2

better_pi_comparison_20 <- ggplot(data=temp, aes(x=species, y=average_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=Cohort), size=4, shape=18, position=position_dodge(width=0.75)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle=90, vjust=1.0, hjust=1)) +
  xlab('Species') + 
  ylab('Average within-host pi') +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 20')
better_pi_comparison_20

aggregate_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=species, y=average_pi, fill=cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  # geom_point(position=position_dodge(width=0.75),aes(group=cohort), size=1) +
  # geom_jitter(aes(color=cohort), size=0.2) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=50, vjust=1.0, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(label = "p.signif", method = "t.test") +
  scale_x_discrete(breaks=list_over_10,
                   labels=species_labels_10) +
  guides(fill=guide_legend(title="Cohort")) +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 10') +
  xlab('Species') +
  ylab('Average within-host pi')

aggregate_pi_comparison_10

better_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=species, y=average_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=Cohort), size=4, shape=18, position=position_dodge(width=0.75)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle=90, vjust=1.0, hjust=1)) +
  xlab('Species') + 
  ylab('Average within-host pi') +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 10')
better_pi_comparison_10

# Pairwise
pairwise_pi_comparison_20 <- ggplot(data=over_20_df, aes(x=species, y=average_pi, fill=Cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  # geom_point(position=position_dodge(width=0.75),aes(group=cohort), size=1) +
  # geom_jitter(aes(color=cohort), size=0.2) +
  geom_point(aes(x=species, y=pairwise_across_pi, color=Cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23))  +
  stat_compare_means(label = "p.signif", method = "t.test") +
  ggtitle('Pi within hosts and distributed across hosts, Minimum #samples >= 10') +
  xlab('Species') +
  ylab('Average within-host pi')

pairwise_pi_comparison_20

pairwise_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=species, y=average_pi, fill=cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  # geom_point(position=position_dodge(width=0.75),aes(group=cohort), size=1) +
  # geom_jitter(aes(color=cohort), size=0.2) +
  geom_point(aes(x=species, y=pairwise_across_pi, color=cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(label = "p.signif", method = "t.test") +
  ggtitle('Pi within hosts and distributed across hosts, Minimum #samples >= 10') +
  xlab('Species') +
  ylab('Average within-host pi')

pairwise_pi_comparison_20 <- ggplot(data=over_20_df, aes(x=species, y=average_pi, fill=cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  # geom_point(position=position_dodge(width=0.75),aes(group=cohort), size=1) +
  # geom_jitter(aes(color=cohort), size=0.2) +
  # geom_point(aes(x=species, y=pairwise_across_pi, color=cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23))  +
  stat_compare_means(label = "p.signif", method = "t.test") +
  ggtitle('Pi within hosts, Minimum #samples >= 20') +
  xlab('Species') +
  ylab('Average within-host pi') +
  scale_x_discrete(breaks=unique(over_20_df$species), labels=c('E. eligens', 'F. cf', 'F. prausnitzii1 ',
                                                               'F. prausnitzii 2', 'F. prausnitzii 3',
                                                               'O. sp', 'P. copri', 'R. inulinivorans',
                                                               'R. bromii', 'R. torques'))

pairwise_pi_comparison_20

pairwise_pi_comparison_10 + stat_compare_means(label = "p.signif", method = "t.test")

across_host_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=aggregate_across_pi, y=pairwise_across_pi, color=cohort)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab('Aggregate across-host pi') +
  ylab('Pairwise distributed across-host pi')
across_host_pi_comparison_10

# aggregate across host pi
aggregate_over_10 = over_10_df[c('species', 'cohort', 'aggregate_across_pi')]
aggregate_over_10 = distinct(aggregate_over_10)
aggregate_HMP_over_10 = aggregate_over_10[aggregate_over_10$cohort==' HMP', ]
aggregate_African_over_10 = aggregate_over_10[aggregate_over_10$cohort==' African', ]
aggregate_over_10 = rbind(aggregate_African_over_10, aggregate_HMP_over_10)
# cohort aggregate_comparison

cohort_aggregate_comparison <- ggpaired(data=aggregate_over_10, x='cohort', y='aggregate_across_pi', color='cohort', line.color='grey') +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(method = "t.test", paired=TRUE, label.y=0.06, label.x=1.5) +
  ggtitle('Aggregate pi across hosts by cohort, Minimum #samples >= 10') +
  xlab('Cohort') +
  ylab('Aggregate across-host pi')
cohort_aggregate_comparison

# pairwise across host pi
pairwise_over_10 = over_10_df[c('species', 'cohort', 'pairwise_across_pi')]
pairwise_over_10 = distinct(pairwise_over_10)
pairwise_HMP_over_10 = pairwise_over_10[pairwise_over_10$cohort==' HMP', ]
pairwise_African_over_10 = pairwise_over_10[pairwise_over_10$cohort==' African', ]
pairwise_over_10 = rbind(pairwise_African_over_10, pairwise_HMP_over_10)
# cohort aggregate_comparison

cohort_pairwise_comparison <- ggpaired(data=pairwise_over_10, x='cohort', y='pairwise_across_pi', color='cohort', line.color='grey') +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(method = "t.test", paired=TRUE, label.y=0.06, label.x=1.5) +
  ggtitle('Pairwise distributed pi across hosts by cohort, Minimum #samples >= 10') +
  xlab('Cohort') +
  ylab('Pairwise distributed across-host pi')
cohort_pairwise_comparison

set.seed(1)

over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Bifidobacterium_longum_57796' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' African', ]$pairwise_across_pi =
  compute_pi(over_iid_df[over_iid_df$species=='Blautia_wexlerae_56130' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Butyrivibrio_crossotus_61674' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Eubacterium_eligens_61678' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Escherichia_coli_58110' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_61481' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_57453' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_prausnitzii_62201' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Faecalibacterium_cf_62236' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Oscillibacter_sp_60799' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Prevotella_copri_61740' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Roseburia_inulinivorans_61943' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' HMP', ]$pairwise_across_pi)
over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_iid_df[over_iid_df$species=='Ruminococcus_bromii_62047' & over_iid_df$Cohort==' African', ]$pairwise_across_pi)

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

better_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=reorder(species, ordered_pi), y=average_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  geom_point(aes(x=species, y=pairwise_across_pi, color=Cohort), size=4, shape=16, position=position_dodge(width=0.75)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle=90, vjust=1.0, hjust=1)) +
  xlab('Species') + 
  ylab('Average within-host pi') +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 20')
better_pi_comparison_10

better_pi_comparison_iid <- ggplot(data=over_iid_df, aes(x=reorder(species, ordered_pi), y=average_pi, fill=Cohort)) +
  geom_boxplot(aes(fill=Cohort), outlier.shape=NA) +
  geom_point(pch = 21, position = position_jitterdodge(), size=1.5) +
  geom_point(aes(x=species, y=pairwise_across_pi, color=Cohort), size=4, shape=16, position=position_dodge(width=0.75)) +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(size=rel(1.5), angle=90, vjust=1.0, hjust=1)) +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(axis.title.x = element_text(size=rel(1.5))) +
  theme(axis.title.y = element_text(size=rel(1.5))) +
  theme(legend.text = element_text(size=rel(1.25))) +
  theme(legend.title = element_text(size=rel(1.5))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x,)),
                limits = c(0.0001, 0.2)) +
  xlab('Species') + 
  ylab('Nucleotide Diversity') +
  # ylim(0, 0.03) +
  # stat_compare_means(method='wilcox.test', label = "p.signif", label.x = 0.75,
  #                    label.y = 0.0275, size=6)
  stat_compare_means(method='wilcox.test', label='p.signif', size=6)
  # stat_compare_means(method='wilcox.test', label='p.format')
better_pi_comparison_iid

distinct_iid_df = distinct(over_iid_df, pairwise_across_pi, .keep_all=TRUE)
HMP_distinct_pairwise = distinct_iid_df[distinct_iid_df$Cohort==' HMP', ]$pairwise_across_pi
African_distinct_pairwise = distinct_iid_df[distinct_iid_df$Cohort==' African', ]$pairwise_across_pi

wilcox.test(HMP_distinct_pairwise, African_distinct_pairwise, paired=TRUE)

# Species prevalence
species_prevalence = read.csv('../Summary/species_prevalence.txt', sep=',')
species_prevalence = data.frame(species_prevalence$species, species_prevalence$prevalence)
names(species_prevalence) = c('species', 'prevalence')
species_prevalence$prevalence=species_prevalence$prevalence-2
species_prevalence = subset(species_prevalence, prevalence > 20)
# names(species_prevalence) = c('species', 'prevalence')
ggplot(species_prevalence, aes(x = reorder(species, prevalence), y=prevalence, fill=species)) +
  geom_bar(stat='identity') +
  theme(legend.position = "none") +
  coord_flip() +
  xlab('Species') +
  ylab('Prevalence') +
  ggtitle('Highest Prevalence Gut Microbial Species in North American Microbiomes')
# species_prevalence = species_prevalence[species_prevalence$species_prevalence.prevalence>= 0]

rho_mu = read.csv('../Data/r_mu_dic.csv', header = FALSE)
rho_mu$significant = c('Yes', 'Yes', 'Yes', 'No', 'No', 
                       'No', 'Yes', 'No', 'No', 'No', 
                       'No', 'No', 'No', 'No', 'No', 
                       'No', 'No', 'Yes', 'No', 'No', 
                       'No', 'Yes', 'No', 'No', 'No', 
                       'No', 'No', 'No', 'No', 'No', 
                       'No', 'No', 'No', 'Yes', 'Yes', 
                       'Yes', 'No', 'No', 'No', 'No', 
                       'No')
names(rho_mu) = c('species', 'rho_ratio', 'Significant')

rho_mu_plot = ggplot(aes(x=species, y=rho_ratio), data = rho_mu) +
  geom_bar(aes(fill = Significant), stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Significance of demographic model vs. ratio of recombination to mutation') +
  xlab('Species') +
  ylab('Rho / mu')

rho_mu_plot

rho_mu_plot = ggplot(aes(x = reorder(species, -rho_ratio), y = rho_ratio), data = rho_mu) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('Rate of recombination vs. mutation in commensal gut species') +
  xlab('Species') +
  ylab('Rho / mu')

rho_mu_plot

# Number of QP samples per species

species_list = c('Akkermansia_muciniphila_55200',
                 'Alistipes_finegoldii_56071',
                 'Alistipes_onderdonkii_55464',
                 'Alistipes_putredinis_61533',
                 'Alistipes_shahii_62199',
                 'Bacteroidales_bacterium_58650',
                 'Bacteroides_caccae_53434',
                 'Bacteroides_cellulosilyticus',
                 'Bacteroides_frgailis_5507',
                 'Bacteroides_massiliensis_44749',
                 'Bacteroides_ovatus_58035',
                 'Bacteroides_stercoris',
                 'Bacteroides_thetaiotaomicron_56941',
                 'Bacteroides_uniformis_57318',
                 'Bacteroides_vulgatus_57955',
                 'Bacteroides_xylanisolvens_57185',
                 'Barnesiella_intestinihominis_62208',
                 'Coprococcus_sp_62244',
                 'Dialest_invisus_61905',
                 'Eubacterium_eligens_61678',
                 'Eubacterium_rectale_56927',
                 'Faecalibacterium_prausnitzii_57453',
                 'Odoribacter_splanchnicus_62174',
                 'Oscillibacter_sp_60799',
                 'Parabacteroides_distasonis_56985',
                 'Parabacteroides_merdae_56972',
                 'Phasolarctobacterium_sp_59817',
                 'Prevotella_copri_61740',
                 'Ruminococcus_bicirculans_59300',
                 'Ruminococcus_bromii_62047')

num_qp_samples = c(19, 39, 53, 33, 55,
                   29, 31, 33, 29, 9, 
                   47, 59, 67, 75, 49, 
                   45, 41, 9, 33, 25, 
                   67, 31, 31, 47, 47, 
                   37, 21, 15, 61, 33)

qp_samples_per_species_csv = 
'Akkermansia_muciniphila_55200, 19
Alistipes_finegoldii_56071, 39
Alistipes_onderdonkii_55464, 53
Alistipes_putredinis_61533, 33
Alistipes_shahii_62199, 55
Bacteroidales_bacterium_58650, 29
Bacteroides_caccae_53434, 31
Bacteroides_cellulosilyticus, 33
Bacteroides_frgailis_5507, 29
Bacteroides_massiliensis_44749, 9
Bacteroides_ovatus_58035, 47
Bacteroides_stercoris, 59
Bacteroides_thetaiotaomicron_56941, 67
Bacteroides_uniformis_57318, 75
Bacteroides_vulgatus_57955, 49
Bacteroides_xylanisolvens_57185, 45
Barnesiella_intestinihominis_62208, 41
Coprococcus_sp_62244, 9
Dialest_invisus_61905, 33
Eubacterium_eligens_61678, 25
Eubacterium_rectale_56927, 67
Faecalibacterium_prausnitzii_57453, 31
Odoribacter_splanchnicus_62174, 31
Oscillibacter_sp_60799, 47
Parabacteroides_distasonis_56985, 47
Parabacteroides_merdae_56972, 37
Phasolarctobacterium_sp_59817, 21
Prevotella_copri_61740, 15
Ruminococcus_bicirculans_59300, 61
Ruminococcus_bromii_62047, 33
'
qp_samples_per_species = data.frame(species_list, num_qp_samples)

qp_samples_per_species_plot = ggplot(aes(x=species_list, y=num_qp_samples), 
                                     data = qp_samples_per_species) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle('QP samples per species') +
  xlab('Species') +
  ylab('Number of QP samples')
qp_samples_per_species_plot

qp_samples_per_species_plot_ordered = ggplot(qp_samples_per_species, aes(x = reorder(species_list, -num_qp_samples), y = num_qp_samples)) +
  geom_bar(stat = 'identity', position = 'dodge', color='#10549c', fill='#10549c') +
  ggtitle('Number of Quasi-phaseable samples per species') +
  xlab('Species') +
  ylab('Number of Qausi-phaseable samples') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
qp_samples_per_species_plot_ordered

set.seed(1)

fold_sfs = function(input_sfs) {
  input_length = length(input_sfs)
  folded_length = length(input_sfs) / 2
  if (input_length %% 2 == 1) {
    folded_length = folded_length + 1
  }
  output_sfs = c()
  for (i in 1:folded_length) {
    output_sfs[i] = input_sfs[i] + input_sfs[input_length - i]
  }
  return(output_sfs)
}

proportional_sfs = function(input_sfs) {
  return (input_sfs / sum(input_sfs))
}

x <- datasim
loglik <- function(theta){
  k<- theta[1]
  lambda<- theta[2]
  out <- sum(dweibull(x,shape = k, scale=lambda, log = TRUE) )
  return(out)
}



e_eligens_pi = read.csv('../Scripts/e_eligens_pi_values.txt', header=TRUE)

b_thetaiotaomicron_downsampled_empirical = proportional_sfs(c(11329.94672695546, 
                                                              6167.075485250698,
                                                              4239.177625142324,
                                                              3342.60586700556,
                                                              2929.626632089841,
                                                              2725.643057081244,
                                                              2635.300969843948,
                                                              2579.444903486481,
                                                              2545.779751971833,
                                                              1267.150917730179))

b_thetaiotaomicron_downsampled_one_epoch = proportional_sfs(fold_sfs(c(11395.016421153823, 
                                                                       5697.509294687101,
                                                                       3798.3396167211836,
                                                                       2848.7547698431354,
                                                                       2279.0038585801676,
                                                                       1899.1699154494872,
                                                                       1627.8599542022703,
                                                                       1424.377481668122,
                                                                       1266.113334959331,
                                                                       1139.5020163371091,
                                                                       1035.910936328398,
                                                                       949.5850352831061,
                                                                       876.5400411349472,
                                                                       813.9300452646684,
                                                                       759.6680480177727,
                                                                       712.188799653274,
                                                                       670.2953444866343,
                                                                       633.0567169853921,
                                                                       599.7379443605873)))

b_thetaiotaomicron_downsampled_two_epoch = proportional_sfs(fold_sfs(c(8664.919870727774,
                                                                       5410.886429304165,
                                                                       3793.1174736529974,
                                                                       2881.691449665706,
                                                                       2313.2722795865775,
                                                                       1929.5172163186119,
                                                                       1654.2900929663722,
                                                                       1447.6034828585334,
                                                                       1286.7826375194118,
                                                                       1158.1101597985346,
                                                                       1052.828809778185,
                                                                       965.0934105406639,
                                                                       890.8555390240017,
                                                                       827.2230241899715,
                                                                       772.0748319217155,
                                                                       723.820160327517,
                                                                       681.2425076585495,
                                                                       643.3957044905935,
                                                                       609.5327746634593)))

b_thetaiotaomicron_downsampled_exponential = proportional_sfs(fold_sfs(c(10187.526135309816,
                                                                         5227.044599884645,
                                                                         3563.621850201483,
                                                                         2727.3004953552727,
                                                                         2222.9082682087164,
                                                                         1885.0050092572908,
                                                                         1642.5279426703821,
                                                                         1459.8676681937545,
                                                                         1317.1988255364213,
                                                                         1202.6013280281009,
                                                                         1108.4741630743192,
                                                                         1029.739855850399,
                                                                         962.8763478354583,
                                                                         905.3630546950819,
                                                                         855.3480686875307,
                                                                         811.4398933124559,
                                                                         772.5725127938603,
                                                                         737.9153260169305,
                                                                         706.8114463671081)))

b_thetaiotaomicron_downsampled_bottleneck = proportional_sfs(fold_sfs(c(8717.037555032524,
                                                                        5310.474345322664,
                                                                        3748.8643888138054,
                                                                        2873.723809926384,
                                                                        2321.016005698025,
                                                                        1942.9554470128837,
                                                                        1669.1777593196819,
                                                                        1462.2643030012487,
                                                                        1300.6200461207936,
                                                                        1170.9684986786854,
                                                                        1064.7251166497713,
                                                                        976.1057198156356,
                                                                        901.0770666862816,
                                                                        836.7441818986339,
                                                                        780.9769787756552,
                                                                        732.174217565379,
                                                                        689.1094802578586,
                                                                        650.8278440587161,
                                                                        616.5748554746294)))

b_thetaiotaomicron_downsampled_three_epoch = proportional_sfs(fold_sfs(c(8689.269010159136,
                                                                         5417.315212120648,
                                                                         3793.8944085777216,
                                                                         2881.1151931885984,
                                                                         2312.4701639083023,
                                                                         1928.7523709984505,
                                                                         1653.6077971646323,
                                                                         1446.9991763032233,
                                                                         1286.2435100988198,
                                                                         1157.6244240487513,
                                                                         1052.3870965152491,
                                                                         964.6884724782393,
                                                                         890.4817416585565,
                                                                         826.8759246652239,
                                                                         771.7508719243848,
                                                                         723.5164477365333,
                                                                         680.9566604946273,
                                                                         643.1257377201415,
                                                                         609.2770166692798)))

b_thetaiotaomicron_downsampled_x_axis = 1:length(b_thetaiotaomicron_downsampled_one_epoch)

b_thetaiotaomicron_downsampled_df = data.frame(b_thetaiotaomicron_downsampled_empirical,
                                               b_thetaiotaomicron_downsampled_two_epoch,
                                               b_thetaiotaomicron_downsampled_one_epoch,
                                               b_thetaiotaomicron_downsampled_x_axis)


names(b_thetaiotaomicron_downsampled_df) = c('Observed',
                                             'Expected under two-epoch demography',
                                             'Null expectation, i.e., standard neutral model',
                                             'x_axis')

p_b_thetaiotaomicron_downsampled_comparison <- ggplot(data = melt(b_thetaiotaomicron_downsampled_df, id='x_axis'),
                                                      aes(x=x_axis, 
                                                          y=value,
                                                          fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "") +
  scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=b_thetaiotaomicron_downsampled_x_axis, limits=c(1.5, length(b_thetaiotaomicron_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Thetaiotaomicron Site Frequency Spectrum, Downsampled to 20 samples') +
  ylim(0, 0.25) +
  ylab('Proportion of Segregating Sites') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))

p_b_thetaiotaomicron_downsampled_comparison

b_thetaiotaomicron_downsampled_small_df = data.frame(b_thetaiotaomicron_downsampled_empirical,
                                                     b_thetaiotaomicron_downsampled_one_epoch,
                                                     b_thetaiotaomicron_downsampled_two_epoch,
                                                     b_thetaiotaomicron_downsampled_x_axis)


names(b_thetaiotaomicron_downsampled_small_df) = c('Empirical',
                                                   'One-epoch',
                                                   'Two-epoch',
                                                   'x_axis')

b_thetaiotaomicron_downsampled_one_epoch_delta = abs(b_thetaiotaomicron_downsampled_empirical - b_thetaiotaomicron_downsampled_one_epoch)
b_thetaiotaomicron_downsampled_two_epoch_delta = abs(b_thetaiotaomicron_downsampled_empirical - b_thetaiotaomicron_downsampled_two_epoch)
b_thetaiotaomicron_downsampled_delta_df = data.frame(b_thetaiotaomicron_downsampled_one_epoch_delta,
                                                     b_thetaiotaomicron_downsampled_two_epoch_delta,
                                                     b_thetaiotaomicron_downsampled_x_axis)
names(b_thetaiotaomicron_downsampled_delta_df) = c('One-epoch Delta',
                                                   'Two-epoch Delta',
                                                   'x_axis')


p_b_thetaiotaomicron_downsampled_delta <- ggplot(data = melt(b_thetaiotaomicron_downsampled_delta_df, id='x_axis'),
                                                 aes(x=x_axis, 
                                                     y=value,
                                                     fill=variable)) +
  geom_line(aes(color=variable)) +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_thetaiotaomicron_downsampled_x_axis, limits=c(1.5, length(b_thetaiotaomicron_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Thetaiotaomicron (Downsampled to 20)') +
  ylab('Absolute difference in proportional frequency') +
  geom_text(aes(label = paste0("One-Epoch Delta Sum = ", sum(b_thetaiotaomicron_downsampled_one_epoch_delta)), 
                x = (9), 
                y = 0.0025)) +
  geom_text(aes(label = paste0("Two-Epoch Delta Sum = ", sum(b_thetaiotaomicron_downsampled_two_epoch_delta)), 
                x = (9),
                y = 0.0015)) 

p_b_thetaiotaomicron_downsampled_delta

input_csv = read.csv('temp_output.txt', header=FALSE)
names(input_csv) = c('likelihood', 'nu', 'tau')
plot = ggplot(data=input_csv, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
plot


input_csv_2 = read.csv('temp_output_2.txt', header=FALSE)
names(input_csv_2) = c('likelihood', 'nu', 'tau')

input_csv_2[input_csv_2$likelihood < -150, ]$likelihood = -150

plot_2 = ggplot(data=input_csv_2, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
plot_2

input_csv_3 = read.csv('temp_output/b_thetaiotaomicron_scatter.txt', header=FALSE)
names(input_csv_3) = c('likelihood', 'nu', 'tau')

input_csv_3[input_csv_3$likelihood < -150, ]$likelihood = -150

input_csv_4 = read.csv('temp_output/b_xylanisolvens_scatter.txt', header=FALSE)
names(input_csv_4) = c('likelihood', 'nu', 'tau')

input_csv_4[input_csv_4$likelihood < -150, ]$likelihood = -150

input_csv_5 = read.csv('temp_output/b_intestinihominis_scatter.txt', header=FALSE)
names(input_csv_5) = c('likelihood', 'nu', 'tau')

input_csv_5[input_csv_5$likelihood < -150, ]$likelihood = -150

input_csv_6 = read.csv('temp_output/p_distasonis_scatter.txt', header=FALSE)
names(input_csv_6) = c('likelihood', 'nu', 'tau')

input_csv_6[input_csv_6$likelihood < -150, ]$likelihood = -150

scatter_b_thetaiotaomicron = ggplot(data=input_csv_3, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
scatter_b_thetaiotaomicron

input_csv_4 = input_csv_4[order(-input_csv_4$likelihood), ]

input_csv_4[input_csv_4$tau < 0.0001, ]

input_csv_4[input_csv_4$tau < 0.00001, ]

b_xylanisolvens_downsampled_empirical = c(2354.975426140429,
                                          1125.802067113204, 809.8079494704909,
                                          689.0643046976489, 622.4599121761868,
                                          562.9077350396836, 521.9242943638338,
                                          498.4492947002323, 483.0524634080177,
                                          240.2388435326407)

b_xylanisolvens_downsampled_red = fold_sfs(c(2225.8375688163587, 1112.9189983969616,
                                             741.946017393644, 556.4595253498074,
                                             445.1676295107077, 370.9730318372629,
                                             317.9768902767878, 278.2297837939835,
                                             247.31536736626893, 222.58383397882884,
                                             202.34894280359518, 185.486533288166,
                                             171.21834043457568, 158.98846067272606,
                                             148.38923138441444, 139.11490560600612,
                                             130.93167683575913, 123.6576955724437,
                                             117.14939642030969))

b_xylanisolvens_downsampled_orange = fold_sfs(c(1354.6450720818552, 964.1121714611451,
                                                723.3597958043349, 567.8574193786202,
                                                462.71067502604933, 388.4800696665438,
                                                333.988234526398, 292.59153993815636,
                                                260.2041943786457, 234.22626049010108,
                                                212.9474333685768, 195.20663452063698,
                                                180.19229972042848, 167.32190788610325,
                                                156.16725916101194, 146.40684653213373,
                                                137.79469013723616, 130.13943250581772,
                                                123.28998955486668))

b_xylanisolvens_downsampled_violet = fold_sfs(c(2150.301238110714, 1111.0540590328678,
                                                742.1488683045079, 556.7010970246465,
                                                445.36807402550346, 371.14075768398175,
                                                318.1207296252597, 278.35565159531444,
                                                247.4272504985305, 222.68452850602046,
                                                202.4404829010326, 185.57044467799253,
                                                171.29579676175558, 159.06038408863958,
                                                148.45635961048444, 139.17783804093617,
                                                130.990907102119, 123.71363502234682,
                                                117.2023914555424))

b_xylanisolvens_downsampled_one_epoch = fold_sfs(c(2225.8375946491637, 1112.9190090884904,
                                                   741.9460230393622, 556.4595284726163,
                                                   445.16763111997324, 370.9730324378159,
                                                   317.97689015707357, 278.22978313422897,
                                                   247.31536628656738, 222.58383256325666,
                                                   202.34894111319673, 185.48653136871667,
                                                   171.21833832126538, 158.98845839321308,
                                                   148.38922896089073, 139.11490305652447,
                                                   130.93167417529625, 123.65769281345032,
                                                   117.14939357325068))

b_xylanisolvens_downsampled_two_epoch = fold_sfs(c(1354.7424693587352, 964.1545370204719,
                                                   723.3720688058446, 567.8574177822163,
                                                   462.70629991290303, 388.4744983802059,
                                                   333.98264785739957, 292.58632056979917,
                                                   260.1994236208664, 234.22191614360744,
                                                   212.94346498536052, 195.20298995814255,
                                                   180.18893310882493, 167.31878094576342,
                                                   156.1643404299584, 146.4041101466808,
                                                   137.79211469511313, 130.13700013878122,
                                                   123.28768520619438))

b_xylanisolvens_downsampled_x_axis = 1:length(b_xylanisolvens_downsampled_empirical)

b_xylanisolvens_downsampled_df = data.frame(b_xylanisolvens_downsampled_empirical,
                                            b_xylanisolvens_downsampled_red,
                                            b_xylanisolvens_downsampled_orange,
                                            b_xylanisolvens_downsampled_violet,
                                            b_xylanisolvens_downsampled_x_axis,
                                            b_xylanisolvens_downsampled_one_epoch,
                                            b_xylanisolvens_downsampled_two_epoch)


names(b_xylanisolvens_downsampled_df) = c('Empirical',
                                          'Red',
                                          'Orange',
                                          'Violet',
                                          'x_axis',
                                          'One epoch',
                                          'Two epoch')

p_b_xylanisolvens_downsampled_comparison <- ggplot(data = melt(b_xylanisolvens_downsampled_df, id='x_axis'),
                                                   aes(x=x_axis, 
                                                       y=value,
                                                       fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Parameter Estimate") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_xylanisolvens_downsampled_x_axis, limits=c(0.5, length(b_xylanisolvens_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Xylanisolvens (Downsampled, Raw Counts)') +
  ylab('Proportional Frequency') +
  scale_fill_manual(values = c('black', 'red', 'orange', 'violet', 'blue', 'green')) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p_b_xylanisolvens_downsampled_comparison

b_xylanisolvens_downsampled_empirical = proportional_sfs(c(2354.975426140429,
                                          1125.802067113204, 809.8079494704909,
                                          689.0643046976489, 622.4599121761868,
                                          562.9077350396836, 521.9242943638338,
                                          498.4492947002323, 483.0524634080177,
                                          240.2388435326407))

b_xylanisolvens_downsampled_red = proportional_sfs(fold_sfs(c(2225.8375688163587, 1112.9189983969616,
                                             741.946017393644, 556.4595253498074,
                                             445.1676295107077, 370.9730318372629,
                                             317.9768902767878, 278.2297837939835,
                                             247.31536736626893, 222.58383397882884,
                                             202.34894280359518, 185.486533288166,
                                             171.21834043457568, 158.98846067272606,
                                             148.38923138441444, 139.11490560600612,
                                             130.93167683575913, 123.6576955724437,
                                             117.14939642030969)))

b_xylanisolvens_downsampled_orange = proportional_sfs(fold_sfs(c(1354.6450720818552, 964.1121714611451,
                                                723.3597958043349, 567.8574193786202,
                                                462.71067502604933, 388.4800696665438,
                                                333.988234526398, 292.59153993815636,
                                                260.2041943786457, 234.22626049010108,
                                                212.9474333685768, 195.20663452063698,
                                                180.19229972042848, 167.32190788610325,
                                                156.16725916101194, 146.40684653213373,
                                                137.79469013723616, 130.13943250581772,
                                                123.28998955486668)))

b_xylanisolvens_downsampled_violet = proportional_sfs(fold_sfs(c(2150.301238110714, 1111.0540590328678,
                                                742.1488683045079, 556.7010970246465,
                                                445.36807402550346, 371.14075768398175,
                                                318.1207296252597, 278.35565159531444,
                                                247.4272504985305, 222.68452850602046,
                                                202.4404829010326, 185.57044467799253,
                                                171.29579676175558, 159.06038408863958,
                                                148.45635961048444, 139.17783804093617,
                                                130.990907102119, 123.71363502234682,
                                                117.2023914555424)))

b_xylanisolvens_downsampled_one_epoch = proportional_sfs(fold_sfs(c(2225.8375946491637, 1112.9190090884904,
                                                   741.9460230393622, 556.4595284726163,
                                                   445.16763111997324, 370.9730324378159,
                                                   317.97689015707357, 278.22978313422897,
                                                   247.31536628656738, 222.58383256325666,
                                                   202.34894111319673, 185.48653136871667,
                                                   171.21833832126538, 158.98845839321308,
                                                   148.38922896089073, 139.11490305652447,
                                                   130.93167417529625, 123.65769281345032,
                                                   117.14939357325068)))

b_xylanisolvens_downsampled_two_epoch = proportional_sfs(fold_sfs(c(1354.7424693587352, 964.1545370204719,
                                                   723.3720688058446, 567.8574177822163,
                                                   462.70629991290303, 388.4744983802059,
                                                   333.98264785739957, 292.58632056979917,
                                                   260.1994236208664, 234.22191614360744,
                                                   212.94346498536052, 195.20298995814255,
                                                   180.18893310882493, 167.31878094576342,
                                                   156.1643404299584, 146.4041101466808,
                                                   137.79211469511313, 130.13700013878122,
                                                   123.28768520619438)))

b_xylanisolvens_downsampled_x_axis = 1:length(b_xylanisolvens_downsampled_empirical)

b_xylanisolvens_downsampled_df = data.frame(b_xylanisolvens_downsampled_empirical,
                                            b_xylanisolvens_downsampled_red,
                                            b_xylanisolvens_downsampled_orange,
                                            b_xylanisolvens_downsampled_violet,
                                            b_xylanisolvens_downsampled_x_axis,
                                            b_xylanisolvens_downsampled_one_epoch,
                                            b_xylanisolvens_downsampled_two_epoch)


names(b_xylanisolvens_downsampled_df) = c('Empirical',
                                          'Red',
                                          'Orange',
                                          'Violet',
                                          'x_axis',
                                          'One epoch',
                                          'Two epoch')

p_b_xylanisolvens_downsampled_comparison <- ggplot(data = melt(b_xylanisolvens_downsampled_df, id='x_axis'),
                                                   aes(x=x_axis, 
                                                       y=value,
                                                       fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Demographic Model") +
  scale_x_continuous(name='Frequency in Sample', breaks=b_xylanisolvens_downsampled_x_axis, limits=c(1.5, length(b_xylanisolvens_downsampled_x_axis) + 0.5)) +
  ggtitle('B. Xylanisolvens (Downsampled, Proportional)') +
  ylab('Proportional Frequency') +
  scale_fill_manual(values = c('black', 'red', 'orange', 'violet', 'blue', 'green')) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p_b_xylanisolvens_downsampled_comparison

scatter_b_xylanisolvens = ggplot(data=input_csv_4, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  geom_point(aes(x=4.06991e-05, y=0.000467818), color='red', size=3) +
  geom_point(aes(x=0.000631269, y=4.55855e-05), color='orange', size=3) +
  geom_point(aes(x=0.001183690, y=2.92090e-06), color='violet', size=3) +
  geom_point(aes(x=8.48466166e-06, y=6.13311336e-07), color='green', size=3)
scatter_b_xylanisolvens

scatter_b_intestinihominis = ggplot(data=input_csv_5, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
scatter_b_intestinihominis

scatter_p_distasonis = ggplot(data=input_csv_6, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
scatter_p_distasonis

# a_muciniphila_exp_scatter = read.csv('temp_output/a_muciniphila_expansion_scatter.txt', header=FALSE)
a_muciniphila_exp_scatter = read.csv('likelihood_surfaces/a_muciniphila_likelihood_surface.csv', header=FALSE)
names(a_muciniphila_exp_scatter) = c('likelihood', 'nu', 'tau')

a_muciniphila_exp_scatter[a_muciniphila_exp_scatter$likelihood < -100, ]$likelihood = -85

exp_scatter_a_muciniphila = ggplot(data=a_muciniphila_exp_scatter, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('A. muciniphila rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_a_muciniphila

a_muciniphila_expansion = a_muciniphila_exp_scatter[a_muciniphila_exp_scatter$nu > 1.0, ]
a_muciniphila_contraction = a_muciniphila_exp_scatter[a_muciniphila_exp_scatter$nu <= 1.0, ]

a_muciniphila_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = a_muciniphila_expansion) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = a_muciniphila_contraction) +
  scale_fill_manual(name = "Demographic Change", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('A. muciniphila histogram of likelihoods')
a_muciniphila_hist

# a_onderdonkii_exp_scatter = read.csv('temp_output/a_onderdonkii_expansion_scatter.txt', header=FALSE)
a_onderdonkii_exp_scatter = read.csv('likelihood_surfaces/a_onderdonkii_likelihood_surface.csv', header=FALSE)
names(a_onderdonkii_exp_scatter) = c('likelihood', 'nu', 'tau')

a_onderdonkii_exp_scatter[a_onderdonkii_exp_scatter$likelihood < -75, ]$likelihood = -75

exp_scatter_a_onderdonkii = ggplot(data=a_onderdonkii_exp_scatter, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('A. onderonkii rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_a_onderdonkii

a_onderdonkii_expansion = a_onderdonkii_exp_scatter[a_onderdonkii_exp_scatter$nu > 1.0, ]
a_onderdonkii_contraction = a_onderdonkii_exp_scatter[a_onderdonkii_exp_scatter$nu <= 1.0, ]

a_onderdonkii_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = a_onderdonkii_expansion) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = a_onderdonkii_contraction) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('A. onderdonkii histogram of likelihoods')
a_onderdonkii_hist

# b_intestinihominis_exp_scatter = read.csv('temp_output/b_intestinihominis_expansion_scatter.txt', header=FALSE)
b_intestinihominis_exp_scatter = read.csv('likelihood_surfaces/b_intestinihominis_likelihood_surface.csv', header=FALSE)
names(b_intestinihominis_exp_scatter) = c('likelihood', 'nu', 'tau')

b_intestinihominis_exp_scatter[b_intestinihominis_exp_scatter$likelihood < -75, ]$likelihood = -75

exp_scatter_b_intestinihominis = ggplot(data=b_intestinihominis_exp_scatter, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('B. intestinihominis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_b_intestinihominis

b_intestinihominis_expansion = b_intestinihominis_exp_scatter[b_intestinihominis_exp_scatter$nu > 1.0, ]
b_intestinihominis_contraction = b_intestinihominis_exp_scatter[b_intestinihominis_exp_scatter$nu <= 1.0, ]

b_intestinihominis_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = b_intestinihominis_expansion) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = b_intestinihominis_contraction) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('B. intestinihominis histogram of likelihoods')
b_intestinihominis_hist

# b_thetaiotaomicron_exp_scatter = read.csv('temp_output/b_thetaiotaomicron_expansion_scatter.txt', header=FALSE)
b_thetaiotaomicron_exp_scatter = read.csv('likelihood_surfaces/b_thetaiotaomicron_likelihood_surface.csv', header=FALSE)
names(b_thetaiotaomicron_exp_scatter) = c('likelihood', 'nu', 'tau')

b_thetaiotaomicron_exp_scatter[b_thetaiotaomicron_exp_scatter$likelihood < -75, ]$likelihood = -75

exp_scatter_b_thetaiotaomicron = ggplot(data=b_thetaiotaomicron_exp_scatter, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('B. thetaiotaomicron rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_b_thetaiotaomicron

b_thetaiotaomicron_expansion = b_thetaiotaomicron_exp_scatter[b_thetaiotaomicron_exp_scatter$nu > 1.0, ]
b_thetaiotaomicron_contraction = b_thetaiotaomicron_exp_scatter[b_thetaiotaomicron_exp_scatter$nu <= 1.0, ]

b_thetaiotaomicron_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = b_thetaiotaomicron_expansion) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = b_thetaiotaomicron_contraction) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('B. thetaiotaomicron histogram of likelihoods')
b_thetaiotaomicron_hist

# b_xylanisolvens_exp_scatter = read.csv('temp_output/b_xylanisolvens_expansion_scatter.txt', header=FALSE)
b_xylanisolvens_exp_scatter = read.csv('likelihood_surfaces/b_xylanisolvens_likelihood_surface.csv', header=FALSE)
names(b_xylanisolvens_exp_scatter) = c('likelihood', 'nu', 'tau')

b_xylanisolvens_exp_scatter[b_xylanisolvens_exp_scatter$likelihood < -55, ]$likelihood = -55

exp_scatter_b_xylanisolvens = ggplot(data=b_xylanisolvens_exp_scatter, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('B. xylanisolvens rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_b_xylanisolvens

b_xylanisolvens_expansion = b_xylanisolvens_exp_scatter[b_xylanisolvens_exp_scatter$nu > 1.0, ]
b_xylanisolvens_contraction = b_xylanisolvens_exp_scatter[b_xylanisolvens_exp_scatter$nu <= 1.0, ]

b_xylanisolvens_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = b_xylanisolvens_expansion) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = b_xylanisolvens_contraction) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10()  +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('B. xylanisolvens histogram of likelihoods')
b_xylanisolvens_hist

# p_distasonis_exp_scatter = read.csv('temp_output/p_distasonis_expansion_scatter.txt', header=FALSE)
p_distasonis_exp_scatter = read.csv('likelihood_surfaces/p_distasonis_likelihood_surface.csv', header=FALSE)
names(p_distasonis_exp_scatter) = c('likelihood', 'nu', 'tau')

p_distasonis_exp_scatter[p_distasonis_exp_scatter$likelihood < -75, ]$likelihood = -75

p_distasonis_good_likelihood = p_distasonis_exp_scatter[p_distasonis_exp_scatter$likelihood > -60, ]
fit = lm(log(p_distasonis_good_likelihood$tau) ~ log(p_distasonis_good_likelihood$nu))

exp_scatter_p_distasonis = ggplot(data=p_distasonis_exp_scatter, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('P. distasonis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red') +
  geom_abline(intercept = -1.2, slope = 1.05, col='violet') +
  geom_point(aes(x=0.1, y=0.006), color='red', size=3, shape=15) +
  geom_point(aes(x=0.01, y=0.00045), color='orange', size=3, shape=15) +
  geom_point(aes(x=0.001, y=4.5e-05), color='green', size=3, shape=15) +
  geom_point(aes(x=5.63834969e-04, y=3.09685202e-05), color='violet', size=3, shape=15)
  exp_scatter_p_distasonis

p_distasonis_expansion = p_distasonis_exp_scatter[p_distasonis_exp_scatter$nu > 1.0, ]
p_distasonis_contraction = p_distasonis_exp_scatter[p_distasonis_exp_scatter$nu <= 1.0, ]

p_distasonis_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = p_distasonis_expansion, bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = p_distasonis_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('P. distasonis histogram of likelihoods')
p_distasonis_hist

p_distasonis_downsampled_empirical = c(10253.18489341513,
                                       5326.520737228057,
                                       3924.694937012874,
                                       3321.958570197894,
                                       2970.095360152478,
                                       2663.637872179951,
                                       2392.125470912756,
                                       2189.947401730031,
                                       2071.812959514204,
                                       1015.051256310989)

p_distasonis_downsampled_red = fold_sfs(c(7077.17815103975,
                                          4726.656795917924,
                                          3423.1328638052682,
                                          2637.6250607421644,
                                          2129.45750111715,
                                          1780.0818044961181,
                                          1527.400302135002,
                                          1336.951929919985,
                                          1188.5425471222807,
                                          1069.729660523428,
                                          972.4935085410946,
                                          891.4557973258138,
                                          822.8832225432012,
                                          764.106106077283,
                                          713.1657676828243,
                                          668.5929265692381,
                                          629.2639375745741,
                                          594.3048331458824,
                                          563.0256333394243))

p_distasonis_downsampled_orange = fold_sfs(c(4449.654131052629,
                                             3652.8866688511757,
                                             3042.074440063343,
                                             2568.5691269367353,
                                             2197.985023907023,
                                             1905.063736777304,
                                             1671.0984264156718,
                                             1482.1761127747545,
                                             1327.9114641514925,
                                             1200.522083540078,
                                             1094.150174932085,
                                             1004.3640328005865,
                                             927.7912979410152,
                                             861.848898930354,
                                             804.5439696858973,
                                             754.3268777067542,
                                             709.9825176381233,
                                             670.5497121868409,
                                             635.2612712292624))

p_distasonis_downsampled_violet = fold_sfs(c(7015.378947471712,
                                             4714.614313079614,
                                             3421.498263322795,
                                             2638.540153376612,
                                             2130.903155622198,
                                             1781.5232462632164,
                                             1528.7138384333805,
                                             1338.1267394879326,
                                             1189.5950267851217,
                                             1070.6794907741141,
                                             973.3577958861454,
                                             892.2483035727298,
                                             823.6148375615049,
                                             764.7854826640892,
                                             713.7998577436606,
                                             669.1873873043774,
                                             629.823430329617,
                                             594.8332430321193,
                                             563.5262321902541))

p_distasonis_downsampled_one_epoch = fold_sfs(c(10370.627948469039,
                                                5185.314960885411,
                                                3456.8767197052484,
                                                2592.6575919298302,
                                                2074.126112410343,
                                                1728.4384573144491,
                                                1481.5187019742768,
                                                1296.3288840141815,
                                                1152.2923576558376,
                                                1037.0631354268637,
                                                942.7846798420104,
                                                864.219299243238,
                                                797.7408994050211,
                                                740.7594130229054,
                                                691.375457406692,
                                                648.164495538564,
                                                610.037175581179,
                                                576.1462238831828,
                                                545.8227401934395))

p_distasonis_downsampled_two_epoch = fold_sfs(c(6928.279335504198,
                                                4689.46319955322,
                                                3416.6003600771573,
                                                2639.874501783247,
                                                2133.875848052315,
                                                1784.6932438175231,
                                                1531.6759559795496,
                                                1340.8033169923415,
                                                1192.0028890612432,
                                                1072.8560569470897,
                                                975.339551782675,
                                                894.0658697998036,
                                                825.2928794526885,
                                                766.3437476426332,
                                                715.2542611351579,
                                                670.5508962940819,
                                                631.1067342710417,
                                                596.0452525974625,
                                                564.6744518323502))

p_distasonis_downsampled_x_axis = 1:length(p_distasonis_downsampled_empirical)

p_distasonis_downsampled_df = data.frame(p_distasonis_downsampled_empirical,
                                         p_distasonis_downsampled_red,
                                         p_distasonis_downsampled_orange,
                                         p_distasonis_downsampled_green,
                                         p_distasonis_downsampled_x_axis,
                                         p_distasonis_downsampled_one_epoch,
                                         p_distasonis_downsampled_two_epoch)


names(p_distasonis_downsampled_df) = c('Empirical',
                                       'Red',
                                       'Orange',
                                       'green',
                                       'x_axis',
                                       'One epoch',
                                       'Two epoch')

p_p_distasonis_downsampled_comparison <- ggplot(data = melt(p_distasonis_downsampled_df, id='x_axis'),
                                                aes(x=x_axis, 
                                                    y=value,
                                                    fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Parameter Estimate") +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_downsampled_x_axis, limits=c(0.5, length(p_distasonis_downsampled_x_axis) + 0.5)) +
  ggtitle('P. distasonis (Downsampled, Raw Counts)') +
  ylab('Proportional Frequency') +
  scale_fill_manual(values = c('black', 'red', 'orange', 'green', 'blue', 'violet')) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p_p_distasonis_downsampled_comparison

p_distasonis_downsampled_empirical = proportional_sfs(c(10253.18489341513,
                                       5326.520737228057,
                                       3924.694937012874,
                                       3321.958570197894,
                                       2970.095360152478,
                                       2663.637872179951,
                                       2392.125470912756,
                                       2189.947401730031,
                                       2071.812959514204,
                                       1015.051256310989))

p_distasonis_downsampled_red = proportional_sfs(fold_sfs(c(7077.17815103975,
                                          4726.656795917924,
                                          3423.1328638052682,
                                          2637.6250607421644,
                                          2129.45750111715,
                                          1780.0818044961181,
                                          1527.400302135002,
                                          1336.951929919985,
                                          1188.5425471222807,
                                          1069.729660523428,
                                          972.4935085410946,
                                          891.4557973258138,
                                          822.8832225432012,
                                          764.106106077283,
                                          713.1657676828243,
                                          668.5929265692381,
                                          629.2639375745741,
                                          594.3048331458824,
                                          563.0256333394243)))

p_distasonis_downsampled_orange = proportional_sfs(fold_sfs(c(4449.654131052629,
                                             3652.8866688511757,
                                             3042.074440063343,
                                             2568.5691269367353,
                                             2197.985023907023,
                                             1905.063736777304,
                                             1671.0984264156718,
                                             1482.1761127747545,
                                             1327.9114641514925,
                                             1200.522083540078,
                                             1094.150174932085,
                                             1004.3640328005865,
                                             927.7912979410152,
                                             861.848898930354,
                                             804.5439696858973,
                                             754.3268777067542,
                                             709.9825176381233,
                                             670.5497121868409,
                                             635.2612712292624)))

p_distasonis_downsampled_violet = proportional_sfs(fold_sfs(c(7015.378947471712,
                                             4714.614313079614,
                                             3421.498263322795,
                                             2638.540153376612,
                                             2130.903155622198,
                                             1781.5232462632164,
                                             1528.7138384333805,
                                             1338.1267394879326,
                                             1189.5950267851217,
                                             1070.6794907741141,
                                             973.3577958861454,
                                             892.2483035727298,
                                             823.6148375615049,
                                             764.7854826640892,
                                             713.7998577436606,
                                             669.1873873043774,
                                             629.823430329617,
                                             594.8332430321193,
                                             563.5262321902541)))

p_distasonis_downsampled_one_epoch = proportional_sfs(fold_sfs(c(10370.627948469039,
                                                5185.314960885411,
                                                3456.8767197052484,
                                                2592.6575919298302,
                                                2074.126112410343,
                                                1728.4384573144491,
                                                1481.5187019742768,
                                                1296.3288840141815,
                                                1152.2923576558376,
                                                1037.0631354268637,
                                                942.7846798420104,
                                                864.219299243238,
                                                797.7408994050211,
                                                740.7594130229054,
                                                691.375457406692,
                                                648.164495538564,
                                                610.037175581179,
                                                576.1462238831828,
                                                545.8227401934395)))

p_distasonis_downsampled_two_epoch = proportional_sfs(fold_sfs(c(6928.279335504198,
                                                4689.46319955322,
                                                3416.6003600771573,
                                                2639.874501783247,
                                                2133.875848052315,
                                                1784.6932438175231,
                                                1531.6759559795496,
                                                1340.8033169923415,
                                                1192.0028890612432,
                                                1072.8560569470897,
                                                975.339551782675,
                                                894.0658697998036,
                                                825.2928794526885,
                                                766.3437476426332,
                                                715.2542611351579,
                                                670.5508962940819,
                                                631.1067342710417,
                                                596.0452525974625,
                                                564.6744518323502)))

p_distasonis_downsampled_x_axis = 1:length(p_distasonis_downsampled_empirical)

p_distasonis_downsampled_df = data.frame(p_distasonis_downsampled_empirical,
                                         p_distasonis_downsampled_red,
                                         p_distasonis_downsampled_orange,
                                         p_distasonis_downsampled_green,
                                         p_distasonis_downsampled_x_axis,
                                         p_distasonis_downsampled_one_epoch,
                                         p_distasonis_downsampled_two_epoch)


names(p_distasonis_downsampled_df) = c('Empirical',
                                       'Red',
                                       'Orange',
                                       'green',
                                       'x_axis',
                                       'One epoch',
                                       'Two epoch')

p_p_distasonis_downsampled_comparison <- ggplot(data = melt(p_distasonis_downsampled_df, id='x_axis'),
                                                aes(x=x_axis, 
                                                    y=value,
                                                    fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "Parameter Estimate") +
  scale_x_continuous(name='Frequency in Sample', breaks=p_distasonis_downsampled_x_axis, limits=c(0.5, length(p_distasonis_downsampled_x_axis) + 0.5)) +
  ggtitle('P. distasonis (Downsampled, Proportional SFS)') +
  ylab('Proportional Frequency') +
  scale_fill_manual(values = c('black', 'red', 'orange', 'green', 'blue', 'violet')) +
  theme(panel.border = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p_p_distasonis_downsampled_comparison

input_csv_5 = read.csv('temp_output/b_intestinihominis_scatter.txt', header=FALSE)
names(input_csv_5) = c('likelihood', 'nu', 'tau')

b_intestinihominis_exp_scatter = read.csv('temp_output/b_intestinihominis_expansion_scatter.txt', header=FALSE)
names(b_intestinihominis_exp_scatter) = c('likelihood', 'nu', 'tau')

b_intestinihominis_all = rbind(input_csv_5, b_intestinihominis_exp_scatter)

exp_scatter_b_intestinihominis_all = ggplot(data=b_intestinihominis_all, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('P. distasonis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_b_intestinihominis_all

b_intestinihominis_all_expansion = b_intestinihominis_all[b_intestinihominis_all$nu > 1.0, ]
b_intestinihominis_all_contraction = b_intestinihominis_all[b_intestinihominis_all$nu <= 1.0, ]

b_intestinihominis_all_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = b_intestinihominis_all_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = b_intestinihominis_all_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('B. intestinihominis histogram of likelihoods')
b_intestinihominis_all_hist

b_intestinihominis_all[b_intestinihominis_all$likelihood < -75, ]$likelihood = -75

exp_scatter_b_intestinihominis_all = ggplot(data=b_intestinihominis_all, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('B. intestinihominis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_b_intestinihominis_all

input_csv_4 = read.csv('temp_output/b_xylanisolvens_scatter.txt', header=FALSE)
names(input_csv_4) = c('likelihood', 'nu', 'tau')

b_xylanisolvens_exp_scatter = read.csv('temp_output/b_xylanisolvens_expansion_scatter.txt', header=FALSE)
names(b_xylanisolvens_exp_scatter) = c('likelihood', 'nu', 'tau')

b_xylanisolvens_all = rbind(input_csv_4, b_xylanisolvens_exp_scatter)

b_xylanisolvens_all_expansion = b_xylanisolvens_all[b_xylanisolvens_all$nu > 1.0, ]
b_xylanisolvens_all_contraction = b_xylanisolvens_all[b_xylanisolvens_all$nu <= 1.0, ]

b_xylanisolvens_all_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = b_xylanisolvens_all_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = b_xylanisolvens_all_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('B. xylanisolvens histogram of likelihoods')
b_xylanisolvens_all_hist

b_xylanisolvens_all[b_xylanisolvens_all$likelihood < -55, ]$likelihood = -55

exp_scatter_b_xylanisolvens_all = ggplot(data=b_xylanisolvens_all, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('B. xylanisolvens rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
exp_scatter_b_xylanisolvens_all

