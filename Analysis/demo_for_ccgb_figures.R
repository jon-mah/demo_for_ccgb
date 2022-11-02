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
  if (input_length %% 2 == 0) {
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

# Oral Data

actinomyces_sp_masked_surface = read.csv('oral_likelihood_surfaces/actinomyces_sp_masked.csv', header=FALSE)
names(actinomyces_sp_masked_surface) = c('likelihood', 'nu', 'tau')

actinomyces_sp_masked_surface_expansion = actinomyces_sp_masked_surface[actinomyces_sp_masked_surface$nu > 1.0, ]
actinomyces_sp_masked_surface_contraction = actinomyces_sp_masked_surface[actinomyces_sp_masked_surface$nu <= 1.0, ]

actinomyces_sp_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = actinomyces_sp_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = actinomyces_sp_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Actinomyces sp. histogram of likelihoods [singletons masked]')
actinomyces_sp_masked_surface_hist

actinomyces_sp_masked_surface_cutoff = quantile(actinomyces_sp_masked_surface$likelihood, 0.80)

actinomyces_sp_masked_surface[actinomyces_sp_masked_surface$likelihood < actinomyces_sp_masked_surface_cutoff, ]$likelihood = actinomyces_sp_masked_surface_cutoff

actinomyces_sp_masked_surface_scatter = ggplot(data=actinomyces_sp_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Actinomyces sp. rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
actinomyces_sp_masked_surface_scatter

actinomyces_sp_unmasked_surface = read.csv('oral_likelihood_surfaces/actinomyces_sp_unmasked.csv', header=FALSE)
names(actinomyces_sp_unmasked_surface) = c('likelihood', 'nu', 'tau')

actinomyces_sp_unmasked_surface_expansion = actinomyces_sp_unmasked_surface[actinomyces_sp_unmasked_surface$nu > 1.0, ]
actinomyces_sp_unmasked_surface_contraction = actinomyces_sp_unmasked_surface[actinomyces_sp_unmasked_surface$nu <= 1.0, ]

actinomyces_sp_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = actinomyces_sp_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = actinomyces_sp_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Actinomyces sp. histogram of likelihoods')
actinomyces_sp_unmasked_surface_hist

actinomyces_sp_unmasked_surface_cutoff = quantile(actinomyces_sp_unmasked_surface$likelihood, 0.80)

actinomyces_sp_unmasked_surface[actinomyces_sp_unmasked_surface$likelihood < actinomyces_sp_unmasked_surface_cutoff, ]$likelihood = actinomyces_sp_unmasked_surface_cutoff

actinomyces_sp_unmasked_surface_scatter = ggplot(data=actinomyces_sp_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Actinomyces sp. rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
actinomyces_sp_unmasked_surface_scatter

aggregatibacter_sp_masked_surface = read.csv('oral_likelihood_surfaces/aggregatibacter_sp_masked.csv', header=FALSE)
names(aggregatibacter_sp_masked_surface) = c('likelihood', 'nu', 'tau')

aggregatibacter_sp_masked_surface_expansion = aggregatibacter_sp_masked_surface[aggregatibacter_sp_masked_surface$nu > 1.0, ]
aggregatibacter_sp_masked_surface_contraction = aggregatibacter_sp_masked_surface[aggregatibacter_sp_masked_surface$nu <= 1.0, ]

aggregatibacter_sp_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = aggregatibacter_sp_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = aggregatibacter_sp_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Aggregatibacter sp. histogram of likelihoods [singletons masked]')
aggregatibacter_sp_masked_surface_hist

aggregatibacter_sp_masked_surface_cutoff = quantile(aggregatibacter_sp_masked_surface$likelihood, 0.80)

aggregatibacter_sp_masked_surface[aggregatibacter_sp_masked_surface$likelihood < aggregatibacter_sp_masked_surface_cutoff, ]$likelihood = aggregatibacter_sp_masked_surface_cutoff

aggregatibacter_sp_masked_surface_scatter = ggplot(data=aggregatibacter_sp_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Aggregatibacter sp. rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
aggregatibacter_sp_masked_surface_scatter

aggregatibacter_sp_unmasked_surface = read.csv('oral_likelihood_surfaces/aggregatibacter_sp_unmasked.csv', header=FALSE)
names(aggregatibacter_sp_unmasked_surface) = c('likelihood', 'nu', 'tau')

aggregatibacter_sp_unmasked_surface_expansion = aggregatibacter_sp_unmasked_surface[aggregatibacter_sp_unmasked_surface$nu > 1.0, ]
aggregatibacter_sp_unmasked_surface_contraction = aggregatibacter_sp_unmasked_surface[aggregatibacter_sp_unmasked_surface$nu <= 1.0, ]

aggregatibacter_sp_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = aggregatibacter_sp_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = aggregatibacter_sp_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Aggregatibacter sp. histogram of likelihoods')
aggregatibacter_sp_unmasked_surface_hist

aggregatibacter_sp_unmasked_surface_cutoff = quantile(aggregatibacter_sp_unmasked_surface$likelihood, 0.80)

aggregatibacter_sp_unmasked_surface[aggregatibacter_sp_unmasked_surface$likelihood < aggregatibacter_sp_unmasked_surface_cutoff, ]$likelihood = aggregatibacter_sp_unmasked_surface_cutoff

aggregatibacter_sp_unmasked_surface_scatter = ggplot(data=aggregatibacter_sp_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Aggregatibacter sp. rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
aggregatibacter_sp_unmasked_surface_scatter

capnocytophaga_gingivalis_masked_surface = read.csv('oral_likelihood_surfaces/capnocytophaga_gingivalis_masked.csv', header=FALSE)
names(capnocytophaga_gingivalis_masked_surface) = c('likelihood', 'nu', 'tau')

capnocytophaga_gingivalis_masked_surface_expansion = capnocytophaga_gingivalis_masked_surface[capnocytophaga_gingivalis_masked_surface$nu > 1.0, ]
capnocytophaga_gingivalis_masked_surface_contraction = capnocytophaga_gingivalis_masked_surface[capnocytophaga_gingivalis_masked_surface$nu <= 1.0, ]

capnocytophaga_gingivalis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = capnocytophaga_gingivalis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = capnocytophaga_gingivalis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Capnocytophaga gingivalis histogram of likelihoods [singletons masked]')
capnocytophaga_gingivalis_masked_surface_hist

capnocytophaga_gingivalis_masked_surface_cutoff = quantile(capnocytophaga_gingivalis_masked_surface$likelihood, 0.80)

capnocytophaga_gingivalis_masked_surface[capnocytophaga_gingivalis_masked_surface$likelihood < capnocytophaga_gingivalis_masked_surface_cutoff, ]$likelihood = capnocytophaga_gingivalis_masked_surface_cutoff

capnocytophaga_gingivalis_masked_surface_scatter = ggplot(data=capnocytophaga_gingivalis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Capnocytophaga gingivalis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
capnocytophaga_gingivalis_masked_surface_scatter

capnocytophaga_gingivalis_unmasked_surface = read.csv('oral_likelihood_surfaces/capnocytophaga_gingivalis_unmasked.csv', header=FALSE)
names(capnocytophaga_gingivalis_unmasked_surface) = c('likelihood', 'nu', 'tau')

capnocytophaga_gingivalis_unmasked_surface_expansion = capnocytophaga_gingivalis_unmasked_surface[capnocytophaga_gingivalis_unmasked_surface$nu > 1.0, ]
capnocytophaga_gingivalis_unmasked_surface_contraction = capnocytophaga_gingivalis_unmasked_surface[capnocytophaga_gingivalis_unmasked_surface$nu <= 1.0, ]

capnocytophaga_gingivalis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = capnocytophaga_gingivalis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = capnocytophaga_gingivalis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Capnocytophaga gingivalis histogram of likelihoods')
capnocytophaga_gingivalis_unmasked_surface_hist

capnocytophaga_gingivalis_unmasked_surface_cutoff = quantile(capnocytophaga_gingivalis_unmasked_surface$likelihood, 0.80)

capnocytophaga_gingivalis_unmasked_surface[capnocytophaga_gingivalis_unmasked_surface$likelihood < capnocytophaga_gingivalis_unmasked_surface_cutoff, ]$likelihood = capnocytophaga_gingivalis_unmasked_surface_cutoff

capnocytophaga_gingivalis_unmasked_surface_scatter = ggplot(data=capnocytophaga_gingivalis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Capnocytophaga gingivalis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
capnocytophaga_gingivalis_unmasked_surface_scatter

capnocytophaga_sputigena_masked_surface = read.csv('oral_likelihood_surfaces/capnocytophaga_sputigena_masked.csv', header=FALSE)
names(capnocytophaga_sputigena_masked_surface) = c('likelihood', 'nu', 'tau')

capnocytophaga_sputigena_masked_surface_expansion = capnocytophaga_sputigena_masked_surface[capnocytophaga_sputigena_masked_surface$nu > 1.0, ]
capnocytophaga_sputigena_masked_surface_contraction = capnocytophaga_sputigena_masked_surface[capnocytophaga_sputigena_masked_surface$nu <= 1.0, ]

capnocytophaga_sputigena_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = capnocytophaga_sputigena_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = capnocytophaga_sputigena_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Capnocytophaga sputigena histogram of likelihoods [singletons masked]')
capnocytophaga_sputigena_masked_surface_hist

capnocytophaga_sputigena_masked_surface_cutoff = quantile(capnocytophaga_sputigena_masked_surface$likelihood, 0.80)

capnocytophaga_sputigena_masked_surface[capnocytophaga_sputigena_masked_surface$likelihood < capnocytophaga_sputigena_masked_surface_cutoff, ]$likelihood = capnocytophaga_sputigena_masked_surface_cutoff

capnocytophaga_sputigena_masked_surface_scatter = ggplot(data=capnocytophaga_sputigena_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Capnocytophaga sputigena rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
capnocytophaga_sputigena_masked_surface_scatter

capnocytophaga_sputigena_unmasked_surface = read.csv('oral_likelihood_surfaces/capnocytophaga_sputigena_unmasked.csv', header=FALSE)
names(capnocytophaga_sputigena_unmasked_surface) = c('likelihood', 'nu', 'tau')

capnocytophaga_sputigena_unmasked_surface_expansion = capnocytophaga_sputigena_unmasked_surface[capnocytophaga_sputigena_unmasked_surface$nu > 1.0, ]
capnocytophaga_sputigena_unmasked_surface_contraction = capnocytophaga_sputigena_unmasked_surface[capnocytophaga_sputigena_unmasked_surface$nu <= 1.0, ]

capnocytophaga_sputigena_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = capnocytophaga_sputigena_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = capnocytophaga_sputigena_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Capnocytophaga sputigena histogram of likelihoods')
capnocytophaga_sputigena_unmasked_surface_hist

capnocytophaga_sputigena_unmasked_surface_cutoff = quantile(capnocytophaga_sputigena_unmasked_surface$likelihood, 0.80)

capnocytophaga_sputigena_unmasked_surface[capnocytophaga_sputigena_unmasked_surface$likelihood < capnocytophaga_sputigena_unmasked_surface_cutoff, ]$likelihood = capnocytophaga_sputigena_unmasked_surface_cutoff

capnocytophaga_sputigena_unmasked_surface_scatter = ggplot(data=capnocytophaga_sputigena_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Capnocytophaga sputigena rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
capnocytophaga_sputigena_unmasked_surface_scatter

corynebacterium_matruchotii_masked_surface = read.csv('oral_likelihood_surfaces/corynebacterium_matruchotii_masked.csv', header=FALSE)
names(corynebacterium_matruchotii_masked_surface) = c('likelihood', 'nu', 'tau')

corynebacterium_matruchotii_masked_surface_expansion = corynebacterium_matruchotii_masked_surface[corynebacterium_matruchotii_masked_surface$nu > 1.0, ]
corynebacterium_matruchotii_masked_surface_contraction = corynebacterium_matruchotii_masked_surface[corynebacterium_matruchotii_masked_surface$nu <= 1.0, ]

corynebacterium_matruchotii_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = corynebacterium_matruchotii_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = corynebacterium_matruchotii_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Corynebacterium matruchotii histogram of likelihoods [singletons masked]')
corynebacterium_matruchotii_masked_surface_hist

corynebacterium_matruchotii_masked_surface_cutoff = quantile(corynebacterium_matruchotii_masked_surface$likelihood, 0.80)

corynebacterium_matruchotii_masked_surface[corynebacterium_matruchotii_masked_surface$likelihood < corynebacterium_matruchotii_masked_surface_cutoff, ]$likelihood = corynebacterium_matruchotii_masked_surface_cutoff

corynebacterium_matruchotii_masked_surface_scatter = ggplot(data=corynebacterium_matruchotii_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Corynebacterium matruchotii rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
corynebacterium_matruchotii_masked_surface_scatter

corynebacterium_matruchotii_unmasked_surface = read.csv('oral_likelihood_surfaces/corynebacterium_matruchotii_unmasked.csv', header=FALSE)
names(corynebacterium_matruchotii_unmasked_surface) = c('likelihood', 'nu', 'tau')

corynebacterium_matruchotii_unmasked_surface_expansion = corynebacterium_matruchotii_unmasked_surface[corynebacterium_matruchotii_unmasked_surface$nu > 1.0, ]
corynebacterium_matruchotii_unmasked_surface_contraction = corynebacterium_matruchotii_unmasked_surface[corynebacterium_matruchotii_unmasked_surface$nu <= 1.0, ]

corynebacterium_matruchotii_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = corynebacterium_matruchotii_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = corynebacterium_matruchotii_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Corynebacterium matruchotii histogram of likelihoods')
corynebacterium_matruchotii_unmasked_surface_hist

corynebacterium_matruchotii_unmasked_surface_cutoff = quantile(corynebacterium_matruchotii_unmasked_surface$likelihood, 0.80)

corynebacterium_matruchotii_unmasked_surface[corynebacterium_matruchotii_unmasked_surface$likelihood < corynebacterium_matruchotii_unmasked_surface_cutoff, ]$likelihood = corynebacterium_matruchotii_unmasked_surface_cutoff

corynebacterium_matruchotii_unmasked_surface_scatter = ggplot(data=corynebacterium_matruchotii_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Corynebacterium matruchotii rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
corynebacterium_matruchotii_unmasked_surface_scatter

fusobacterium_nucleatum_masked_surface = read.csv('oral_likelihood_surfaces/fusobacterium_nucleatum_masked.csv', header=FALSE)
names(fusobacterium_nucleatum_masked_surface) = c('likelihood', 'nu', 'tau')

fusobacterium_nucleatum_masked_surface_expansion = fusobacterium_nucleatum_masked_surface[fusobacterium_nucleatum_masked_surface$nu > 1.0, ]
fusobacterium_nucleatum_masked_surface_contraction = fusobacterium_nucleatum_masked_surface[fusobacterium_nucleatum_masked_surface$nu <= 1.0, ]

fusobacterium_nucleatum_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = fusobacterium_nucleatum_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = fusobacterium_nucleatum_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Fusobacterium nucleatum histogram of likelihoods [singletons masked]')
fusobacterium_nucleatum_masked_surface_hist

fusobacterium_nucleatum_masked_surface_cutoff = quantile(fusobacterium_nucleatum_masked_surface$likelihood, 0.80)

fusobacterium_nucleatum_masked_surface[fusobacterium_nucleatum_masked_surface$likelihood < fusobacterium_nucleatum_masked_surface_cutoff, ]$likelihood = fusobacterium_nucleatum_masked_surface_cutoff

fusobacterium_nucleatum_masked_surface_scatter = ggplot(data=fusobacterium_nucleatum_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Fusobacterium nucleatum rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
fusobacterium_nucleatum_masked_surface_scatter

fusobacterium_nucleatum_unmasked_surface = read.csv('oral_likelihood_surfaces/fusobacterium_nucleatum_unmasked.csv', header=FALSE)
names(fusobacterium_nucleatum_unmasked_surface) = c('likelihood', 'nu', 'tau')

fusobacterium_nucleatum_unmasked_surface_expansion = fusobacterium_nucleatum_unmasked_surface[fusobacterium_nucleatum_unmasked_surface$nu > 1.0, ]
fusobacterium_nucleatum_unmasked_surface_contraction = fusobacterium_nucleatum_unmasked_surface[fusobacterium_nucleatum_unmasked_surface$nu <= 1.0, ]

fusobacterium_nucleatum_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = fusobacterium_nucleatum_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = fusobacterium_nucleatum_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Fusobacterium nucleatum histogram of likelihoods')
fusobacterium_nucleatum_unmasked_surface_hist

fusobacterium_nucleatum_unmasked_surface_cutoff = quantile(fusobacterium_nucleatum_unmasked_surface$likelihood, 0.80)

fusobacterium_nucleatum_unmasked_surface[fusobacterium_nucleatum_unmasked_surface$likelihood < fusobacterium_nucleatum_unmasked_surface_cutoff, ]$likelihood = fusobacterium_nucleatum_unmasked_surface_cutoff

fusobacterium_nucleatum_unmasked_surface_scatter = ggplot(data=fusobacterium_nucleatum_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Fusobacterium nucleatum rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
fusobacterium_nucleatum_unmasked_surface_scatter

granulicatella_adiacens_masked_surface = read.csv('oral_likelihood_surfaces/granulicatella_adiacens_masked.csv', header=FALSE)
names(granulicatella_adiacens_masked_surface) = c('likelihood', 'nu', 'tau')

granulicatella_adiacens_masked_surface_expansion = granulicatella_adiacens_masked_surface[granulicatella_adiacens_masked_surface$nu > 1.0, ]
granulicatella_adiacens_masked_surface_contraction = granulicatella_adiacens_masked_surface[granulicatella_adiacens_masked_surface$nu <= 1.0, ]

granulicatella_adiacens_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = granulicatella_adiacens_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = granulicatella_adiacens_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Granulicatella adiacens histogram of likelihoods [singletons masked]')
granulicatella_adiacens_masked_surface_hist

granulicatella_adiacens_masked_surface_cutoff = quantile(granulicatella_adiacens_masked_surface$likelihood, 0.80)

granulicatella_adiacens_masked_surface[granulicatella_adiacens_masked_surface$likelihood < granulicatella_adiacens_masked_surface_cutoff, ]$likelihood = granulicatella_adiacens_masked_surface_cutoff

granulicatella_adiacens_masked_surface_scatter = ggplot(data=granulicatella_adiacens_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Granulicatella adiacens rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
granulicatella_adiacens_masked_surface_scatter

granulicatella_adiacens_unmasked_surface = read.csv('oral_likelihood_surfaces/granulicatella_adiacens_unmasked.csv', header=FALSE)
names(granulicatella_adiacens_unmasked_surface) = c('likelihood', 'nu', 'tau')

granulicatella_adiacens_unmasked_surface_expansion = granulicatella_adiacens_unmasked_surface[granulicatella_adiacens_unmasked_surface$nu > 1.0, ]
granulicatella_adiacens_unmasked_surface_contraction = granulicatella_adiacens_unmasked_surface[granulicatella_adiacens_unmasked_surface$nu <= 1.0, ]

granulicatella_adiacens_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = granulicatella_adiacens_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = granulicatella_adiacens_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Granulicatella adiacens histogram of likelihoods')
granulicatella_adiacens_unmasked_surface_hist

granulicatella_adiacens_unmasked_surface_cutoff = quantile(granulicatella_adiacens_unmasked_surface$likelihood, 0.80)

granulicatella_adiacens_unmasked_surface[granulicatella_adiacens_unmasked_surface$likelihood < granulicatella_adiacens_unmasked_surface_cutoff, ]$likelihood = granulicatella_adiacens_unmasked_surface_cutoff

granulicatella_adiacens_unmasked_surface_scatter = ggplot(data=granulicatella_adiacens_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Granulicatella adiacens rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
granulicatella_adiacens_unmasked_surface_scatter

haemophilus_haemolyticus_58348_masked_surface = read.csv('oral_likelihood_surfaces/haemophilus_haemolyticus_58348_masked.csv', header=FALSE)
names(haemophilus_haemolyticus_58348_masked_surface) = c('likelihood', 'nu', 'tau')

haemophilus_haemolyticus_58348_masked_surface_expansion = haemophilus_haemolyticus_58348_masked_surface[haemophilus_haemolyticus_58348_masked_surface$nu > 1.0, ]
haemophilus_haemolyticus_58348_masked_surface_contraction = haemophilus_haemolyticus_58348_masked_surface[haemophilus_haemolyticus_58348_masked_surface$nu <= 1.0, ]

haemophilus_haemolyticus_58348_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = haemophilus_haemolyticus_58348_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = haemophilus_haemolyticus_58348_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Haemophilus haemolyticus 58348 histogram of likelihoods [singletons masked]')
haemophilus_haemolyticus_58348_masked_surface_hist

haemophilus_haemolyticus_58348_masked_surface_cutoff = quantile(haemophilus_haemolyticus_58348_masked_surface$likelihood, 0.80)

haemophilus_haemolyticus_58348_masked_surface[haemophilus_haemolyticus_58348_masked_surface$likelihood < haemophilus_haemolyticus_58348_masked_surface_cutoff, ]$likelihood = haemophilus_haemolyticus_58348_masked_surface_cutoff

haemophilus_haemolyticus_58348_masked_surface_scatter = ggplot(data=haemophilus_haemolyticus_58348_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Haemophilus haemolyticus 58348 rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
haemophilus_haemolyticus_58348_masked_surface_scatter

haemophilus_haemolyticus_58348_unmasked_surface = read.csv('oral_likelihood_surfaces/haemophilus_haemolyticus_58348_unmasked.csv', header=FALSE)
names(haemophilus_haemolyticus_58348_unmasked_surface) = c('likelihood', 'nu', 'tau')

haemophilus_haemolyticus_58348_unmasked_surface_expansion = haemophilus_haemolyticus_58348_unmasked_surface[haemophilus_haemolyticus_58348_unmasked_surface$nu > 1.0, ]
haemophilus_haemolyticus_58348_unmasked_surface_contraction = haemophilus_haemolyticus_58348_unmasked_surface[haemophilus_haemolyticus_58348_unmasked_surface$nu <= 1.0, ]

haemophilus_haemolyticus_58348_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = haemophilus_haemolyticus_58348_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = haemophilus_haemolyticus_58348_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Haemophilus haemolyticus 58348 histogram of likelihoods')
haemophilus_haemolyticus_58348_unmasked_surface_hist

haemophilus_haemolyticus_58348_unmasked_surface_cutoff = quantile(haemophilus_haemolyticus_58348_unmasked_surface$likelihood, 0.80)

haemophilus_haemolyticus_58348_unmasked_surface[haemophilus_haemolyticus_58348_unmasked_surface$likelihood < haemophilus_haemolyticus_58348_unmasked_surface_cutoff, ]$likelihood = haemophilus_haemolyticus_58348_unmasked_surface_cutoff

haemophilus_haemolyticus_58348_unmasked_surface_scatter = ggplot(data=haemophilus_haemolyticus_58348_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Haemophilus haemolyticus 58348 rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
haemophilus_haemolyticus_58348_unmasked_surface_scatter

haemophilus_haemolyticus_58350_masked_surface = read.csv('oral_likelihood_surfaces/haemophilus_haemolyticus_58350_masked.csv', header=FALSE)
names(haemophilus_haemolyticus_58350_masked_surface) = c('likelihood', 'nu', 'tau')

haemophilus_haemolyticus_58350_masked_surface_expansion = haemophilus_haemolyticus_58350_masked_surface[haemophilus_haemolyticus_58350_masked_surface$nu > 1.0, ]
haemophilus_haemolyticus_58350_masked_surface_contraction = haemophilus_haemolyticus_58350_masked_surface[haemophilus_haemolyticus_58350_masked_surface$nu <= 1.0, ]

haemophilus_haemolyticus_58350_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = haemophilus_haemolyticus_58350_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = haemophilus_haemolyticus_58350_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Haemophilus haemolyticus 58350 histogram of likelihoods [singletons masked]')
haemophilus_haemolyticus_58350_masked_surface_hist

haemophilus_haemolyticus_58350_masked_surface_cutoff = quantile(haemophilus_haemolyticus_58350_masked_surface$likelihood, 0.80)

haemophilus_haemolyticus_58350_masked_surface[haemophilus_haemolyticus_58350_masked_surface$likelihood < haemophilus_haemolyticus_58350_masked_surface_cutoff, ]$likelihood = haemophilus_haemolyticus_58350_masked_surface_cutoff

haemophilus_haemolyticus_58350_masked_surface_scatter = ggplot(data=haemophilus_haemolyticus_58350_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Haemophilus haemolyticus 58350 rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
haemophilus_haemolyticus_58350_masked_surface_scatter

haemophilus_haemolyticus_58350_unmasked_surface = read.csv('oral_likelihood_surfaces/haemophilus_haemolyticus_58350_unmasked.csv', header=FALSE)
names(haemophilus_haemolyticus_58350_unmasked_surface) = c('likelihood', 'nu', 'tau')

haemophilus_haemolyticus_58350_unmasked_surface_expansion = haemophilus_haemolyticus_58350_unmasked_surface[haemophilus_haemolyticus_58350_unmasked_surface$nu > 1.0, ]
haemophilus_haemolyticus_58350_unmasked_surface_contraction = haemophilus_haemolyticus_58350_unmasked_surface[haemophilus_haemolyticus_58350_unmasked_surface$nu <= 1.0, ]

haemophilus_haemolyticus_58350_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = haemophilus_haemolyticus_58350_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = haemophilus_haemolyticus_58350_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Haemophilus haemolyticus 58350 histogram of likelihoods')
haemophilus_haemolyticus_58350_unmasked_surface_hist

haemophilus_haemolyticus_58350_unmasked_surface_cutoff = quantile(haemophilus_haemolyticus_58350_unmasked_surface$likelihood, 0.80)

haemophilus_haemolyticus_58350_unmasked_surface[haemophilus_haemolyticus_58350_unmasked_surface$likelihood < haemophilus_haemolyticus_58350_unmasked_surface_cutoff, ]$likelihood = haemophilus_haemolyticus_58350_unmasked_surface_cutoff

haemophilus_haemolyticus_58350_unmasked_surface_scatter = ggplot(data=haemophilus_haemolyticus_58350_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Haemophilus haemolyticus 58350 rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
haemophilus_haemolyticus_58350_unmasked_surface_scatter

kingella_oralis_masked_surface = read.csv('oral_likelihood_surfaces/kingella_oralis_masked.csv', header=FALSE)
names(kingella_oralis_masked_surface) = c('likelihood', 'nu', 'tau')

kingella_oralis_masked_surface_expansion = kingella_oralis_masked_surface[kingella_oralis_masked_surface$nu > 1.0, ]
kingella_oralis_masked_surface_contraction = kingella_oralis_masked_surface[kingella_oralis_masked_surface$nu <= 1.0, ]

kingella_oralis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = kingella_oralis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = kingella_oralis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Kingella oralis histogram of likelihoods [singletons masked]')
kingella_oralis_masked_surface_hist

kingella_oralis_masked_surface_cutoff = quantile(kingella_oralis_masked_surface$likelihood, 0.80)

kingella_oralis_masked_surface[kingella_oralis_masked_surface$likelihood < kingella_oralis_masked_surface_cutoff, ]$likelihood = kingella_oralis_masked_surface_cutoff

kingella_oralis_masked_surface_scatter = ggplot(data=kingella_oralis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Kingella oralis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
kingella_oralis_masked_surface_scatter

kingella_oralis_unmasked_surface = read.csv('oral_likelihood_surfaces/kingella_oralis_unmasked.csv', header=FALSE)
names(kingella_oralis_unmasked_surface) = c('likelihood', 'nu', 'tau')

kingella_oralis_unmasked_surface_expansion = kingella_oralis_unmasked_surface[kingella_oralis_unmasked_surface$nu > 1.0, ]
kingella_oralis_unmasked_surface_contraction = kingella_oralis_unmasked_surface[kingella_oralis_unmasked_surface$nu <= 1.0, ]

kingella_oralis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = kingella_oralis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = kingella_oralis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Kingella oralis histogram of likelihoods')
kingella_oralis_unmasked_surface_hist

kingella_oralis_unmasked_surface_cutoff = quantile(kingella_oralis_unmasked_surface$likelihood, 0.80)

kingella_oralis_unmasked_surface[kingella_oralis_unmasked_surface$likelihood < kingella_oralis_unmasked_surface_cutoff, ]$likelihood = kingella_oralis_unmasked_surface_cutoff

kingella_oralis_unmasked_surface_scatter = ggplot(data=kingella_oralis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Kingella oralis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
kingella_oralis_unmasked_surface_scatter

lautropia_mirabilis_masked_surface = read.csv('oral_likelihood_surfaces/lautropia_mirabilis_masked.csv', header=FALSE)
names(lautropia_mirabilis_masked_surface) = c('likelihood', 'nu', 'tau')

lautropia_mirabilis_masked_surface_expansion = lautropia_mirabilis_masked_surface[lautropia_mirabilis_masked_surface$nu > 1.0, ]
lautropia_mirabilis_masked_surface_contraction = lautropia_mirabilis_masked_surface[lautropia_mirabilis_masked_surface$nu <= 1.0, ]

lautropia_mirabilis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = lautropia_mirabilis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = lautropia_mirabilis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Lautropia mirabilis histogram of likelihoods [singletons masked]')
lautropia_mirabilis_masked_surface_hist

lautropia_mirabilis_masked_surface_cutoff = quantile(lautropia_mirabilis_masked_surface$likelihood, 0.80)

lautropia_mirabilis_masked_surface[lautropia_mirabilis_masked_surface$likelihood < lautropia_mirabilis_masked_surface_cutoff, ]$likelihood = lautropia_mirabilis_masked_surface_cutoff

lautropia_mirabilis_masked_surface_scatter = ggplot(data=lautropia_mirabilis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Lautropia mirabilis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
lautropia_mirabilis_masked_surface_scatter

lautropia_mirabilis_unmasked_surface = read.csv('oral_likelihood_surfaces/lautropia_mirabilis_unmasked.csv', header=FALSE)
names(lautropia_mirabilis_unmasked_surface) = c('likelihood', 'nu', 'tau')

lautropia_mirabilis_unmasked_surface_expansion = lautropia_mirabilis_unmasked_surface[lautropia_mirabilis_unmasked_surface$nu > 1.0, ]
lautropia_mirabilis_unmasked_surface_contraction = lautropia_mirabilis_unmasked_surface[lautropia_mirabilis_unmasked_surface$nu <= 1.0, ]

lautropia_mirabilis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = lautropia_mirabilis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = lautropia_mirabilis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Lautropia mirabilis histogram of likelihoods')
lautropia_mirabilis_unmasked_surface_hist

lautropia_mirabilis_unmasked_surface_cutoff = quantile(lautropia_mirabilis_unmasked_surface$likelihood, 0.80)

lautropia_mirabilis_unmasked_surface[lautropia_mirabilis_unmasked_surface$likelihood < lautropia_mirabilis_unmasked_surface_cutoff, ]$likelihood = lautropia_mirabilis_unmasked_surface_cutoff

lautropia_mirabilis_unmasked_surface_scatter = ggplot(data=lautropia_mirabilis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Lautropia mirabilis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
lautropia_mirabilis_unmasked_surface_scatter

neisseria_cinerea_masked_surface = read.csv('oral_likelihood_surfaces/neisseria_cinerea_masked.csv', header=FALSE)
names(neisseria_cinerea_masked_surface) = c('likelihood', 'nu', 'tau')

neisseria_cinerea_masked_surface_expansion = neisseria_cinerea_masked_surface[neisseria_cinerea_masked_surface$nu > 1.0, ]
neisseria_cinerea_masked_surface_contraction = neisseria_cinerea_masked_surface[neisseria_cinerea_masked_surface$nu <= 1.0, ]

neisseria_cinerea_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = neisseria_cinerea_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = neisseria_cinerea_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Neisseria cinerea histogram of likelihoods [singletons masked]')
neisseria_cinerea_masked_surface_hist

neisseria_cinerea_masked_surface_cutoff = quantile(neisseria_cinerea_masked_surface$likelihood, 0.80)

neisseria_cinerea_masked_surface[neisseria_cinerea_masked_surface$likelihood < neisseria_cinerea_masked_surface_cutoff, ]$likelihood = neisseria_cinerea_masked_surface_cutoff

neisseria_cinerea_masked_surface_scatter = ggplot(data=neisseria_cinerea_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Neisseria cinerea rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
neisseria_cinerea_masked_surface_scatter

neisseria_cinerea_unmasked_surface = read.csv('oral_likelihood_surfaces/neisseria_cinerea_unmasked.csv', header=FALSE)
names(neisseria_cinerea_unmasked_surface) = c('likelihood', 'nu', 'tau')

neisseria_cinerea_unmasked_surface_expansion = neisseria_cinerea_unmasked_surface[neisseria_cinerea_unmasked_surface$nu > 1.0, ]
neisseria_cinerea_unmasked_surface_contraction = neisseria_cinerea_unmasked_surface[neisseria_cinerea_unmasked_surface$nu <= 1.0, ]

neisseria_cinerea_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = neisseria_cinerea_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = neisseria_cinerea_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Neisseria cinerea histogram of likelihoods')
neisseria_cinerea_unmasked_surface_hist

neisseria_cinerea_unmasked_surface_cutoff = quantile(neisseria_cinerea_unmasked_surface$likelihood, 0.80)

neisseria_cinerea_unmasked_surface[neisseria_cinerea_unmasked_surface$likelihood < neisseria_cinerea_unmasked_surface_cutoff, ]$likelihood = neisseria_cinerea_unmasked_surface_cutoff

neisseria_cinerea_unmasked_surface_scatter = ggplot(data=neisseria_cinerea_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Neisseria cinerea rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
neisseria_cinerea_unmasked_surface_scatter

neisseria_elongata_masked_surface = read.csv('oral_likelihood_surfaces/neisseria_elongata_masked.csv', header=FALSE)
names(neisseria_elongata_masked_surface) = c('likelihood', 'nu', 'tau')

neisseria_elongata_masked_surface_expansion = neisseria_elongata_masked_surface[neisseria_elongata_masked_surface$nu > 2.0, ]
neisseria_elongata_masked_surface_contraction = neisseria_elongata_masked_surface[neisseria_elongata_masked_surface$nu <= 2.0, ]

neisseria_elongata_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = neisseria_elongata_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = neisseria_elongata_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Neisseria elongata histogram of likelihoods [singletons masked]')
neisseria_elongata_masked_surface_hist

neisseria_elongata_masked_surface_cutoff = quantile(neisseria_elongata_masked_surface$likelihood, 0.80)

neisseria_elongata_masked_surface[neisseria_elongata_masked_surface$likelihood < neisseria_elongata_masked_surface_cutoff, ]$likelihood = neisseria_elongata_masked_surface_cutoff

neisseria_elongata_masked_surface_scatter = ggplot(data=neisseria_elongata_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Neisseria elongata rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
neisseria_elongata_masked_surface_scatter

neisseria_elongata_unmasked_surface = read.csv('oral_likelihood_surfaces/neisseria_elongata_unmasked.csv', header=FALSE)
names(neisseria_elongata_unmasked_surface) = c('likelihood', 'nu', 'tau')

neisseria_elongata_unmasked_surface_expansion = neisseria_elongata_unmasked_surface[neisseria_elongata_unmasked_surface$nu > 1.0, ]
neisseria_elongata_unmasked_surface_contraction = neisseria_elongata_unmasked_surface[neisseria_elongata_unmasked_surface$nu <= 1.0, ]

neisseria_elongata_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = neisseria_elongata_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = neisseria_elongata_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Neisseria elongata histogram of likelihoods')
neisseria_elongata_unmasked_surface_hist

neisseria_elongata_unmasked_surface_cutoff = quantile(neisseria_elongata_unmasked_surface$likelihood, 0.80)

neisseria_elongata_unmasked_surface[neisseria_elongata_unmasked_surface$likelihood < neisseria_elongata_unmasked_surface_cutoff, ]$likelihood = neisseria_elongata_unmasked_surface_cutoff

neisseria_elongata_unmasked_surface_scatter = ggplot(data=neisseria_elongata_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Neisseria elongata rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
neisseria_elongata_unmasked_surface_scatter

neisseria_subflava_masked_surface = read.csv('oral_likelihood_surfaces/neisseria_subflava_masked.csv', header=FALSE)
names(neisseria_subflava_masked_surface) = c('likelihood', 'nu', 'tau')

neisseria_subflava_masked_surface_expansion = neisseria_subflava_masked_surface[neisseria_subflava_masked_surface$nu > 1.0, ]
neisseria_subflava_masked_surface_contraction = neisseria_subflava_masked_surface[neisseria_subflava_masked_surface$nu <= 1.0, ]

neisseria_subflava_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = neisseria_subflava_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = neisseria_subflava_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Neisseria subflava histogram of likelihoods [singletons masked]')
neisseria_subflava_masked_surface_hist

neisseria_subflava_masked_surface_cutoff = quantile(neisseria_subflava_masked_surface$likelihood, 0.80)

neisseria_subflava_masked_surface[neisseria_subflava_masked_surface$likelihood < neisseria_subflava_masked_surface_cutoff, ]$likelihood = neisseria_subflava_masked_surface_cutoff

neisseria_subflava_masked_surface_scatter = ggplot(data=neisseria_subflava_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Neisseria subflava rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
neisseria_subflava_masked_surface_scatter

neisseria_subflava_unmasked_surface = read.csv('oral_likelihood_surfaces/neisseria_subflava_unmasked.csv', header=FALSE)
names(neisseria_subflava_unmasked_surface) = c('likelihood', 'nu', 'tau')

neisseria_subflava_unmasked_surface_expansion = neisseria_subflava_unmasked_surface[neisseria_subflava_unmasked_surface$nu > 1.0, ]
neisseria_subflava_unmasked_surface_contraction = neisseria_subflava_unmasked_surface[neisseria_subflava_unmasked_surface$nu <= 1.0, ]

neisseria_subflava_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = neisseria_subflava_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = neisseria_subflava_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Neisseria subflava histogram of likelihoods')
neisseria_subflava_unmasked_surface_hist

neisseria_subflava_unmasked_surface_cutoff = quantile(neisseria_subflava_unmasked_surface$likelihood, 0.80)

neisseria_subflava_unmasked_surface[neisseria_subflava_unmasked_surface$likelihood < neisseria_subflava_unmasked_surface_cutoff, ]$likelihood = neisseria_subflava_unmasked_surface_cutoff

neisseria_subflava_unmasked_surface_scatter = ggplot(data=neisseria_subflava_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Neisseria subflava rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
neisseria_subflava_unmasked_surface_scatter

rothia_dentocariosa_masked_surface = read.csv('oral_likelihood_surfaces/rothia_dentocariosa_masked.csv', header=FALSE)
names(rothia_dentocariosa_masked_surface) = c('likelihood', 'nu', 'tau')

rothia_dentocariosa_masked_surface_expansion = rothia_dentocariosa_masked_surface[rothia_dentocariosa_masked_surface$nu > 1.0, ]
rothia_dentocariosa_masked_surface_contraction = rothia_dentocariosa_masked_surface[rothia_dentocariosa_masked_surface$nu <= 1.0, ]

rothia_dentocariosa_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = rothia_dentocariosa_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = rothia_dentocariosa_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Rothia dentocariosa histogram of likelihoods [singletons masked]')
rothia_dentocariosa_masked_surface_hist

rothia_dentocariosa_masked_surface_cutoff = quantile(rothia_dentocariosa_masked_surface$likelihood, 0.80)

rothia_dentocariosa_masked_surface[rothia_dentocariosa_masked_surface$likelihood < rothia_dentocariosa_masked_surface_cutoff, ]$likelihood = rothia_dentocariosa_masked_surface_cutoff

rothia_dentocariosa_masked_surface_scatter = ggplot(data=rothia_dentocariosa_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Rothia dentocariosa rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
rothia_dentocariosa_masked_surface_scatter

rothia_dentocariosa_unmasked_surface = read.csv('oral_likelihood_surfaces/rothia_dentocariosa_unmasked.csv', header=FALSE)
names(rothia_dentocariosa_unmasked_surface) = c('likelihood', 'nu', 'tau')

rothia_dentocariosa_unmasked_surface_expansion = rothia_dentocariosa_unmasked_surface[rothia_dentocariosa_unmasked_surface$nu > 1.0, ]
rothia_dentocariosa_unmasked_surface_contraction = rothia_dentocariosa_unmasked_surface[rothia_dentocariosa_unmasked_surface$nu <= 1.0, ]

rothia_dentocariosa_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = rothia_dentocariosa_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = rothia_dentocariosa_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Rothia dentocariosa histogram of likelihoods')
rothia_dentocariosa_unmasked_surface_hist

rothia_dentocariosa_unmasked_surface_cutoff = quantile(rothia_dentocariosa_unmasked_surface$likelihood, 0.80)

rothia_dentocariosa_unmasked_surface[rothia_dentocariosa_unmasked_surface$likelihood < rothia_dentocariosa_unmasked_surface_cutoff, ]$likelihood = rothia_dentocariosa_unmasked_surface_cutoff

rothia_dentocariosa_unmasked_surface_scatter = ggplot(data=rothia_dentocariosa_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Rothia dentocariosa rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
rothia_dentocariosa_unmasked_surface_scatter

rothia_mucilaginosa_masked_surface = read.csv('oral_likelihood_surfaces/rothia_mucilaginosa_masked.csv', header=FALSE)
names(rothia_mucilaginosa_masked_surface) = c('likelihood', 'nu', 'tau')

rothia_mucilaginosa_masked_surface_expansion = rothia_mucilaginosa_masked_surface[rothia_mucilaginosa_masked_surface$nu > 1.0, ]
rothia_mucilaginosa_masked_surface_contraction = rothia_mucilaginosa_masked_surface[rothia_mucilaginosa_masked_surface$nu <= 1.0, ]

rothia_mucilaginosa_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = rothia_mucilaginosa_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = rothia_mucilaginosa_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Rothia mucilaginosa histogram of likelihoods [singletons masked]')
rothia_mucilaginosa_masked_surface_hist

rothia_mucilaginosa_masked_surface_cutoff = quantile(rothia_mucilaginosa_masked_surface$likelihood, 0.80)

rothia_mucilaginosa_masked_surface[rothia_mucilaginosa_masked_surface$likelihood < rothia_mucilaginosa_masked_surface_cutoff, ]$likelihood = rothia_mucilaginosa_masked_surface_cutoff

rothia_mucilaginosa_masked_surface_scatter = ggplot(data=rothia_mucilaginosa_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Rothia mucilaginosa rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
rothia_mucilaginosa_masked_surface_scatter

rothia_mucilaginosa_unmasked_surface = read.csv('oral_likelihood_surfaces/rothia_mucilaginosa_unmasked.csv', header=FALSE)
names(rothia_mucilaginosa_unmasked_surface) = c('likelihood', 'nu', 'tau')

rothia_mucilaginosa_unmasked_surface_expansion = rothia_mucilaginosa_unmasked_surface[rothia_mucilaginosa_unmasked_surface$nu > 1.0, ]
rothia_mucilaginosa_unmasked_surface_contraction = rothia_mucilaginosa_unmasked_surface[rothia_mucilaginosa_unmasked_surface$nu <= 1.0, ]

rothia_mucilaginosa_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = rothia_mucilaginosa_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = rothia_mucilaginosa_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Rothia mucilaginosa histogram of likelihoods')
rothia_mucilaginosa_unmasked_surface_hist

rothia_mucilaginosa_unmasked_surface_cutoff = quantile(rothia_mucilaginosa_unmasked_surface$likelihood, 0.80)

rothia_mucilaginosa_unmasked_surface[rothia_mucilaginosa_unmasked_surface$likelihood < rothia_mucilaginosa_unmasked_surface_cutoff, ]$likelihood = rothia_mucilaginosa_unmasked_surface_cutoff

rothia_mucilaginosa_unmasked_surface_scatter = ggplot(data=rothia_mucilaginosa_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Rothia mucilaginosa rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
rothia_mucilaginosa_unmasked_surface_scatter

streptococcus_australis_masked_surface = read.csv('oral_likelihood_surfaces/streptococcus_australis_masked.csv', header=FALSE)
names(streptococcus_australis_masked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_australis_masked_surface_expansion = streptococcus_australis_masked_surface[streptococcus_australis_masked_surface$nu > 1.0, ]
streptococcus_australis_masked_surface_contraction = streptococcus_australis_masked_surface[streptococcus_australis_masked_surface$nu <= 1.0, ]

streptococcus_australis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_australis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_australis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus australis histogram of likelihoods [singletons masked]')
streptococcus_australis_masked_surface_hist

streptococcus_australis_masked_surface_cutoff = quantile(streptococcus_australis_masked_surface$likelihood, 0.80)

streptococcus_australis_masked_surface[streptococcus_australis_masked_surface$likelihood < streptococcus_australis_masked_surface_cutoff, ]$likelihood = streptococcus_australis_masked_surface_cutoff

streptococcus_australis_masked_surface_scatter = ggplot(data=streptococcus_australis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus australis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_australis_masked_surface_scatter

streptococcus_australis_unmasked_surface = read.csv('oral_likelihood_surfaces/streptococcus_australis_unmasked.csv', header=FALSE)
names(streptococcus_australis_unmasked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_australis_unmasked_surface_expansion = streptococcus_australis_unmasked_surface[streptococcus_australis_unmasked_surface$nu > 1.0, ]
streptococcus_australis_unmasked_surface_contraction = streptococcus_australis_unmasked_surface[streptococcus_australis_unmasked_surface$nu <= 1.0, ]

streptococcus_australis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_australis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_australis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus australis histogram of likelihoods')
streptococcus_australis_unmasked_surface_hist

streptococcus_australis_unmasked_surface_cutoff = quantile(streptococcus_australis_unmasked_surface$likelihood, 0.80)

streptococcus_australis_unmasked_surface[streptococcus_australis_unmasked_surface$likelihood < streptococcus_australis_unmasked_surface_cutoff, ]$likelihood = streptococcus_australis_unmasked_surface_cutoff

streptococcus_australis_unmasked_surface_scatter = ggplot(data=streptococcus_australis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus australis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_australis_unmasked_surface_scatter

streptococcus_mitis_masked_surface = read.csv('oral_likelihood_surfaces/streptococcus_mitis_masked.csv', header=FALSE)
names(streptococcus_mitis_masked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_mitis_masked_surface_expansion = streptococcus_mitis_masked_surface[streptococcus_mitis_masked_surface$nu > 1.0, ]
streptococcus_mitis_masked_surface_contraction = streptococcus_mitis_masked_surface[streptococcus_mitis_masked_surface$nu <= 1.0, ]

streptococcus_mitis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_mitis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_mitis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus mitis histogram of likelihoods [singletons masked]')
streptococcus_mitis_masked_surface_hist

streptococcus_mitis_masked_surface_cutoff = quantile(streptococcus_mitis_masked_surface$likelihood, 0.80)

streptococcus_mitis_masked_surface[streptococcus_mitis_masked_surface$likelihood < streptococcus_mitis_masked_surface_cutoff, ]$likelihood = streptococcus_mitis_masked_surface_cutoff

streptococcus_mitis_masked_surface_scatter = ggplot(data=streptococcus_mitis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus mitis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_mitis_masked_surface_scatter

streptococcus_mitis_unmasked_surface = read.csv('oral_likelihood_surfaces/streptococcus_mitis_unmasked.csv', header=FALSE)
names(streptococcus_mitis_unmasked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_mitis_unmasked_surface_expansion = streptococcus_mitis_unmasked_surface[streptococcus_mitis_unmasked_surface$nu > 1.0, ]
streptococcus_mitis_unmasked_surface_contraction = streptococcus_mitis_unmasked_surface[streptococcus_mitis_unmasked_surface$nu <= 1.0, ]

streptococcus_mitis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_mitis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_mitis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus mitis histogram of likelihoods')
streptococcus_mitis_unmasked_surface_hist

streptococcus_mitis_unmasked_surface_cutoff = quantile(streptococcus_mitis_unmasked_surface$likelihood, 0.80)

streptococcus_mitis_unmasked_surface[streptococcus_mitis_unmasked_surface$likelihood < streptococcus_mitis_unmasked_surface_cutoff, ]$likelihood = streptococcus_mitis_unmasked_surface_cutoff

streptococcus_mitis_unmasked_surface_scatter = ggplot(data=streptococcus_mitis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus mitis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_mitis_unmasked_surface_scatter

streptococcus_salivarius_masked_surface = read.csv('oral_likelihood_surfaces/streptococcus_salivarius_masked.csv', header=FALSE)
names(streptococcus_salivarius_masked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_salivarius_masked_surface_expansion = streptococcus_salivarius_masked_surface[streptococcus_salivarius_masked_surface$nu > 1.0, ]
streptococcus_salivarius_masked_surface_contraction = streptococcus_salivarius_masked_surface[streptococcus_salivarius_masked_surface$nu <= 1.0, ]

streptococcus_salivarius_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_salivarius_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_salivarius_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus salivarius histogram of likelihoods [singletons masked]')
streptococcus_salivarius_masked_surface_hist

streptococcus_salivarius_masked_surface_cutoff = quantile(streptococcus_salivarius_masked_surface$likelihood, 0.80)

streptococcus_salivarius_masked_surface[streptococcus_salivarius_masked_surface$likelihood < streptococcus_salivarius_masked_surface_cutoff, ]$likelihood = streptococcus_salivarius_masked_surface_cutoff

streptococcus_salivarius_masked_surface_scatter = ggplot(data=streptococcus_salivarius_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus salivarius rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_salivarius_masked_surface_scatter

streptococcus_salivarius_unmasked_surface = read.csv('oral_likelihood_surfaces/streptococcus_salivarius_unmasked.csv', header=FALSE)
names(streptococcus_salivarius_unmasked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_salivarius_unmasked_surface_expansion = streptococcus_salivarius_unmasked_surface[streptococcus_salivarius_unmasked_surface$nu > 1.0, ]
streptococcus_salivarius_unmasked_surface_contraction = streptococcus_salivarius_unmasked_surface[streptococcus_salivarius_unmasked_surface$nu <= 1.0, ]

streptococcus_salivarius_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_salivarius_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_salivarius_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus salivarius histogram of likelihoods')
streptococcus_salivarius_unmasked_surface_hist

streptococcus_salivarius_unmasked_surface_cutoff = quantile(streptococcus_salivarius_unmasked_surface$likelihood, 0.80)

streptococcus_salivarius_unmasked_surface[streptococcus_salivarius_unmasked_surface$likelihood < streptococcus_salivarius_unmasked_surface_cutoff, ]$likelihood = streptococcus_salivarius_unmasked_surface_cutoff

streptococcus_salivarius_unmasked_surface_scatter = ggplot(data=streptococcus_salivarius_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus salivarius rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_salivarius_unmasked_surface_scatter

streptococcus_sanguinis_masked_surface = read.csv('oral_likelihood_surfaces/streptococcus_sanguinis_masked.csv', header=FALSE)
names(streptococcus_sanguinis_masked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_sanguinis_masked_surface_expansion = streptococcus_sanguinis_masked_surface[streptococcus_sanguinis_masked_surface$nu > 1.0, ]
streptococcus_sanguinis_masked_surface_contraction = streptococcus_sanguinis_masked_surface[streptococcus_sanguinis_masked_surface$nu <= 1.0, ]

streptococcus_sanguinis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_sanguinis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_sanguinis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus sanguinis histogram of likelihoods [singletons masked]')
streptococcus_sanguinis_masked_surface_hist

streptococcus_sanguinis_masked_surface_cutoff = quantile(streptococcus_sanguinis_masked_surface$likelihood, 0.80)

streptococcus_sanguinis_masked_surface[streptococcus_sanguinis_masked_surface$likelihood < streptococcus_sanguinis_masked_surface_cutoff, ]$likelihood = streptococcus_sanguinis_masked_surface_cutoff

streptococcus_sanguinis_masked_surface_scatter = ggplot(data=streptococcus_sanguinis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus sanguinis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_sanguinis_masked_surface_scatter

streptococcus_sanguinis_unmasked_surface = read.csv('oral_likelihood_surfaces/streptococcus_sanguinis_unmasked.csv', header=FALSE)
names(streptococcus_sanguinis_unmasked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_sanguinis_unmasked_surface_expansion = streptococcus_sanguinis_unmasked_surface[streptococcus_sanguinis_unmasked_surface$nu > 1.0, ]
streptococcus_sanguinis_unmasked_surface_contraction = streptococcus_sanguinis_unmasked_surface[streptococcus_sanguinis_unmasked_surface$nu <= 1.0, ]

streptococcus_sanguinis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_sanguinis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_sanguinis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus sanguinis histogram of likelihoods')
streptococcus_sanguinis_unmasked_surface_hist

streptococcus_sanguinis_unmasked_surface_cutoff = quantile(streptococcus_sanguinis_unmasked_surface$likelihood, 0.80)

streptococcus_sanguinis_unmasked_surface[streptococcus_sanguinis_unmasked_surface$likelihood < streptococcus_sanguinis_unmasked_surface_cutoff, ]$likelihood = streptococcus_sanguinis_unmasked_surface_cutoff

streptococcus_sanguinis_unmasked_surface_scatter = ggplot(data=streptococcus_sanguinis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus sanguinis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_sanguinis_unmasked_surface_scatter

streptococcus_sp_masked_surface = read.csv('oral_likelihood_surfaces/streptococcus_sp_masked.csv', header=FALSE)
names(streptococcus_sp_masked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_sp_masked_surface_expansion = streptococcus_sp_masked_surface[streptococcus_sp_masked_surface$nu > 1.0, ]
streptococcus_sp_masked_surface_contraction = streptococcus_sp_masked_surface[streptococcus_sp_masked_surface$nu <= 1.0, ]

streptococcus_sp_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_sp_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_sp_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus sp. histogram of likelihoods [singletons masked]')
streptococcus_sp_masked_surface_hist

streptococcus_sp_masked_surface_cutoff = quantile(streptococcus_sp_masked_surface$likelihood, 0.80)

streptococcus_sp_masked_surface[streptococcus_sp_masked_surface$likelihood < streptococcus_sp_masked_surface_cutoff, ]$likelihood = streptococcus_sp_masked_surface_cutoff

streptococcus_sp_masked_surface_scatter = ggplot(data=streptococcus_sp_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus sp. rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_sp_masked_surface_scatter

streptococcus_sp_unmasked_surface = read.csv('oral_likelihood_surfaces/streptococcus_sp_unmasked.csv', header=FALSE)
names(streptococcus_sp_unmasked_surface) = c('likelihood', 'nu', 'tau')

streptococcus_sp_unmasked_surface_expansion = streptococcus_sp_unmasked_surface[streptococcus_sp_unmasked_surface$nu > 1.0, ]
streptococcus_sp_unmasked_surface_contraction = streptococcus_sp_unmasked_surface[streptococcus_sp_unmasked_surface$nu <= 1.0, ]

streptococcus_sp_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = streptococcus_sp_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = streptococcus_sp_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Streptococcus sp. histogram of likelihoods')
streptococcus_sp_unmasked_surface_hist

streptococcus_sp_unmasked_surface_cutoff = quantile(streptococcus_sp_unmasked_surface$likelihood, 0.80)

streptococcus_sp_unmasked_surface[streptococcus_sp_unmasked_surface$likelihood < streptococcus_sp_unmasked_surface_cutoff, ]$likelihood = streptococcus_sp_unmasked_surface_cutoff

streptococcus_sp_unmasked_surface_scatter = ggplot(data=streptococcus_sp_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Streptococcus sp. rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
streptococcus_sp_unmasked_surface_scatter


veillonella_parvula_57794_masked_surface = read.csv('oral_likelihood_surfaces/veillonella_parvula_57794_masked.csv', header=FALSE)
names(veillonella_parvula_57794_masked_surface) = c('likelihood', 'nu', 'tau')

veillonella_parvula_57794_masked_surface_expansion = veillonella_parvula_57794_masked_surface[veillonella_parvula_57794_masked_surface$nu > 1.0, ]
veillonella_parvula_57794_masked_surface_contraction = veillonella_parvula_57794_masked_surface[veillonella_parvula_57794_masked_surface$nu <= 1.0, ]

veillonella_parvula_57794_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = veillonella_parvula_57794_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = veillonella_parvula_57794_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('V. parvula 57794 histogram of likelihoods [singletons masked]')
veillonella_parvula_57794_masked_surface_hist

veillonella_parvula_57794_masked_surface_cutoff = quantile(veillonella_parvula_57794_masked_surface$likelihood, 0.80)

veillonella_parvula_57794_masked_surface[veillonella_parvula_57794_masked_surface$likelihood < veillonella_parvula_57794_masked_surface_cutoff, ]$likelihood = veillonella_parvula_57794_masked_surface_cutoff

veillonella_parvula_57794_masked_surface_scatter = ggplot(data=veillonella_parvula_57794_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('V. parvula 57794 rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
veillonella_parvula_57794_masked_surface_scatter

veillonella_parvula_57794_unmasked_surface = read.csv('oral_likelihood_surfaces/veillonella_parvula_57794_unmasked.csv', header=FALSE)
names(veillonella_parvula_57794_unmasked_surface) = c('likelihood', 'nu', 'tau')

veillonella_parvula_57794_unmasked_surface_expansion = veillonella_parvula_57794_unmasked_surface[veillonella_parvula_57794_unmasked_surface$nu > 1.0, ]
veillonella_parvula_57794_unmasked_surface_contraction = veillonella_parvula_57794_unmasked_surface[veillonella_parvula_57794_unmasked_surface$nu <= 1.0, ]

veillonella_parvula_57794_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = veillonella_parvula_57794_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = veillonella_parvula_57794_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('V. parvula 57794 histogram of likelihoods')
veillonella_parvula_57794_unmasked_surface_hist

veillonella_parvula_57794_unmasked_surface_cutoff = quantile(veillonella_parvula_57794_unmasked_surface$likelihood, 0.80)

veillonella_parvula_57794_unmasked_surface[veillonella_parvula_57794_unmasked_surface$likelihood < veillonella_parvula_57794_unmasked_surface_cutoff, ]$likelihood = veillonella_parvula_57794_unmasked_surface_cutoff

veillonella_parvula_57794_unmasked_surface_scatter = ggplot(data=veillonella_parvula_57794_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('V. parvula 57794 rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
veillonella_parvula_57794_unmasked_surface_scatter

veillonella_parvula_58184_masked_surface = read.csv('oral_likelihood_surfaces/veillonella_parvula_58184_masked.csv', header=FALSE)
names(veillonella_parvula_58184_masked_surface) = c('likelihood', 'nu', 'tau')

veillonella_parvula_58184_masked_surface_expansion = veillonella_parvula_58184_masked_surface[veillonella_parvula_58184_masked_surface$nu > 1.0, ]
veillonella_parvula_58184_masked_surface_contraction = veillonella_parvula_58184_masked_surface[veillonella_parvula_58184_masked_surface$nu <= 1.0, ]

veillonella_parvula_58184_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = veillonella_parvula_58184_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = veillonella_parvula_58184_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('V. parvula 58184 histogram of likelihoods [singletons masked]')
veillonella_parvula_58184_masked_surface_hist

veillonella_parvula_58184_masked_surface_cutoff = quantile(veillonella_parvula_58184_masked_surface$likelihood, 0.80)

veillonella_parvula_58184_masked_surface[veillonella_parvula_58184_masked_surface$likelihood < veillonella_parvula_58184_masked_surface_cutoff, ]$likelihood = veillonella_parvula_58184_masked_surface_cutoff

veillonella_parvula_58184_masked_surface_scatter = ggplot(data=veillonella_parvula_58184_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('V. parvula 58184 rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
veillonella_parvula_58184_masked_surface_scatter

veillonella_parvula_58184_unmasked_surface = read.csv('oral_likelihood_surfaces/veillonella_parvula_58184_unmasked.csv', header=FALSE)
names(veillonella_parvula_58184_unmasked_surface) = c('likelihood', 'nu', 'tau')

veillonella_parvula_58184_unmasked_surface_expansion = veillonella_parvula_58184_unmasked_surface[veillonella_parvula_58184_unmasked_surface$nu > 1.0, ]
veillonella_parvula_58184_unmasked_surface_contraction = veillonella_parvula_58184_unmasked_surface[veillonella_parvula_58184_unmasked_surface$nu <= 1.0, ]

veillonella_parvula_58184_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = veillonella_parvula_58184_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = veillonella_parvula_58184_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('V. parvula 58184 histogram of likelihoods')
veillonella_parvula_58184_unmasked_surface_hist

veillonella_parvula_58184_unmasked_surface_cutoff = quantile(veillonella_parvula_58184_unmasked_surface$likelihood, 0.80)

veillonella_parvula_58184_unmasked_surface[veillonella_parvula_58184_unmasked_surface$likelihood < veillonella_parvula_58184_unmasked_surface_cutoff, ]$likelihood = veillonella_parvula_58184_unmasked_surface_cutoff

veillonella_parvula_58184_unmasked_surface_scatter = ggplot(data=veillonella_parvula_58184_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('V. parvula 58184 rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
veillonella_parvula_58184_unmasked_surface_scatter

# Gut Data likelihood surfaces

akkermansia_muciniphila_masked_surface = read.csv('gut_likelihood_surfaces/akkermansia_muciniphila_masked.csv', header=FALSE)
names(akkermansia_muciniphila_masked_surface) = c('likelihood', 'nu', 'tau')

akkermansia_muciniphila_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/akkermansia_muciniphila_masked.csv', header=FALSE)
names(akkermansia_muciniphila_masked_expansion) = c('likelihood', 'nu', 'tau')

akkermansia_muciniphila_masked_surface = rbind(akkermansia_muciniphila_masked_surface,
                                            akkermansia_muciniphila_masked_expansion)

akkermansia_muciniphila_masked_surface_expansion = akkermansia_muciniphila_masked_surface[akkermansia_muciniphila_masked_surface$nu > 1.0, ]
akkermansia_muciniphila_masked_surface_contraction = akkermansia_muciniphila_masked_surface[akkermansia_muciniphila_masked_surface$nu <= 1.0, ]

akkermansia_muciniphila_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = akkermansia_muciniphila_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = akkermansia_muciniphila_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Akkermansia muciniphila histogram of likelihoods [singletons masked]')
akkermansia_muciniphila_masked_surface_hist

akkermansia_muciniphila_masked_surface_cutoff = max(akkermansia_muciniphila_masked_surface$likelihood) - 10

akkermansia_muciniphila_masked_surface[akkermansia_muciniphila_masked_surface$likelihood < akkermansia_muciniphila_masked_surface_cutoff, ]$likelihood = akkermansia_muciniphila_masked_surface_cutoff

akkermansia_muciniphila_masked_surface_scatter = ggplot(data=akkermansia_muciniphila_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Akkermansia muciniphila rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
akkermansia_muciniphila_masked_surface_scatter

akkermansia_muciniphila_unmasked_surface = read.csv('gut_likelihood_surfaces/akkermansia_muciniphila_unmasked.csv', header=FALSE)
names(akkermansia_muciniphila_unmasked_surface) = c('likelihood', 'nu', 'tau')

akkermansia_muciniphila_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/akkermansia_muciniphila_unmasked.csv', header=FALSE)
names(akkermansia_muciniphila_unmasked_expansion) = c('likelihood', 'nu', 'tau')

akkermansia_muciniphila_unmasked_surface = rbind(akkermansia_muciniphila_unmasked_surface,
                                              akkermansia_muciniphila_unmasked_expansion)

akkermansia_muciniphila_unmasked_surface_expansion = akkermansia_muciniphila_unmasked_surface[akkermansia_muciniphila_unmasked_surface$nu > 1.0, ]
akkermansia_muciniphila_unmasked_surface_contraction = akkermansia_muciniphila_unmasked_surface[akkermansia_muciniphila_unmasked_surface$nu <= 1.0, ]

akkermansia_muciniphila_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = akkermansia_muciniphila_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = akkermansia_muciniphila_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Akkermansia muciniphila histogram of likelihoods [singletons unmasked]')
akkermansia_muciniphila_unmasked_surface_hist

akkermansia_muciniphila_unmasked_surface_cutoff = max(akkermansia_muciniphila_unmasked_surface$likelihood) - 10

akkermansia_muciniphila_unmasked_surface[akkermansia_muciniphila_unmasked_surface$likelihood < akkermansia_muciniphila_unmasked_surface_cutoff, ]$likelihood = akkermansia_muciniphila_unmasked_surface_cutoff

akkermansia_muciniphila_unmasked_surface_scatter = ggplot(data=akkermansia_muciniphila_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Akkermansia muciniphila rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
akkermansia_muciniphila_unmasked_surface_scatter

alistipes_finegoldii_masked_surface = read.csv('gut_likelihood_surfaces/alistipes_finegoldii_masked.csv', header=FALSE)
names(alistipes_finegoldii_masked_surface) = c('likelihood', 'nu', 'tau')

alistipes_finegoldii_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/alistipes_finegoldii_masked.csv', header=FALSE)
names(alistipes_finegoldii_masked_expansion) = c('likelihood', 'nu', 'tau')

alistipes_finegoldii_masked_surface = rbind(alistipes_finegoldii_masked_surface,
                                            alistipes_finegoldii_masked_expansion)

alistipes_finegoldii_masked_surface_expansion = alistipes_finegoldii_masked_surface[alistipes_finegoldii_masked_surface$nu > 1.0, ]
alistipes_finegoldii_masked_surface_contraction = alistipes_finegoldii_masked_surface[alistipes_finegoldii_masked_surface$nu <= 1.0, ]

alistipes_finegoldii_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_finegoldii_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_finegoldii_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes finegoldii histogram of likelihoods [singletons masked]')
alistipes_finegoldii_masked_surface_hist

alistipes_finegoldii_masked_surface_cutoff = max(alistipes_finegoldii_masked_surface$likelihood) - 10

alistipes_finegoldii_masked_surface[alistipes_finegoldii_masked_surface$likelihood < alistipes_finegoldii_masked_surface_cutoff, ]$likelihood = alistipes_finegoldii_masked_surface_cutoff

alistipes_finegoldii_masked_surface_scatter = ggplot(data=alistipes_finegoldii_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes finegoldii rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
alistipes_finegoldii_masked_surface_scatter

alistipes_finegoldii_unmasked_surface = read.csv('gut_likelihood_surfaces/alistipes_finegoldii_unmasked.csv', header=FALSE)
names(alistipes_finegoldii_unmasked_surface) = c('likelihood', 'nu', 'tau')

alistipes_finegoldii_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/alistipes_finegoldii_unmasked.csv', header=FALSE)
names(alistipes_finegoldii_unmasked_expansion) = c('likelihood', 'nu', 'tau')

alistipes_finegoldii_unmasked_surface = rbind(alistipes_finegoldii_unmasked_surface,
                                            alistipes_finegoldii_unmasked_expansion)

alistipes_finegoldii_unmasked_surface_expansion = alistipes_finegoldii_unmasked_surface[alistipes_finegoldii_unmasked_surface$nu > 1.0, ]
alistipes_finegoldii_unmasked_surface_contraction = alistipes_finegoldii_unmasked_surface[alistipes_finegoldii_unmasked_surface$nu <= 1.0, ]

alistipes_finegoldii_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_finegoldii_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_finegoldii_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes finegoldii histogram of likelihoods [singletons unmasked]')
alistipes_finegoldii_unmasked_surface_hist

alistipes_finegoldii_unmasked_surface_cutoff = max(alistipes_finegoldii_unmasked_surface$likelihood) - 10

alistipes_finegoldii_unmasked_surface[alistipes_finegoldii_unmasked_surface$likelihood < alistipes_finegoldii_unmasked_surface_cutoff, ]$likelihood = alistipes_finegoldii_unmasked_surface_cutoff

alistipes_finegoldii_unmasked_surface_scatter = ggplot(data=alistipes_finegoldii_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes finegoldii rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
alistipes_finegoldii_unmasked_surface_scatter

alistipes_onderdonkii_masked_surface = read.csv('gut_likelihood_surfaces/alistipes_onderdonkii_masked.csv', header=FALSE)
names(alistipes_onderdonkii_masked_surface) = c('likelihood', 'nu', 'tau')

alistipes_onderdonkii_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/alistipes_onderdonkii_masked.csv', header=FALSE)
names(alistipes_onderdonkii_masked_expansion) = c('likelihood', 'nu', 'tau')

alistipes_onderdonkii_masked_surface = rbind(alistipes_onderdonkii_masked_surface,
                                            alistipes_onderdonkii_masked_expansion)

alistipes_onderdonkii_masked_surface_expansion = alistipes_onderdonkii_masked_surface[alistipes_onderdonkii_masked_surface$nu > 1.0, ]
alistipes_onderdonkii_masked_surface_contraction = alistipes_onderdonkii_masked_surface[alistipes_onderdonkii_masked_surface$nu <= 1.0, ]

alistipes_onderdonkii_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_onderdonkii_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_onderdonkii_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes onderdonkii histogram of likelihoods [singletons masked]')
alistipes_onderdonkii_masked_surface_hist

alistipes_onderdonkii_masked_surface_cutoff = max(alistipes_onderdonkii_masked_surface$likelihood) - 10

alistipes_onderdonkii_masked_surface[alistipes_onderdonkii_masked_surface$likelihood < alistipes_onderdonkii_masked_surface_cutoff, ]$likelihood = alistipes_onderdonkii_masked_surface_cutoff

alistipes_onderdonkii_masked_surface_scatter = ggplot(data=alistipes_onderdonkii_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes onderdonkii rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
alistipes_onderdonkii_masked_surface_scatter

alistipes_onderdonkii_unmasked_surface = read.csv('gut_likelihood_surfaces/alistipes_onderdonkii_unmasked.csv', header=FALSE)
names(alistipes_onderdonkii_unmasked_surface) = c('likelihood', 'nu', 'tau')

alistipes_onderdonkii_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/alistipes_onderdonkii_unmasked.csv', header=FALSE)
names(alistipes_onderdonkii_unmasked_expansion) = c('likelihood', 'nu', 'tau')

alistipes_onderdonkii_unmasked_surface = rbind(alistipes_onderdonkii_unmasked_surface,
                                              alistipes_onderdonkii_unmasked_expansion)

alistipes_onderdonkii_unmasked_surface_expansion = alistipes_onderdonkii_unmasked_surface[alistipes_onderdonkii_unmasked_surface$nu > 1.0, ]
alistipes_onderdonkii_unmasked_surface_contraction = alistipes_onderdonkii_unmasked_surface[alistipes_onderdonkii_unmasked_surface$nu <= 1.0, ]

alistipes_onderdonkii_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_onderdonkii_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_onderdonkii_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes onderdonkii histogram of likelihoods [singletons unmasked]')
alistipes_onderdonkii_unmasked_surface_hist

alistipes_onderdonkii_unmasked_surface_cutoff = max(alistipes_onderdonkii_unmasked_surface$likelihood) - 10

alistipes_onderdonkii_unmasked_surface[alistipes_onderdonkii_unmasked_surface$likelihood < alistipes_onderdonkii_unmasked_surface_cutoff, ]$likelihood = alistipes_onderdonkii_unmasked_surface_cutoff

alistipes_onderdonkii_unmasked_surface_scatter = ggplot(data=alistipes_onderdonkii_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes onderdonkii rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
alistipes_onderdonkii_unmasked_surface_scatter

alistipes_putredinis_masked_surface = read.csv('gut_likelihood_surfaces/alistipes_putredinis_masked.csv', header=FALSE)
names(alistipes_putredinis_masked_surface) = c('likelihood', 'nu', 'tau')

alistipes_putredinis_masked_surface_expansion = alistipes_putredinis_masked_surface[alistipes_putredinis_masked_surface$nu > 1.0, ]
alistipes_putredinis_masked_surface_contraction = alistipes_putredinis_masked_surface[alistipes_putredinis_masked_surface$nu <= 1.0, ]

alistipes_putredinis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_putredinis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_putredinis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes putredinis histogram of likelihoods [singletons masked]')
alistipes_putredinis_masked_surface_hist

alistipes_putredinis_masked_surface_cutoff = quantile(alistipes_putredinis_masked_surface$likelihood, 0.80)

alistipes_putredinis_masked_surface[alistipes_putredinis_masked_surface$likelihood < alistipes_putredinis_masked_surface_cutoff, ]$likelihood = alistipes_putredinis_masked_surface_cutoff

alistipes_putredinis_masked_surface_scatter = ggplot(data=alistipes_putredinis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes putredinis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
alistipes_putredinis_masked_surface_scatter

alistipes_putredinis_unmasked_surface = read.csv('gut_likelihood_surfaces/alistipes_putredinis_unmasked.csv', header=FALSE)
names(alistipes_putredinis_unmasked_surface) = c('likelihood', 'nu', 'tau')

alistipes_putredinis_unmasked_surface_expansion = alistipes_putredinis_unmasked_surface[alistipes_putredinis_unmasked_surface$nu > 1.0, ]
alistipes_putredinis_unmasked_surface_contraction = alistipes_putredinis_unmasked_surface[alistipes_putredinis_unmasked_surface$nu <= 1.0, ]

alistipes_putredinis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_putredinis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_putredinis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes putredinis histogram of likelihoods')
alistipes_putredinis_unmasked_surface_hist

alistipes_putredinis_unmasked_surface_cutoff = quantile(alistipes_putredinis_unmasked_surface$likelihood, 0.80)

alistipes_putredinis_unmasked_surface[alistipes_putredinis_unmasked_surface$likelihood < alistipes_putredinis_unmasked_surface_cutoff, ]$likelihood = alistipes_putredinis_unmasked_surface_cutoff

alistipes_putredinis_unmasked_surface_scatter = ggplot(data=alistipes_putredinis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes putredinis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
alistipes_putredinis_unmasked_surface_scatter

alistipes_shahii_masked_surface = read.csv('gut_likelihood_surfaces/alistipes_shahii_masked.csv', header=FALSE)
names(alistipes_shahii_masked_surface) = c('likelihood', 'nu', 'tau')

alistipes_shahii_masked_surface_expansion = alistipes_shahii_masked_surface[alistipes_shahii_masked_surface$nu > 1.0, ]
alistipes_shahii_masked_surface_contraction = alistipes_shahii_masked_surface[alistipes_shahii_masked_surface$nu <= 1.0, ]

alistipes_shahii_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_shahii_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_shahii_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes shahii histogram of likelihoods [singletons masked]')
alistipes_shahii_masked_surface_hist

# alistipes_shahii_masked_surface_cutoff = quantile(alistipes_shahii_masked_surface$likelihood, 0.80)

alistipes_shahii_masked_surface_cutoff=-49

alistipes_shahii_masked_surface[alistipes_shahii_masked_surface$likelihood < alistipes_shahii_masked_surface_cutoff, ]$likelihood = alistipes_shahii_masked_surface_cutoff

alistipes_shahii_masked_surface_scatter = ggplot(data=alistipes_shahii_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes shahii rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
alistipes_shahii_masked_surface_scatter

alistipes_shahii_unmasked_surface = read.csv('gut_likelihood_surfaces/alistipes_shahii_unmasked.csv', header=FALSE)
names(alistipes_shahii_unmasked_surface) = c('likelihood', 'nu', 'tau')

alistipes_shahii_unmasked_surface_expansion = alistipes_shahii_unmasked_surface[alistipes_shahii_unmasked_surface$nu > 1.0, ]
alistipes_shahii_unmasked_surface_contraction = alistipes_shahii_unmasked_surface[alistipes_shahii_unmasked_surface$nu <= 1.0, ]

alistipes_shahii_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = alistipes_shahii_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = alistipes_shahii_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Alistipes shahii histogram of likelihoods')
alistipes_shahii_unmasked_surface_hist

#  alistipes_shahii_unmasked_surface_cutoff = quantile(alistipes_shahii_unmasked_surface$likelihood, 0.80)

alistipes_shahii_unmasked_surface_cutoff=-49

alistipes_shahii_unmasked_surface[alistipes_shahii_unmasked_surface$likelihood < alistipes_shahii_unmasked_surface_cutoff, ]$likelihood = alistipes_shahii_unmasked_surface_cutoff

alistipes_shahii_unmasked_surface_scatter = ggplot(data=alistipes_shahii_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Alistipes shahii rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
alistipes_shahii_unmasked_surface_scatter

bacteroidales_bacterium_masked_surface = read.csv('gut_likelihood_surfaces/bacteroidales_bacterium_masked.csv', header=FALSE)
names(bacteroidales_bacterium_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroidales_bacterium_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/bacteroidales_bacterium_masked.csv', header=FALSE)
names(bacteroidales_bacterium_masked_expansion) = c('likelihood', 'nu', 'tau')

bacteroidales_bacterium_masked_surface = rbind(bacteroidales_bacterium_masked_surface,
                                            bacteroidales_bacterium_masked_expansion)

bacteroidales_bacterium_masked_surface_expansion = bacteroidales_bacterium_masked_surface[bacteroidales_bacterium_masked_surface$nu > 1.0, ]
bacteroidales_bacterium_masked_surface_contraction = bacteroidales_bacterium_masked_surface[bacteroidales_bacterium_masked_surface$nu <= 1.0, ]

bacteroidales_bacterium_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroidales_bacterium_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroidales_bacterium_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroidales bacterium histogram of likelihoods [singletons masked]')
bacteroidales_bacterium_masked_surface_hist

bacteroidales_bacterium_masked_surface_cutoff = max(bacteroidales_bacterium_masked_surface$likelihood) - 10

bacteroidales_bacterium_masked_surface[bacteroidales_bacterium_masked_surface$likelihood < bacteroidales_bacterium_masked_surface_cutoff, ]$likelihood = bacteroidales_bacterium_masked_surface_cutoff

bacteroidales_bacterium_masked_surface_scatter = ggplot(data=bacteroidales_bacterium_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroidales bacterium rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroidales_bacterium_masked_surface_scatter

bacteroidales_bacterium_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroidales_bacterium_unmasked.csv', header=FALSE)
names(bacteroidales_bacterium_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroidales_bacterium_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/bacteroidales_bacterium_unmasked.csv', header=FALSE)
names(bacteroidales_bacterium_unmasked_expansion) = c('likelihood', 'nu', 'tau')

bacteroidales_bacterium_unmasked_surface = rbind(bacteroidales_bacterium_unmasked_surface,
                                              bacteroidales_bacterium_unmasked_expansion)

bacteroidales_bacterium_unmasked_surface_expansion = bacteroidales_bacterium_unmasked_surface[bacteroidales_bacterium_unmasked_surface$nu > 1.0, ]
bacteroidales_bacterium_unmasked_surface_contraction = bacteroidales_bacterium_unmasked_surface[bacteroidales_bacterium_unmasked_surface$nu <= 1.0, ]

bacteroidales_bacterium_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroidales_bacterium_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroidales_bacterium_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroidales bacterium histogram of likelihoods [singletons unmasked]')
bacteroidales_bacterium_unmasked_surface_hist

bacteroidales_bacterium_unmasked_surface_cutoff = max(bacteroidales_bacterium_unmasked_surface$likelihood) - 10

bacteroidales_bacterium_unmasked_surface[bacteroidales_bacterium_unmasked_surface$likelihood < bacteroidales_bacterium_unmasked_surface_cutoff, ]$likelihood = bacteroidales_bacterium_unmasked_surface_cutoff

bacteroidales_bacterium_unmasked_surface_scatter = ggplot(data=bacteroidales_bacterium_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroidales bacterium rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroidales_bacterium_unmasked_surface_scatter

bacteroides_caccae_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_caccae_masked.csv', header=FALSE)
names(bacteroides_caccae_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_caccae_masked_surface_expansion = bacteroides_caccae_masked_surface[bacteroides_caccae_masked_surface$nu > 1.0, ]
bacteroides_caccae_masked_surface_contraction = bacteroides_caccae_masked_surface[bacteroides_caccae_masked_surface$nu <= 1.0, ]

bacteroides_caccae_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_caccae_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_caccae_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides caccae histogram of likelihoods [singletons masked]')
bacteroides_caccae_masked_surface_hist

bacteroides_caccae_masked_surface_cutoff = quantile(bacteroides_caccae_masked_surface$likelihood, 0.80)

bacteroides_caccae_masked_surface[bacteroides_caccae_masked_surface$likelihood < bacteroides_caccae_masked_surface_cutoff, ]$likelihood = bacteroides_caccae_masked_surface_cutoff

bacteroides_caccae_masked_surface_scatter = ggplot(data=bacteroides_caccae_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides caccae rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_caccae_masked_surface_scatter

bacteroides_caccae_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_caccae_unmasked.csv', header=FALSE)
names(bacteroides_caccae_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_caccae_unmasked_surface_expansion = bacteroides_caccae_unmasked_surface[bacteroides_caccae_unmasked_surface$nu > 1.0, ]
bacteroides_caccae_unmasked_surface_contraction = bacteroides_caccae_unmasked_surface[bacteroides_caccae_unmasked_surface$nu <= 1.0, ]

bacteroides_caccae_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_caccae_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_caccae_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides caccae histogram of likelihoods')
bacteroides_caccae_unmasked_surface_hist

bacteroides_caccae_unmasked_surface_cutoff = quantile(bacteroides_caccae_unmasked_surface$likelihood, 0.80)

bacteroides_caccae_unmasked_surface[bacteroides_caccae_unmasked_surface$likelihood < bacteroides_caccae_unmasked_surface_cutoff, ]$likelihood = bacteroides_caccae_unmasked_surface_cutoff

bacteroides_caccae_unmasked_surface_scatter = ggplot(data=bacteroides_caccae_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides caccae rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_caccae_unmasked_surface_scatter

bacteroides_cellulosilyticus_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_cellulosilyticus_masked.csv', header=FALSE)
names(bacteroides_cellulosilyticus_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_cellulosilyticus_masked_surface_expansion = bacteroides_cellulosilyticus_masked_surface[bacteroides_cellulosilyticus_masked_surface$nu > 1.0, ]
bacteroides_cellulosilyticus_masked_surface_contraction = bacteroides_cellulosilyticus_masked_surface[bacteroides_cellulosilyticus_masked_surface$nu <= 1.0, ]

bacteroides_cellulosilyticus_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_cellulosilyticus_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_cellulosilyticus_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides cellulosilyticus histogram of likelihoods [singletons masked]')
bacteroides_cellulosilyticus_masked_surface_hist

bacteroides_cellulosilyticus_masked_surface_cutoff = quantile(bacteroides_cellulosilyticus_masked_surface$likelihood, 0.80)

bacteroides_cellulosilyticus_masked_surface[bacteroides_cellulosilyticus_masked_surface$likelihood < bacteroides_cellulosilyticus_masked_surface_cutoff, ]$likelihood = bacteroides_cellulosilyticus_masked_surface_cutoff

bacteroides_cellulosilyticus_masked_surface_scatter = ggplot(data=bacteroides_cellulosilyticus_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides cellulosilyticus rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_cellulosilyticus_masked_surface_scatter

bacteroides_cellulosilyticus_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_cellulosilyticus_unmasked.csv', header=FALSE)
names(bacteroides_cellulosilyticus_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_cellulosilyticus_unmasked_surface_expansion = bacteroides_cellulosilyticus_unmasked_surface[bacteroides_cellulosilyticus_unmasked_surface$nu > 1.0, ]
bacteroides_cellulosilyticus_unmasked_surface_contraction = bacteroides_cellulosilyticus_unmasked_surface[bacteroides_cellulosilyticus_unmasked_surface$nu <= 1.0, ]

bacteroides_cellulosilyticus_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_cellulosilyticus_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_cellulosilyticus_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides cellulosilyticus histogram of likelihoods')
bacteroides_cellulosilyticus_unmasked_surface_hist

bacteroides_cellulosilyticus_unmasked_surface_cutoff = quantile(bacteroides_cellulosilyticus_unmasked_surface$likelihood, 0.80)

bacteroides_cellulosilyticus_unmasked_surface[bacteroides_cellulosilyticus_unmasked_surface$likelihood < bacteroides_cellulosilyticus_unmasked_surface_cutoff, ]$likelihood = bacteroides_cellulosilyticus_unmasked_surface_cutoff

bacteroides_cellulosilyticus_unmasked_surface_scatter = ggplot(data=bacteroides_cellulosilyticus_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides cellulosilyticus rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_cellulosilyticus_unmasked_surface_scatter

bacteroides_fragilis_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_fragilis_masked.csv', header=FALSE)
names(bacteroides_fragilis_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_fragilis_masked_surface_expansion = bacteroides_fragilis_masked_surface[bacteroides_fragilis_masked_surface$nu > 1.0, ]
bacteroides_fragilis_masked_surface_contraction = bacteroides_fragilis_masked_surface[bacteroides_fragilis_masked_surface$nu <= 1.0, ]

bacteroides_fragilis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_fragilis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_fragilis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides fragilis histogram of likelihoods [singletons masked]')
bacteroides_fragilis_masked_surface_hist

bacteroides_fragilis_masked_surface_cutoff = quantile(bacteroides_fragilis_masked_surface$likelihood, 0.80)

bacteroides_fragilis_masked_surface[bacteroides_fragilis_masked_surface$likelihood < bacteroides_fragilis_masked_surface_cutoff, ]$likelihood = bacteroides_fragilis_masked_surface_cutoff

bacteroides_fragilis_masked_surface_scatter = ggplot(data=bacteroides_fragilis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides fragilis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_fragilis_masked_surface_scatter

bacteroides_fragilis_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_fragilis_unmasked.csv', header=FALSE)
names(bacteroides_fragilis_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_fragilis_unmasked_surface_expansion = bacteroides_fragilis_unmasked_surface[bacteroides_fragilis_unmasked_surface$nu > 1.0, ]
bacteroides_fragilis_unmasked_surface_contraction = bacteroides_fragilis_unmasked_surface[bacteroides_fragilis_unmasked_surface$nu <= 1.0, ]

bacteroides_fragilis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_fragilis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_fragilis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides fragilis histogram of likelihoods')
bacteroides_fragilis_unmasked_surface_hist

bacteroides_fragilis_unmasked_surface_cutoff = quantile(bacteroides_fragilis_unmasked_surface$likelihood, 0.80)

bacteroides_fragilis_unmasked_surface[bacteroides_fragilis_unmasked_surface$likelihood < bacteroides_fragilis_unmasked_surface_cutoff, ]$likelihood = bacteroides_fragilis_unmasked_surface_cutoff

bacteroides_fragilis_unmasked_surface_scatter = ggplot(data=bacteroides_fragilis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides fragilis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_fragilis_unmasked_surface_scatter

bacteroides_ovatus_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_ovatus_masked.csv', header=FALSE)
names(bacteroides_ovatus_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_ovatus_masked_surface_expansion = bacteroides_ovatus_masked_surface[bacteroides_ovatus_masked_surface$nu > 1.0, ]
bacteroides_ovatus_masked_surface_contraction = bacteroides_ovatus_masked_surface[bacteroides_ovatus_masked_surface$nu <= 1.0, ]

bacteroides_ovatus_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_ovatus_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_ovatus_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides ovatus histogram of likelihoods [singletons masked]')
bacteroides_ovatus_masked_surface_hist

bacteroides_ovatus_masked_surface_cutoff = quantile(bacteroides_ovatus_masked_surface$likelihood, 0.80)

bacteroides_ovatus_masked_surface[bacteroides_ovatus_masked_surface$likelihood < bacteroides_ovatus_masked_surface_cutoff, ]$likelihood = bacteroides_ovatus_masked_surface_cutoff

bacteroides_ovatus_masked_surface_scatter = ggplot(data=bacteroides_ovatus_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides ovatus rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_ovatus_masked_surface_scatter

bacteroides_ovatus_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_ovatus_unmasked.csv', header=FALSE)
names(bacteroides_ovatus_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_ovatus_unmasked_surface_expansion = bacteroides_ovatus_unmasked_surface[bacteroides_ovatus_unmasked_surface$nu > 1.0, ]
bacteroides_ovatus_unmasked_surface_contraction = bacteroides_ovatus_unmasked_surface[bacteroides_ovatus_unmasked_surface$nu <= 1.0, ]

bacteroides_ovatus_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_ovatus_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_ovatus_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides ovatus histogram of likelihoods')
bacteroides_ovatus_unmasked_surface_hist

bacteroides_ovatus_unmasked_surface_cutoff = quantile(bacteroides_ovatus_unmasked_surface$likelihood, 0.80)

bacteroides_ovatus_unmasked_surface[bacteroides_ovatus_unmasked_surface$likelihood < bacteroides_ovatus_unmasked_surface_cutoff, ]$likelihood = bacteroides_ovatus_unmasked_surface_cutoff

bacteroides_ovatus_unmasked_surface_scatter = ggplot(data=bacteroides_ovatus_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides ovatus rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_ovatus_unmasked_surface_scatter

bacteroides_stercoris_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_stercoris_masked.csv', header=FALSE)
names(bacteroides_stercoris_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_stercoris_masked_surface_expansion = bacteroides_stercoris_masked_surface[bacteroides_stercoris_masked_surface$nu > 1.0, ]
bacteroides_stercoris_masked_surface_contraction = bacteroides_stercoris_masked_surface[bacteroides_stercoris_masked_surface$nu <= 1.0, ]

bacteroides_stercoris_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_stercoris_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_stercoris_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides stercoris histogram of likelihoods [singletons masked]')
bacteroides_stercoris_masked_surface_hist

bacteroides_stercoris_masked_surface_cutoff = quantile(bacteroides_stercoris_masked_surface$likelihood, 0.80)

bacteroides_stercoris_masked_surface[bacteroides_stercoris_masked_surface$likelihood < bacteroides_stercoris_masked_surface_cutoff, ]$likelihood = bacteroides_stercoris_masked_surface_cutoff

bacteroides_stercoris_masked_surface_scatter = ggplot(data=bacteroides_stercoris_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides stercoris rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_stercoris_masked_surface_scatter

bacteroides_stercoris_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_stercoris_unmasked.csv', header=FALSE)
names(bacteroides_stercoris_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_stercoris_unmasked_surface_expansion = bacteroides_stercoris_unmasked_surface[bacteroides_stercoris_unmasked_surface$nu > 1.0, ]
bacteroides_stercoris_unmasked_surface_contraction = bacteroides_stercoris_unmasked_surface[bacteroides_stercoris_unmasked_surface$nu <= 1.0, ]

bacteroides_stercoris_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_stercoris_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_stercoris_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides stercoris histogram of likelihoods')
bacteroides_stercoris_unmasked_surface_hist

bacteroides_stercoris_unmasked_surface_cutoff = quantile(bacteroides_stercoris_unmasked_surface$likelihood, 0.80)

bacteroides_stercoris_unmasked_surface[bacteroides_stercoris_unmasked_surface$likelihood < bacteroides_stercoris_unmasked_surface_cutoff, ]$likelihood = bacteroides_stercoris_unmasked_surface_cutoff

bacteroides_stercoris_unmasked_surface_scatter = ggplot(data=bacteroides_stercoris_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides stercoris rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_stercoris_unmasked_surface_scatter

bacteroides_thetaiotaomicron_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_thetaiotaomicron_masked.csv', header=FALSE)
names(bacteroides_thetaiotaomicron_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_thetaiotaomicron_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/bacteroides_thetaiotaomicron_masked.csv', header=FALSE)
names(bacteroides_thetaiotaomicron_masked_expansion) = c('likelihood', 'nu', 'tau')

bacteroides_thetaiotaomicron_masked_surface = rbind(bacteroides_thetaiotaomicron_masked_surface,
                                            bacteroides_thetaiotaomicron_masked_expansion)

bacteroides_thetaiotaomicron_masked_surface_expansion = bacteroides_thetaiotaomicron_masked_surface[bacteroides_thetaiotaomicron_masked_surface$nu > 1.0, ]
bacteroides_thetaiotaomicron_masked_surface_contraction = bacteroides_thetaiotaomicron_masked_surface[bacteroides_thetaiotaomicron_masked_surface$nu <= 1.0, ]

bacteroides_thetaiotaomicron_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_thetaiotaomicron_masked_surface_expansion,  bins=200) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_thetaiotaomicron_masked_surface_contraction, bins=200) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides thetaiotaomicron histogram of likelihoods [singletons masked]')
bacteroides_thetaiotaomicron_masked_surface_hist

bacteroides_thetaiotaomicron_masked_surface_cutoff = max(bacteroides_thetaiotaomicron_masked_surface$likelihood) - 10

bacteroides_thetaiotaomicron_masked_surface[bacteroides_thetaiotaomicron_masked_surface$likelihood < bacteroides_thetaiotaomicron_masked_surface_cutoff, ]$likelihood = bacteroides_thetaiotaomicron_masked_surface_cutoff

bacteroides_thetaiotaomicron_masked_surface_scatter = ggplot(data=bacteroides_thetaiotaomicron_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides thetaiotaomicron rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_thetaiotaomicron_masked_surface_scatter

bacteroides_thetaiotaomicron_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_thetaiotaomicron_unmasked.csv', header=FALSE)
names(bacteroides_thetaiotaomicron_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_thetaiotaomicron_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/bacteroides_thetaiotaomicron_unmasked.csv', header=FALSE)
names(bacteroides_thetaiotaomicron_unmasked_expansion) = c('likelihood', 'nu', 'tau')

bacteroides_thetaiotaomicron_unmasked_surface = rbind(bacteroides_thetaiotaomicron_unmasked_surface,
                                              bacteroides_thetaiotaomicron_unmasked_expansion)

bacteroides_thetaiotaomicron_unmasked_surface_expansion = bacteroides_thetaiotaomicron_unmasked_surface[bacteroides_thetaiotaomicron_unmasked_surface$nu > 1.0, ]
bacteroides_thetaiotaomicron_unmasked_surface_contraction = bacteroides_thetaiotaomicron_unmasked_surface[bacteroides_thetaiotaomicron_unmasked_surface$nu <= 1.0, ]

bacteroides_thetaiotaomicron_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_thetaiotaomicron_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_thetaiotaomicron_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides thetaiotaomicron histogram of likelihoods [singletons unmasked]')
bacteroides_thetaiotaomicron_unmasked_surface_hist

bacteroides_thetaiotaomicron_unmasked_surface_cutoff = max(bacteroides_thetaiotaomicron_unmasked_surface$likelihood) - 10

bacteroides_thetaiotaomicron_unmasked_surface[bacteroides_thetaiotaomicron_unmasked_surface$likelihood < bacteroides_thetaiotaomicron_unmasked_surface_cutoff, ]$likelihood = bacteroides_thetaiotaomicron_unmasked_surface_cutoff

bacteroides_thetaiotaomicron_unmasked_surface_scatter = ggplot(data=bacteroides_thetaiotaomicron_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides thetaiotaomicron rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_thetaiotaomicron_unmasked_surface_scatter

bacteroides_uniformis_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_uniformis_masked.csv', header=FALSE)
names(bacteroides_uniformis_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_uniformis_masked_surface_expansion = bacteroides_uniformis_masked_surface[bacteroides_uniformis_masked_surface$nu > 1.0, ]
bacteroides_uniformis_masked_surface_contraction = bacteroides_uniformis_masked_surface[bacteroides_uniformis_masked_surface$nu <= 1.0, ]

bacteroides_uniformis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_uniformis_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_uniformis_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides uniformis histogram of likelihoods [singletons masked]')
bacteroides_uniformis_masked_surface_hist

bacteroides_uniformis_masked_surface_cutoff = quantile(bacteroides_uniformis_masked_surface$likelihood, 0.80)

bacteroides_uniformis_masked_surface[bacteroides_uniformis_masked_surface$likelihood < bacteroides_uniformis_masked_surface_cutoff, ]$likelihood = bacteroides_uniformis_masked_surface_cutoff

bacteroides_uniformis_masked_surface_scatter = ggplot(data=bacteroides_uniformis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides uniformis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_uniformis_masked_surface_scatter

bacteroides_uniformis_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_uniformis_unmasked.csv', header=FALSE)
names(bacteroides_uniformis_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_uniformis_unmasked_surface_expansion = bacteroides_uniformis_unmasked_surface[bacteroides_uniformis_unmasked_surface$nu > 1.0, ]
bacteroides_uniformis_unmasked_surface_contraction = bacteroides_uniformis_unmasked_surface[bacteroides_uniformis_unmasked_surface$nu <= 1.0, ]

bacteroides_uniformis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_uniformis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_uniformis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides uniformis histogram of likelihoods')
bacteroides_uniformis_unmasked_surface_hist

bacteroides_uniformis_unmasked_surface_cutoff = quantile(bacteroides_uniformis_unmasked_surface$likelihood, 0.80)

bacteroides_uniformis_unmasked_surface[bacteroides_uniformis_unmasked_surface$likelihood < bacteroides_uniformis_unmasked_surface_cutoff, ]$likelihood = bacteroides_uniformis_unmasked_surface_cutoff

bacteroides_uniformis_unmasked_surface_scatter = ggplot(data=bacteroides_uniformis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides uniformis rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_uniformis_unmasked_surface_scatter

bacteroides_vulgatus_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_vulgatus_masked.csv', header=FALSE)
names(bacteroides_vulgatus_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_vulgatus_masked_surface_expansion = bacteroides_vulgatus_masked_surface[bacteroides_vulgatus_masked_surface$nu > 1.0, ]
bacteroides_vulgatus_masked_surface_contraction = bacteroides_vulgatus_masked_surface[bacteroides_vulgatus_masked_surface$nu <= 1.0, ]

bacteroides_vulgatus_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_vulgatus_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_vulgatus_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides vulgatus histogram of likelihoods [singletons masked]')
bacteroides_vulgatus_masked_surface_hist

bacteroides_vulgatus_masked_surface_cutoff = quantile(bacteroides_vulgatus_masked_surface$likelihood, 0.80)

bacteroides_vulgatus_masked_surface[bacteroides_vulgatus_masked_surface$likelihood < bacteroides_vulgatus_masked_surface_cutoff, ]$likelihood = bacteroides_vulgatus_masked_surface_cutoff

bacteroides_vulgatus_masked_surface_scatter = ggplot(data=bacteroides_vulgatus_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides vulgatus rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_vulgatus_masked_surface_scatter

bacteroides_vulgatus_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_vulgatus_unmasked.csv', header=FALSE)
names(bacteroides_vulgatus_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_vulgatus_unmasked_surface_expansion = bacteroides_vulgatus_unmasked_surface[bacteroides_vulgatus_unmasked_surface$nu > 1.0, ]
bacteroides_vulgatus_unmasked_surface_contraction = bacteroides_vulgatus_unmasked_surface[bacteroides_vulgatus_unmasked_surface$nu <= 1.0, ]

bacteroides_vulgatus_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_vulgatus_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_vulgatus_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides vulgatus histogram of likelihoods')
bacteroides_vulgatus_unmasked_surface_hist

bacteroides_vulgatus_unmasked_surface_cutoff = quantile(bacteroides_vulgatus_unmasked_surface$likelihood, 0.80)

bacteroides_vulgatus_unmasked_surface[bacteroides_vulgatus_unmasked_surface$likelihood < bacteroides_vulgatus_unmasked_surface_cutoff, ]$likelihood = bacteroides_vulgatus_unmasked_surface_cutoff

bacteroides_vulgatus_unmasked_surface_scatter = ggplot(data=bacteroides_vulgatus_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides vulgatus rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_vulgatus_unmasked_surface_scatter

bacteroides_xylanisolvens_masked_surface = read.csv('gut_likelihood_surfaces/bacteroides_xylanisolvens_masked.csv', header=FALSE)
names(bacteroides_xylanisolvens_masked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_xylanisolvens_masked_surface_expansion = bacteroides_xylanisolvens_masked_surface[bacteroides_xylanisolvens_masked_surface$nu > 1.0, ]
bacteroides_xylanisolvens_masked_surface_contraction = bacteroides_xylanisolvens_masked_surface[bacteroides_xylanisolvens_masked_surface$nu <= 1.0, ]

bacteroides_xylanisolvens_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_xylanisolvens_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_xylanisolvens_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides xylanisolvens histogram of likelihoods [singletons masked]')
bacteroides_xylanisolvens_masked_surface_hist

bacteroides_xylanisolvens_masked_surface_cutoff = quantile(bacteroides_xylanisolvens_masked_surface$likelihood, 0.80)

bacteroides_xylanisolvens_masked_surface[bacteroides_xylanisolvens_masked_surface$likelihood < bacteroides_xylanisolvens_masked_surface_cutoff, ]$likelihood = bacteroides_xylanisolvens_masked_surface_cutoff

bacteroides_xylanisolvens_masked_surface_scatter = ggplot(data=bacteroides_xylanisolvens_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides xylanisolvens rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_xylanisolvens_masked_surface_scatter

bacteroides_xylanisolvens_unmasked_surface = read.csv('gut_likelihood_surfaces/bacteroides_xylanisolvens_unmasked.csv', header=FALSE)
names(bacteroides_xylanisolvens_unmasked_surface) = c('likelihood', 'nu', 'tau')

bacteroides_xylanisolvens_unmasked_surface_expansion = bacteroides_xylanisolvens_unmasked_surface[bacteroides_xylanisolvens_unmasked_surface$nu > 1.0, ]
bacteroides_xylanisolvens_unmasked_surface_contraction = bacteroides_xylanisolvens_unmasked_surface[bacteroides_xylanisolvens_unmasked_surface$nu <= 1.0, ]

bacteroides_xylanisolvens_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = bacteroides_xylanisolvens_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = bacteroides_xylanisolvens_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Bacteroides xylanisolvens histogram of likelihoods')
bacteroides_xylanisolvens_unmasked_surface_hist

bacteroides_xylanisolvens_unmasked_surface_cutoff = quantile(bacteroides_xylanisolvens_unmasked_surface$likelihood, 0.80)

bacteroides_xylanisolvens_unmasked_surface[bacteroides_xylanisolvens_unmasked_surface$likelihood < bacteroides_xylanisolvens_unmasked_surface_cutoff, ]$likelihood = bacteroides_xylanisolvens_unmasked_surface_cutoff

bacteroides_xylanisolvens_unmasked_surface_scatter = ggplot(data=bacteroides_xylanisolvens_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Bacteroides xylanisolvens rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
bacteroides_xylanisolvens_unmasked_surface_scatter

barnesiella_intestinihominis_masked_surface = read.csv('gut_likelihood_surfaces/barnesiella_intestinihominis_masked.csv', header=FALSE)
names(barnesiella_intestinihominis_masked_surface) = c('likelihood', 'nu', 'tau')

barnesiella_intestinihominis_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/barnesiella_intestinihominis_masked.csv', header=FALSE)
names(barnesiella_intestinihominis_masked_expansion) = c('likelihood', 'nu', 'tau')

barnesiella_intestinihominis_masked_surface = rbind(barnesiella_intestinihominis_masked_surface,
                                            barnesiella_intestinihominis_masked_expansion)

barnesiella_intestinihominis_masked_surface_expansion = barnesiella_intestinihominis_masked_surface[barnesiella_intestinihominis_masked_surface$nu > 1.0, ]
barnesiella_intestinihominis_masked_surface_contraction = barnesiella_intestinihominis_masked_surface[barnesiella_intestinihominis_masked_surface$nu <= 1.0, ]

barnesiella_intestinihominis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = barnesiella_intestinihominis_masked_surface_expansion,  bins=200) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = barnesiella_intestinihominis_masked_surface_contraction, bins=200) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Barnesiella intestinihominis histogram of likelihoods [singletons masked]')
barnesiella_intestinihominis_masked_surface_hist

barnesiella_intestinihominis_masked_surface_cutoff = max(barnesiella_intestinihominis_masked_surface$likelihood) - 10

barnesiella_intestinihominis_masked_surface[barnesiella_intestinihominis_masked_surface$likelihood < barnesiella_intestinihominis_masked_surface_cutoff, ]$likelihood = barnesiella_intestinihominis_masked_surface_cutoff

barnesiella_intestinihominis_masked_surface_scatter = ggplot(data=barnesiella_intestinihominis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Barnesiella intestinihominis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
barnesiella_intestinihominis_masked_surface_scatter

barnesiella_intestinihominis_unmasked_surface = read.csv('gut_likelihood_surfaces/barnesiella_intestinihominis_unmasked.csv', header=FALSE)
names(barnesiella_intestinihominis_unmasked_surface) = c('likelihood', 'nu', 'tau')

barnesiella_intestinihominis_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/barnesiella_intestinihominis_unmasked.csv', header=FALSE)
names(barnesiella_intestinihominis_unmasked_expansion) = c('likelihood', 'nu', 'tau')

barnesiella_intestinihominis_unmasked_surface = rbind(barnesiella_intestinihominis_unmasked_surface,
                                              barnesiella_intestinihominis_unmasked_expansion)

barnesiella_intestinihominis_unmasked_surface_expansion = barnesiella_intestinihominis_unmasked_surface[barnesiella_intestinihominis_unmasked_surface$nu > 1.0, ]
barnesiella_intestinihominis_unmasked_surface_contraction = barnesiella_intestinihominis_unmasked_surface[barnesiella_intestinihominis_unmasked_surface$nu <= 1.0, ]

barnesiella_intestinihominis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = barnesiella_intestinihominis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = barnesiella_intestinihominis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Barnesiella intestinihominis histogram of likelihoods [singletons unmasked]')
barnesiella_intestinihominis_unmasked_surface_hist

barnesiella_intestinihominis_unmasked_surface_cutoff = max(barnesiella_intestinihominis_unmasked_surface$likelihood) - 10

barnesiella_intestinihominis_unmasked_surface[barnesiella_intestinihominis_unmasked_surface$likelihood < barnesiella_intestinihominis_unmasked_surface_cutoff, ]$likelihood = barnesiella_intestinihominis_unmasked_surface_cutoff

barnesiella_intestinihominis_unmasked_surface_scatter = ggplot(data=barnesiella_intestinihominis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Barnesiella intestinihominis rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
barnesiella_intestinihominis_unmasked_surface_scatter

dialister_invisus_masked_surface = read.csv('gut_likelihood_surfaces/dialister_invisus_masked.csv', header=FALSE)
names(dialister_invisus_masked_surface) = c('likelihood', 'nu', 'tau')

dialister_invisus_masked_surface_expansion = dialister_invisus_masked_surface[dialister_invisus_masked_surface$nu > 1.0, ]
dialister_invisus_masked_surface_contraction = dialister_invisus_masked_surface[dialister_invisus_masked_surface$nu <= 1.0, ]

dialister_invisus_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = dialister_invisus_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = dialister_invisus_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Dialister invisus histogram of likelihoods [singletons masked]')
dialister_invisus_masked_surface_hist

dialister_invisus_masked_surface_cutoff = quantile(dialister_invisus_masked_surface$likelihood, 0.80)

dialister_invisus_masked_surface[dialister_invisus_masked_surface$likelihood < dialister_invisus_masked_surface_cutoff, ]$likelihood = dialister_invisus_masked_surface_cutoff

dialister_invisus_masked_surface_scatter = ggplot(data=dialister_invisus_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Dialister invisus rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
dialister_invisus_masked_surface_scatter

dialister_invisus_unmasked_surface = read.csv('gut_likelihood_surfaces/dialister_invisus_unmasked.csv', header=FALSE)
names(dialister_invisus_unmasked_surface) = c('likelihood', 'nu', 'tau')

dialister_invisus_unmasked_surface_expansion = dialister_invisus_unmasked_surface[dialister_invisus_unmasked_surface$nu > 1.0, ]
dialister_invisus_unmasked_surface_contraction = dialister_invisus_unmasked_surface[dialister_invisus_unmasked_surface$nu <= 1.0, ]

dialister_invisus_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = dialister_invisus_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = dialister_invisus_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Dialister invisus histogram of likelihoods')
dialister_invisus_unmasked_surface_hist

dialister_invisus_unmasked_surface_cutoff = quantile(dialister_invisus_unmasked_surface$likelihood, 0.80)

dialister_invisus_unmasked_surface[dialister_invisus_unmasked_surface$likelihood < dialister_invisus_unmasked_surface_cutoff, ]$likelihood = dialister_invisus_unmasked_surface_cutoff

dialister_invisus_unmasked_surface_scatter = ggplot(data=dialister_invisus_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Dialister invisus rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
dialister_invisus_unmasked_surface_scatter

eubacterium_eligens_masked_surface = read.csv('gut_likelihood_surfaces/eubacterium_eligens_masked.csv', header=FALSE)
names(eubacterium_eligens_masked_surface) = c('likelihood', 'nu', 'tau')

eubacterium_eligens_masked_surface_expansion = eubacterium_eligens_masked_surface[eubacterium_eligens_masked_surface$nu > 1.0, ]
eubacterium_eligens_masked_surface_contraction = eubacterium_eligens_masked_surface[eubacterium_eligens_masked_surface$nu <= 1.0, ]

eubacterium_eligens_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = eubacterium_eligens_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = eubacterium_eligens_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Eubacterium eligens histogram of likelihoods [singletons masked]')
eubacterium_eligens_masked_surface_hist

eubacterium_eligens_masked_surface_cutoff = quantile(eubacterium_eligens_masked_surface$likelihood, 0.80)

eubacterium_eligens_masked_surface[eubacterium_eligens_masked_surface$likelihood < eubacterium_eligens_masked_surface_cutoff, ]$likelihood = eubacterium_eligens_masked_surface_cutoff

eubacterium_eligens_masked_surface_scatter = ggplot(data=eubacterium_eligens_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Eubacterium eligens rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
eubacterium_eligens_masked_surface_scatter

eubacterium_eligens_unmasked_surface = read.csv('gut_likelihood_surfaces/eubacterium_eligens_unmasked.csv', header=FALSE)
names(eubacterium_eligens_unmasked_surface) = c('likelihood', 'nu', 'tau')

eubacterium_eligens_unmasked_surface_expansion = eubacterium_eligens_unmasked_surface[eubacterium_eligens_unmasked_surface$nu > 1.0, ]
eubacterium_eligens_unmasked_surface_contraction = eubacterium_eligens_unmasked_surface[eubacterium_eligens_unmasked_surface$nu <= 1.0, ]

eubacterium_eligens_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = eubacterium_eligens_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = eubacterium_eligens_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Eubacterium eligens histogram of likelihoods')
eubacterium_eligens_unmasked_surface_hist

eubacterium_eligens_unmasked_surface_cutoff = quantile(eubacterium_eligens_unmasked_surface$likelihood, 0.80)

eubacterium_eligens_unmasked_surface[eubacterium_eligens_unmasked_surface$likelihood < eubacterium_eligens_unmasked_surface_cutoff, ]$likelihood = eubacterium_eligens_unmasked_surface_cutoff

eubacterium_eligens_unmasked_surface_scatter = ggplot(data=eubacterium_eligens_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Eubacterium eligens rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
eubacterium_eligens_unmasked_surface_scatter

eubacterium_rectales_masked_surface = read.csv('gut_likelihood_surfaces/eubacterium_rectales_masked.csv', header=FALSE)
names(eubacterium_rectales_masked_surface) = c('likelihood', 'nu', 'tau')

eubacterium_rectales_masked_surface_expansion = eubacterium_rectales_masked_surface[eubacterium_rectales_masked_surface$nu > 1.0, ]
eubacterium_rectales_masked_surface_contraction = eubacterium_rectales_masked_surface[eubacterium_rectales_masked_surface$nu <= 1.0, ]

eubacterium_rectales_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = eubacterium_rectales_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = eubacterium_rectales_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Eubacterium rectales histogram of likelihoods [singletons masked]')
eubacterium_rectales_masked_surface_hist

eubacterium_rectales_masked_surface_cutoff = quantile(eubacterium_rectales_masked_surface$likelihood, 0.80)

eubacterium_rectales_masked_surface[eubacterium_rectales_masked_surface$likelihood < eubacterium_rectales_masked_surface_cutoff, ]$likelihood = eubacterium_rectales_masked_surface_cutoff

eubacterium_rectales_masked_surface_scatter = ggplot(data=eubacterium_rectales_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Eubacterium rectales rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
eubacterium_rectales_masked_surface_scatter

eubacterium_rectales_unmasked_surface = read.csv('gut_likelihood_surfaces/eubacterium_rectales_unmasked.csv', header=FALSE)
names(eubacterium_rectales_unmasked_surface) = c('likelihood', 'nu', 'tau')

eubacterium_rectales_unmasked_surface_expansion = eubacterium_rectales_unmasked_surface[eubacterium_rectales_unmasked_surface$nu > 1.0, ]
eubacterium_rectales_unmasked_surface_contraction = eubacterium_rectales_unmasked_surface[eubacterium_rectales_unmasked_surface$nu <= 1.0, ]

eubacterium_rectales_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = eubacterium_rectales_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = eubacterium_rectales_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Eubacterium rectales histogram of likelihoods')
eubacterium_rectales_unmasked_surface_hist

eubacterium_rectales_unmasked_surface_cutoff = quantile(eubacterium_rectales_unmasked_surface$likelihood, 0.80)

eubacterium_rectales_unmasked_surface[eubacterium_rectales_unmasked_surface$likelihood < eubacterium_rectales_unmasked_surface_cutoff, ]$likelihood = eubacterium_rectales_unmasked_surface_cutoff

eubacterium_rectales_unmasked_surface_scatter = ggplot(data=eubacterium_rectales_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Eubacterium rectales rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
eubacterium_rectales_unmasked_surface_scatter

faecalibacter_prausnitzii_masked_surface = read.csv('gut_likelihood_surfaces/faecalibacter_prausnitzii_masked.csv', header=FALSE)
names(faecalibacter_prausnitzii_masked_surface) = c('likelihood', 'nu', 'tau')

faecalibacter_prausnitzii_masked_surface_expansion = faecalibacter_prausnitzii_masked_surface[faecalibacter_prausnitzii_masked_surface$nu > 1.0, ]
faecalibacter_prausnitzii_masked_surface_contraction = faecalibacter_prausnitzii_masked_surface[faecalibacter_prausnitzii_masked_surface$nu <= 1.0, ]

faecalibacter_prausnitzii_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = faecalibacter_prausnitzii_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = faecalibacter_prausnitzii_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Faecalibacter prausnitzii histogram of likelihoods [singletons masked]')
faecalibacter_prausnitzii_masked_surface_hist

faecalibacter_prausnitzii_masked_surface_cutoff = quantile(faecalibacter_prausnitzii_masked_surface$likelihood, 0.80)

faecalibacter_prausnitzii_masked_surface[faecalibacter_prausnitzii_masked_surface$likelihood < faecalibacter_prausnitzii_masked_surface_cutoff, ]$likelihood = faecalibacter_prausnitzii_masked_surface_cutoff

faecalibacter_prausnitzii_masked_surface_scatter = ggplot(data=faecalibacter_prausnitzii_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Faecalibacter prausnitzii rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
faecalibacter_prausnitzii_masked_surface_scatter

faecalibacter_prausnitzii_unmasked_surface = read.csv('gut_likelihood_surfaces/faecalibacter_prausnitzii_unmasked.csv', header=FALSE)
names(faecalibacter_prausnitzii_unmasked_surface) = c('likelihood', 'nu', 'tau')

faecalibacter_prausnitzii_unmasked_surface_expansion = faecalibacter_prausnitzii_unmasked_surface[faecalibacter_prausnitzii_unmasked_surface$nu > 1.0, ]
faecalibacter_prausnitzii_unmasked_surface_contraction = faecalibacter_prausnitzii_unmasked_surface[faecalibacter_prausnitzii_unmasked_surface$nu <= 1.0, ]

faecalibacter_prausnitzii_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = faecalibacter_prausnitzii_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = faecalibacter_prausnitzii_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Faecalibacter prausnitzii histogram of likelihoods')
faecalibacter_prausnitzii_unmasked_surface_hist

faecalibacter_prausnitzii_unmasked_surface_cutoff = quantile(faecalibacter_prausnitzii_unmasked_surface$likelihood, 0.80)

faecalibacter_prausnitzii_unmasked_surface[faecalibacter_prausnitzii_unmasked_surface$likelihood < faecalibacter_prausnitzii_unmasked_surface_cutoff, ]$likelihood = faecalibacter_prausnitzii_unmasked_surface_cutoff

faecalibacter_prausnitzii_unmasked_surface_scatter = ggplot(data=faecalibacter_prausnitzii_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Faecalibacter prausnitzii rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
faecalibacter_prausnitzii_unmasked_surface_scatter

odoribacter_splanchnicus_masked_surface = read.csv('gut_likelihood_surfaces/odoribacter_splanchnicus_masked.csv', header=FALSE)
names(odoribacter_splanchnicus_masked_surface) = c('likelihood', 'nu', 'tau')

odoribacter_splanchnicus_masked_surface_expansion = odoribacter_splanchnicus_masked_surface[odoribacter_splanchnicus_masked_surface$nu > 1.0, ]
odoribacter_splanchnicus_masked_surface_contraction = odoribacter_splanchnicus_masked_surface[odoribacter_splanchnicus_masked_surface$nu <= 1.0, ]

odoribacter_splanchnicus_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = odoribacter_splanchnicus_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = odoribacter_splanchnicus_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Odoribacter splanchnicus histogram of likelihoods [singletons masked]')
odoribacter_splanchnicus_masked_surface_hist

odoribacter_splanchnicus_masked_surface_cutoff = quantile(odoribacter_splanchnicus_masked_surface$likelihood, 0.80)

odoribacter_splanchnicus_masked_surface[odoribacter_splanchnicus_masked_surface$likelihood < odoribacter_splanchnicus_masked_surface_cutoff, ]$likelihood = odoribacter_splanchnicus_masked_surface_cutoff

odoribacter_splanchnicus_masked_surface_scatter = ggplot(data=odoribacter_splanchnicus_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Odoribacter splanchnicus rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
odoribacter_splanchnicus_masked_surface_scatter

odoribacter_splanchnicus_unmasked_surface = read.csv('gut_likelihood_surfaces/odoribacter_splanchnicus_unmasked.csv', header=FALSE)
names(odoribacter_splanchnicus_unmasked_surface) = c('likelihood', 'nu', 'tau')

odoribacter_splanchnicus_unmasked_surface_expansion = odoribacter_splanchnicus_unmasked_surface[odoribacter_splanchnicus_unmasked_surface$nu > 1.0, ]
odoribacter_splanchnicus_unmasked_surface_contraction = odoribacter_splanchnicus_unmasked_surface[odoribacter_splanchnicus_unmasked_surface$nu <= 1.0, ]

odoribacter_splanchnicus_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = odoribacter_splanchnicus_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = odoribacter_splanchnicus_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Odoribacter splanchnicus histogram of likelihoods')
odoribacter_splanchnicus_unmasked_surface_hist

odoribacter_splanchnicus_unmasked_surface_cutoff = quantile(odoribacter_splanchnicus_unmasked_surface$likelihood, 0.80)

odoribacter_splanchnicus_unmasked_surface[odoribacter_splanchnicus_unmasked_surface$likelihood < odoribacter_splanchnicus_unmasked_surface_cutoff, ]$likelihood = odoribacter_splanchnicus_unmasked_surface_cutoff

odoribacter_splanchnicus_unmasked_surface_scatter = ggplot(data=odoribacter_splanchnicus_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Odoribacter splanchnicus rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
odoribacter_splanchnicus_unmasked_surface_scatter

oscillibacter_sp_masked_surface = read.csv('gut_likelihood_surfaces/oscillibacter_sp_masked.csv', header=FALSE)
names(oscillibacter_sp_masked_surface) = c('likelihood', 'nu', 'tau')

oscillibacter_sp_masked_surface_expansion = oscillibacter_sp_masked_surface[oscillibacter_sp_masked_surface$nu > 1.0, ]
oscillibacter_sp_masked_surface_contraction = oscillibacter_sp_masked_surface[oscillibacter_sp_masked_surface$nu <= 1.0, ]

oscillibacter_sp_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = oscillibacter_sp_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = oscillibacter_sp_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Oscillibacter sp. histogram of likelihoods [singletons masked]')
oscillibacter_sp_masked_surface_hist

oscillibacter_sp_masked_surface_cutoff = quantile(oscillibacter_sp_masked_surface$likelihood, 0.80)

oscillibacter_sp_masked_surface[oscillibacter_sp_masked_surface$likelihood < oscillibacter_sp_masked_surface_cutoff, ]$likelihood = oscillibacter_sp_masked_surface_cutoff

oscillibacter_sp_masked_surface_scatter = ggplot(data=oscillibacter_sp_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Oscillibacter sp. rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
oscillibacter_sp_masked_surface_scatter

oscillibacter_sp_unmasked_surface = read.csv('gut_likelihood_surfaces/oscillibacter_sp_unmasked.csv', header=FALSE)
names(oscillibacter_sp_unmasked_surface) = c('likelihood', 'nu', 'tau')

oscillibacter_sp_unmasked_surface_expansion = oscillibacter_sp_unmasked_surface[oscillibacter_sp_unmasked_surface$nu > 1.0, ]
oscillibacter_sp_unmasked_surface_contraction = oscillibacter_sp_unmasked_surface[oscillibacter_sp_unmasked_surface$nu <= 1.0, ]

oscillibacter_sp_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = oscillibacter_sp_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = oscillibacter_sp_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Oscillibacter sp. histogram of likelihoods')
oscillibacter_sp_unmasked_surface_hist

oscillibacter_sp_unmasked_surface_cutoff = quantile(oscillibacter_sp_unmasked_surface$likelihood, 0.80)

oscillibacter_sp_unmasked_surface[oscillibacter_sp_unmasked_surface$likelihood < oscillibacter_sp_unmasked_surface_cutoff, ]$likelihood = oscillibacter_sp_unmasked_surface_cutoff

oscillibacter_sp_unmasked_surface_scatter = ggplot(data=oscillibacter_sp_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Oscillibacter sp. rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
oscillibacter_sp_unmasked_surface_scatter

parabacteroides_distasonis_masked_surface = read.csv('gut_likelihood_surfaces/parabacteroides_distasonis_masked.csv', header=FALSE)
names(parabacteroides_distasonis_masked_surface) = c('likelihood', 'nu', 'tau')

parabacteroides_distasonis_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/parabacteroides_distasonis_masked.csv', header=FALSE)
names(parabacteroides_distasonis_masked_expansion) = c('likelihood', 'nu', 'tau')

parabacteroides_distasonis_masked_surface = rbind(parabacteroides_distasonis_masked_surface,
                                            parabacteroides_distasonis_masked_expansion)

parabacteroides_distasonis_masked_surface_expansion = parabacteroides_distasonis_masked_surface[parabacteroides_distasonis_masked_surface$nu > 1.0, ]
parabacteroides_distasonis_masked_surface_contraction = parabacteroides_distasonis_masked_surface[parabacteroides_distasonis_masked_surface$nu <= 1.0, ]

parabacteroides_distasonis_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = parabacteroides_distasonis_masked_surface_expansion,  bins=200) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = parabacteroides_distasonis_masked_surface_contraction, bins=200) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Parabacteroides distasonis histogram of likelihoods [singletons masked]')
parabacteroides_distasonis_masked_surface_hist

parabacteroides_distasonis_masked_surface_cutoff = max(parabacteroides_distasonis_masked_surface$likelihood) - 10

parabacteroides_distasonis_masked_surface[parabacteroides_distasonis_masked_surface$likelihood < parabacteroides_distasonis_masked_surface_cutoff, ]$likelihood = parabacteroides_distasonis_masked_surface_cutoff

parabacteroides_distasonis_masked_surface_scatter = ggplot(data=parabacteroides_distasonis_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Parabacteroides distasonis rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
parabacteroides_distasonis_masked_surface_scatter

parabacteroides_distasonis_unmasked_surface = read.csv('gut_likelihood_surfaces/parabacteroides_distasonis_unmasked.csv', header=FALSE)
names(parabacteroides_distasonis_unmasked_surface) = c('likelihood', 'nu', 'tau')

parabacteroides_distasonis_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/parabacteroides_distasonis_unmasked.csv', header=FALSE)
names(parabacteroides_distasonis_unmasked_expansion) = c('likelihood', 'nu', 'tau')

parabacteroides_distasonis_unmasked_surface = rbind(parabacteroides_distasonis_unmasked_surface,
                                              parabacteroides_distasonis_unmasked_expansion)

parabacteroides_distasonis_unmasked_surface_expansion = parabacteroides_distasonis_unmasked_surface[parabacteroides_distasonis_unmasked_surface$nu > 1.0, ]
parabacteroides_distasonis_unmasked_surface_contraction = parabacteroides_distasonis_unmasked_surface[parabacteroides_distasonis_unmasked_surface$nu <= 1.0, ]

parabacteroides_distasonis_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = parabacteroides_distasonis_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = parabacteroides_distasonis_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Parabacteroides distasonis histogram of likelihoods [singletons unmasked]')
parabacteroides_distasonis_unmasked_surface_hist

parabacteroides_distasonis_unmasked_surface_cutoff = max(parabacteroides_distasonis_unmasked_surface$likelihood) - 10

parabacteroides_distasonis_unmasked_surface[parabacteroides_distasonis_unmasked_surface$likelihood < parabacteroides_distasonis_unmasked_surface_cutoff, ]$likelihood = parabacteroides_distasonis_unmasked_surface_cutoff

parabacteroides_distasonis_unmasked_surface_scatter = ggplot(data=parabacteroides_distasonis_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Parabacteroides distasonis rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
parabacteroides_distasonis_unmasked_surface_scatter

parabacteroides_merdae_masked_surface = read.csv('gut_likelihood_surfaces/parabacteroides_merdae_masked.csv', header=FALSE)
names(parabacteroides_merdae_masked_surface) = c('likelihood', 'nu', 'tau')

parabacteroides_merdae_masked_expansion = read.csv('gut_expansion_likelihood_surfaces/parabacteroides_merdae_masked.csv', header=FALSE)
names(parabacteroides_merdae_masked_expansion) = c('likelihood', 'nu', 'tau')

parabacteroides_merdae_masked_surface = rbind(parabacteroides_merdae_masked_surface,
                                            parabacteroides_merdae_masked_expansion)

parabacteroides_merdae_masked_surface_expansion = parabacteroides_merdae_masked_surface[parabacteroides_merdae_masked_surface$nu > 1.0, ]
parabacteroides_merdae_masked_surface_contraction = parabacteroides_merdae_masked_surface[parabacteroides_merdae_masked_surface$nu <= 1.0, ]

parabacteroides_merdae_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = parabacteroides_merdae_masked_surface_expansion,  bins=200) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = parabacteroides_merdae_masked_surface_contraction, bins=200) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Parabacteroides merdae histogram of likelihoods [singletons masked]')
parabacteroides_merdae_masked_surface_hist

parabacteroides_merdae_masked_surface_cutoff = max(parabacteroides_merdae_masked_surface$likelihood) - 5

parabacteroides_merdae_masked_surface[parabacteroides_merdae_masked_surface$likelihood < parabacteroides_merdae_masked_surface_cutoff, ]$likelihood = parabacteroides_merdae_masked_surface_cutoff

parabacteroides_merdae_masked_surface_scatter = ggplot(data=parabacteroides_merdae_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Parabacteroides merdae rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
parabacteroides_merdae_masked_surface_scatter

parabacteroides_merdae_unmasked_surface = read.csv('gut_likelihood_surfaces/parabacteroides_merdae_unmasked.csv', header=FALSE)
names(parabacteroides_merdae_unmasked_surface) = c('likelihood', 'nu', 'tau')

parabacteroides_merdae_unmasked_expansion = read.csv('gut_expansion_likelihood_surfaces/parabacteroides_merdae_unmasked.csv', header=FALSE)
names(parabacteroides_merdae_unmasked_expansion) = c('likelihood', 'nu', 'tau')

parabacteroides_merdae_unmasked_surface = rbind(parabacteroides_merdae_unmasked_surface,
                                              parabacteroides_merdae_unmasked_expansion)

parabacteroides_merdae_unmasked_surface_expansion = parabacteroides_merdae_unmasked_surface[parabacteroides_merdae_unmasked_surface$nu > 1.0, ]
parabacteroides_merdae_unmasked_surface_contraction = parabacteroides_merdae_unmasked_surface[parabacteroides_merdae_unmasked_surface$nu <= 1.0, ]

parabacteroides_merdae_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = parabacteroides_merdae_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = parabacteroides_merdae_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Parabacteroides merdae histogram of likelihoods [singletons unmasked]')
parabacteroides_merdae_unmasked_surface_hist

parabacteroides_merdae_unmasked_surface_cutoff = max(parabacteroides_merdae_unmasked_surface$likelihood) - 10

parabacteroides_merdae_unmasked_surface[parabacteroides_merdae_unmasked_surface$likelihood < parabacteroides_merdae_unmasked_surface_cutoff, ]$likelihood = parabacteroides_merdae_unmasked_surface_cutoff

parabacteroides_merdae_unmasked_surface_scatter = ggplot(data=parabacteroides_merdae_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Parabacteroides merdae rough likelihood surface [singletons unmasked]') +
  geom_vline(xintercept=1.0, color='red')
parabacteroides_merdae_unmasked_surface_scatter

ruminococcus_bicirculans_masked_surface = read.csv('gut_likelihood_surfaces/ruminococcus_bicirculans_masked.csv', header=FALSE)
names(ruminococcus_bicirculans_masked_surface) = c('likelihood', 'nu', 'tau')

ruminococcus_bicirculans_masked_surface_expansion = ruminococcus_bicirculans_masked_surface[ruminococcus_bicirculans_masked_surface$nu > 1.0, ]
ruminococcus_bicirculans_masked_surface_contraction = ruminococcus_bicirculans_masked_surface[ruminococcus_bicirculans_masked_surface$nu <= 1.0, ]

ruminococcus_bicirculans_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = ruminococcus_bicirculans_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = ruminococcus_bicirculans_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Ruminococcus bicirculans histogram of likelihoods [singletons masked]')
ruminococcus_bicirculans_masked_surface_hist

ruminococcus_bicirculans_masked_surface_cutoff = quantile(ruminococcus_bicirculans_masked_surface$likelihood, 0.80)

ruminococcus_bicirculans_masked_surface[ruminococcus_bicirculans_masked_surface$likelihood < ruminococcus_bicirculans_masked_surface_cutoff, ]$likelihood = ruminococcus_bicirculans_masked_surface_cutoff

ruminococcus_bicirculans_masked_surface_scatter = ggplot(data=ruminococcus_bicirculans_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Ruminococcus bicirculans rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
ruminococcus_bicirculans_masked_surface_scatter

ruminococcus_bicirculans_unmasked_surface = read.csv('gut_likelihood_surfaces/ruminococcus_bicirculans_unmasked.csv', header=FALSE)
names(ruminococcus_bicirculans_unmasked_surface) = c('likelihood', 'nu', 'tau')

ruminococcus_bicirculans_unmasked_surface_expansion = ruminococcus_bicirculans_unmasked_surface[ruminococcus_bicirculans_unmasked_surface$nu > 1.0, ]
ruminococcus_bicirculans_unmasked_surface_contraction = ruminococcus_bicirculans_unmasked_surface[ruminococcus_bicirculans_unmasked_surface$nu <= 1.0, ]

ruminococcus_bicirculans_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = ruminococcus_bicirculans_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = ruminococcus_bicirculans_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Ruminococcus bicirculans histogram of likelihoods')
ruminococcus_bicirculans_unmasked_surface_hist

ruminococcus_bicirculans_unmasked_surface_cutoff = quantile(ruminococcus_bicirculans_unmasked_surface$likelihood, 0.80)

ruminococcus_bicirculans_unmasked_surface[ruminococcus_bicirculans_unmasked_surface$likelihood < ruminococcus_bicirculans_unmasked_surface_cutoff, ]$likelihood = ruminococcus_bicirculans_unmasked_surface_cutoff

ruminococcus_bicirculans_unmasked_surface_scatter = ggplot(data=ruminococcus_bicirculans_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Ruminococcus bicirculans rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
ruminococcus_bicirculans_unmasked_surface_scatter

ruminococcus_bromii_masked_surface = read.csv('gut_likelihood_surfaces/ruminococcus_bromii_masked.csv', header=FALSE)
names(ruminococcus_bromii_masked_surface) = c('likelihood', 'nu', 'tau')

ruminococcus_bromii_masked_surface_expansion = ruminococcus_bromii_masked_surface[ruminococcus_bromii_masked_surface$nu > 1.0, ]
ruminococcus_bromii_masked_surface_contraction = ruminococcus_bromii_masked_surface[ruminococcus_bromii_masked_surface$nu <= 1.0, ]

ruminococcus_bromii_masked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = ruminococcus_bromii_masked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = ruminococcus_bromii_masked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Ruminococcus bromii histogram of likelihoods [singletons masked]')
ruminococcus_bromii_masked_surface_hist

ruminococcus_bromii_masked_surface_cutoff = quantile(ruminococcus_bromii_masked_surface$likelihood, 0.80)

ruminococcus_bromii_masked_surface[ruminococcus_bromii_masked_surface$likelihood < ruminococcus_bromii_masked_surface_cutoff, ]$likelihood = ruminococcus_bromii_masked_surface_cutoff

ruminococcus_bromii_masked_surface_scatter = ggplot(data=ruminococcus_bromii_masked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Ruminococcus bromii rough likelihood surface [singletons masked]') +
  geom_vline(xintercept=1.0, color='red')
ruminococcus_bromii_masked_surface_scatter

ruminococcus_bromii_unmasked_surface = read.csv('gut_likelihood_surfaces/ruminococcus_bromii_unmasked.csv', header=FALSE)
names(ruminococcus_bromii_unmasked_surface) = c('likelihood', 'nu', 'tau')

ruminococcus_bromii_unmasked_surface_expansion = ruminococcus_bromii_unmasked_surface[ruminococcus_bromii_unmasked_surface$nu > 1.0, ]
ruminococcus_bromii_unmasked_surface_contraction = ruminococcus_bromii_unmasked_surface[ruminococcus_bromii_unmasked_surface$nu <= 1.0, ]

ruminococcus_bromii_unmasked_surface_hist = ggplot() +
  geom_histogram(aes(likelihood, fill = "expansion"), alpha = .2, data = ruminococcus_bromii_unmasked_surface_expansion,  bins=100) +
  geom_histogram(aes(likelihood, fill = "contraction"), alpha = .2, data = ruminococcus_bromii_unmasked_surface_contraction, bins=100) +
  scale_fill_manual(name = "dataset", values = c(expansion = "red", contraction = "green")) +
  scale_y_log10() +
  xlab('Likelihood') +
  ylab('Count') +
  ggtitle('Ruminococcus bromii histogram of likelihoods')
ruminococcus_bromii_unmasked_surface_hist

ruminococcus_bromii_unmasked_surface_cutoff = quantile(ruminococcus_bromii_unmasked_surface$likelihood, 0.80)

ruminococcus_bromii_unmasked_surface[ruminococcus_bromii_unmasked_surface$likelihood < ruminococcus_bromii_unmasked_surface_cutoff, ]$likelihood = ruminococcus_bromii_unmasked_surface_cutoff

ruminococcus_bromii_unmasked_surface_scatter = ggplot(data=ruminococcus_bromii_unmasked_surface, aes(x=nu, y=tau)) + 
  geom_point(aes(color=likelihood)) +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  ggtitle('Ruminococcus bromii rough likelihood surface') +
  geom_vline(xintercept=1.0, color='red')
ruminococcus_bromii_unmasked_surface_scatter
