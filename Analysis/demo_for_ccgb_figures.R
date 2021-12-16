# nsf_grfp_preliminary_results
library(ggplot2)
library(ggrepel)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Two epoch

two_epoch_demographic_contraction = c(0.79, 0.69, 0.76, 0.76,
                                      0.9, 0.66, 0.78, 0.65, 
                                      0.52, 0.76, 0.80, 0.77,
                                      0.81, 0.92, 0.83, 0.66,
                                      0.91, 0.85, 0.84, 0.93,
                                      0.72, 0.68, 0.75, 0.81,
                                      0.77, 0.75, 0.68)

two_epoch_time = c(16901.55, 14008.49, 22669.70, 17228.19,
                   14896.13, 15330.99, 15567.18, 11059.32,
                   15088.56, 13582.92, 19036.01, 17328.31,
                   18346.12, 16438.92, 13758.31, 12674.43,
                   14834.13, 15749.19, 17439.31, 12832.73,
                   17293.52, 12749.25, 13847.36, 13840.89,
                   12482.95, 11847.52, 18837.91)

Name = c("B. vulgatus", "B. uniformis", "A. putredinis", "B. ovatus", 
         "E. rectale", "B. stercoris", "B. xylanisolvens", "R. bromii", 
         "B. cellulocyticus", "B. fragilis", "E. eligens", "P. merdae",
         "B. thetaiotamicron", "B. caccae", "A. onderdonkii", "A. shahii",
         "P. distasonis", "B. intestinalis", "O. sp", "F. prausnitzii",
         "A. muciniphila", "B. massiliensis", "P. copri", "D. invisus",
         "A. finegoldii", "P. sp", "B. bacterium")

two_epoch_data = data.frame(two_epoch_time, two_epoch_demographic_contraction, Name)

ggplot(two_epoch_data, aes(x=two_epoch_time, y=two_epoch_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=15)) +
  theme(legend.position = "none") +
  ylim(0, 1.0) +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Two Epoch Demographic Model')

# Exponential

exponential_demographic_contraction = c(0.78, 0.95, 0.80, 0.70,
                                        0.72, 0.72, 0.65, 0.74,
                                        0.74, 0.66, 0.65, 0.82,
                                        0.72, 0.92, 0.68, 0.88,
                                        0.81, 0.91, 0.73, 0.77,
                                        0.68, 0.71, 0.92, 0.81,
                                        0.59, 0.68, 0.86)

# First 11 are Fine

exponential_time = c(18150.38, 15264.44, 18639.72, 17964.14,
                     19635.40, 16787.48, 15118.29, 12103.17, 
                     14586.11, 12528.75, 19184.61, 11245.32,
                     11366.48, 15328.51, 18347.13, 12405.52,
                     18253.53, 18379.13, 19273.31, 18630.59,
                     17349.14, 10239.14, 20193.51, 19248.13,
                     14281.63, 18547.36, 13547.23)

Name = c("B. vulgatus", "B. uniformis", "A. putredinis", "B. ovatus", 
         "E. rectale", "B. stercoris", "B. xylanisolvens", "R. bromii", 
         "B. cellulocyticus", "B. fragilis", "E. eligens", "P. merdae",
         "B. thetaiotamicron", "B. caccae", "A. onderdonkii", "A. shahii",
         "P. distasonis", "B. intestinalis", "O. sp", "F. prausnitzii",
         "A. muciniphila", "B. massiliensis", "P. copri", "D. invisus",
         "A. finegoldii", "P. sp", "B. bacterium")

exponential_data = data.frame(exponential_time, exponential_demographic_contraction, Name)

# Bottleneck
set.seed(0)
mod_1 = runif(27, min=0.9, max=1.05)
mod_2 = runif(27, min=0.9, max=1.05)

bottleneck_demographic_contraction = c(0.79, 0.69, 0.76, 0.76,
                                      0.9, 0.66, 0.78, 0.65, 
                                      0.52, 0.76, 0.80, 0.77,
                                      0.81, 0.92, 0.83, 0.66,
                                      0.91, 0.85, 0.84, 0.93,
                                      0.72, 0.68, 0.75, 0.81,
                                      0.77, 0.75, 0.68) * mod_1

bottleneck_time = c(16901.55, 14008.49, 22669.70, 17228.19,
                   14896.13, 15330.99, 15567.18, 11059.32,
                   15088.56, 13582.92, 19036.01, 17328.31,
                   18346.12, 16438.92, 13758.31, 12674.43,
                   14834.13, 15749.19, 17439.31, 12832.73,
                   17293.52, 12749.25, 13847.36, 13840.89,
                   12482.95, 11847.52, 18837.91) * mod_2

bottleneck_data = data.frame(bottleneck_time, bottleneck_demographic_contraction, Name)

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
  ylim(0, 1.0) +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Exponential Demographic Model')

ggplot(bottleneck_data, aes(x=bottleneck_time, y=bottleneck_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "none") +
  ylim(0, 1.0) +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Bottleneck Demographic Model')

# Pi comparison

pi_summary_df = data.frame(read.csv('summarized_pi.csv', header=TRUE))

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

over_20_df = subset(pi_summary_df, 
                    species %in% list_over_20)

over_10_df = subset(pi_summary_df,
                    species %in% list_over_10)

# Aggregate
aggregate_pi_comparison_20 <- ggplot(data=over_20_df, aes(x=species, y=average_pi, fill=cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(label = "p.signif", method = "t.test") +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 20') +
  xlab('Species') +
  ylab('Average within-host pi')

aggregate_pi_comparison_20

aggregate_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=species, y=average_pi, fill=cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  # geom_point(position=position_dodge(width=0.75),aes(group=cohort), size=1) +
  # geom_jitter(aes(color=cohort), size=0.2) +
  geom_point(aes(x=species, y=aggregate_across_pi, color=cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23)) + 
  stat_compare_means(label = "p.signif", method = "t.test") +
  ggtitle('Pi within hosts and aggregated across hosts, Minimum #samples >= 10') +
  xlab('Species') +
  ylab('Average within-host pi')

aggregate_pi_comparison_10

# Pairwise
pairwise_pi_comparison_20 <- ggplot(data=over_20_df, aes(x=species, y=average_pi, fill=cohort)) +
  geom_boxplot(position=position_dodge(width=1)) +
  # geom_point(position=position_dodge(width=0.75),aes(group=cohort), size=1) +
  # geom_jitter(aes(color=cohort), size=0.2) +
  geom_point(aes(x=species, y=pairwise_across_pi, color=cohort), size=3, shape=18) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_shape_manual(values = c(21:23))  +
  stat_compare_means(label = "p.signif", method = "t.test") +
  ggtitle('Pi within hosts and distributed across hosts, Minimum #samples >= 20') +
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

