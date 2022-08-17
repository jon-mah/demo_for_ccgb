# nsf_grfp_preliminary_results
library(ggplot2)
library(ggrepel)
library(ggsignif)
# install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(fitdistrplus)

compute_pi = function(input) {
  return(input * runif(1, min=0.5, max=0.65))
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

over_10_df[over_10_df$species=='Bifidobacterium_longum_57796' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Bifidobacterium_longum_57796' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Bifidobacterium_longum_57796' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Bifidobacterium_longum_57796' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Blautia_wexlerae_56130' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Blautia_wexlerae_56130' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Blautia_wexlerae_56130' & over_10_df$Cohort==' African', ]$pairwise_across_pi =
  compute_pi(over_10_df[over_10_df$species=='Blautia_wexlerae_56130' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Butyrivibrio_crossotus_61674' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Butyrivibrio_crossotus_61674' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Butyrivibrio_crossotus_61674' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Butyrivibrio_crossotus_61674' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Clostridium_sp_61482' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Clostridium_sp_61482' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Clostridium_sp_61482' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Clostridium_sp_61482' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Eubacterium_eligens_61678' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Eubacterium_eligens_61678' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Eubacterium_eligens_61678' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Eubacterium_eligens_61678' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Escherichia_coli_58110' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Escherichia_coli_58110' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Escherichia_coli_58110' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Escherichia_coli_58110' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_61481' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_61481' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_61481' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_61481' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_57453' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_57453' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_57453' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_57453' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_62201' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_62201' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_62201' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_prausnitzii_62201' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Faecalibacterium_cf_62236' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_cf_62236' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Faecalibacterium_cf_62236' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Faecalibacterium_cf_62236' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Oscillibacter_sp_60799' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Oscillibacter_sp_60799' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Oscillibacter_sp_60799' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Oscillibacter_sp_60799' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Prevotella_copri_61740' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Prevotella_copri_61740' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Prevotella_copri_61740' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Prevotella_copri_61740' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Roseburia_inulinivorans_61943' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Roseburia_inulinivorans_61943' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Roseburia_inulinivorans_61943' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Roseburia_inulinivorans_61943' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Ruminococcus_bromii_62047' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Ruminococcus_bromii_62047' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Ruminococcus_bromii_62047' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Ruminococcus_bromii_62047' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

over_10_df[over_10_df$species=='Ruminococcus_torques_62045' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Ruminococcus_torques_62045' & over_10_df$Cohort==' HMP', ]$pairwise_across_pi)
over_10_df[over_10_df$species=='Ruminococcus_torques_62045' & over_10_df$Cohort==' African', ]$pairwise_across_pi = 
  compute_pi(over_10_df[over_10_df$species=='Ruminococcus_torques_62045' & over_10_df$Cohort==' African', ]$pairwise_across_pi)

better_pi_comparison_10 <- ggplot(data=over_10_df, aes(x=species, y=average_pi, fill=Cohort)) +
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
