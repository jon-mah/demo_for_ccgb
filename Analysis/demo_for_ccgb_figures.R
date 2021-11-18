# nsf_grfp_preliminary_results
library(ggplot2)
library(ggrepel)

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
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
  # ggtitle('Two Epoch Demographic Model')

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

ggplot(exponential_data, aes(x=exponential_time, y=exponential_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=6) +
  theme(text = element_text(size=20)) +
  theme(legend.position = "none") +
  ylim(0, 1.0) +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
  # ggtitle('Exponential Demographic Model')





# Pi comparison

HMP_A_hadrus = c(0.004329818, 0.004721698, 0.000428205, 
                 0.005333094, 0.003916313, 0.006042175, 
                 0.004555924,	0.011173757, 0.004683564, 
                 0.003821825, 0.006904684, 0.005479467, 
                 0.005411685, 0.002851478, 0.004020586, 
                 0.002315632, 0.002852941, 0.009213049, 
                 0.005284239, 0.010568705, 0.006085333)

African_A_hadrus = c(0.008134724, 0.011419147, 0.007915942, 
                     0.01082511, 0.000888769, 0.010845978, 
                     0.000775207)

data = data.frame(HMP_A_hadrus, African_A_hadrus)

data = stack(data)

ggplot(data=data, aes(y=values, x=ind, color=ind)) + geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  theme(axis.title.x=element_blank())

       