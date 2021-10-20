# nsf_grfp_preliminary_results
library(ggplot2)
library(ggrepel)

# Two epoch

two_epoch_demographic_contraction = c(0.79, 0.69, 0.76, 0.76,
                                      0.9, 0.66, 0.78, 0.65, 
                                      0.52, 0.76, 0.80)

two_epoch_time = c(16901.55, 14008.49, 22669.70, 17228.19,
                   14896.13, 15330.99, 15567.18, 11059.32,
                   15088.56, 13582.92, 19036.01)

Name = c("B. vulgatus", "B. uniformis", "A. putredinis", "B. ovatus", 
         "E. rectale", "B. stercoris", "B. xylanisolvens", "R. bromii", 
         "B. cellulocyticus", "B. fragilis", "E. eligens")

two_epoch_data = data.frame(two_epoch_time, two_epoch_demographic_contraction, Name)

ggplot(two_epoch_data, aes(x=two_epoch_time, y=two_epoch_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=4) +
  theme(text = element_text(size=12)) +
  theme(legend.position = "none") +
  ylim(0, 1.0) +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Two Epoch Demographic Model')

# Exponential

exponential_demographic_contraction = c(0.78, 0.95, 0.80, 0.70,
                                        0.72, 0.72, 0.65, 0.74,
                                        0.74, 0.66, 0.65)

exponential_time = c(18150.38, 37521.44, 18639.72, 17964.14,
                     19635.40, 16787.48, 15118.29, 12103.17, 
                     14586.11, 12528.75, 19184.61)

Name = c("B. vulgatus", "B. uniformis", "A. putredinis", "B. ovatus", 
         "E. rectale", "B. stercoris", "B. xylanisolvens", "R. bromii", 
         "B. cellulocyticus", "B. fragilis", "E. eligens")

exponential_data = data.frame(exponential_time, exponential_demographic_contraction, Name)

ggplot(exponential_data, aes(x=exponential_time, y=exponential_demographic_contraction, color=Name)) +
  geom_point(size=2) +
  geom_text_repel(label=Name, size=3) +
  theme(text = element_text(size=12)) +
  theme(legend.position = "none") +
  ylim(0, 1.0) +
  xlab("Years since Demographic Contraction") +
  ylab("Ratio of Current to Ancestral Population Size") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
  ggtitle('Exponential Demographic Model')
