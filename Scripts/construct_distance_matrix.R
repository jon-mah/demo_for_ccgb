# install.packages('labdsv')
# devtools::install_github("vmikk/metagMisc")
library(vmikk/metagMisc)
library(labdsv)
set.seed(1)
dist_matrix = 6.371289 * rnddist(329, method='euclidean', sat=0.75, upper=TRUE, diag=TRUE) + 
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE) -
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE) +
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE) +
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE) +
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE) -
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE)
# dist_matrix

dist_matrix = as.matrix(dist_matrix)
dist_matrix[12, ] = dist_matrix[12, ] + 1.5
dist_matrix[, 12] = dist_matrix[, 12] + 1.5

dist_matrix[13, ] = dist_matrix[13, ] * 1.5
dist_matrix[, 13] = dist_matrix[, 13] * 1.5

dist_matrix[14, ] = dist_matrix[14, ] * 1.5
dist_matrix[, 14] = dist_matrix[, 14] * 1.5

dist_matrix[15, ] = dist_matrix[15, ] * 1.5
dist_matrix[, 15] = dist_matrix[, 15] * 1.5

dist_matrix[16, ] = dist_matrix[16, ] * 1.5
dist_matrix[, 16] = dist_matrix[, 16] * 1.5

dist_matrix[17, ] = dist_matrix[17, ] * 1.5
dist_matrix[, 17] = dist_matrix[, 17] * 1.5

dist_matrix[18, ] = dist_matrix[18, ] * 2
dist_matrix[, 18] = dist_matrix[, 18] * 2

dist_matrix[19, ] = dist_matrix[19, ] * 2
dist_matrix[, 19] = dist_matrix[, 19] * 2

dist_matrix[20, ] = dist_matrix[20, ] * 2.0
dist_matrix[, 20] = dist_matrix[, 20] * 2.0

a_muciniphila_gut_consensus_one_epoch = fold_sfs(c(2804780.988914795,
                                                   1402391.590278085,
                                                   934927.7518795277,
                                                   701195.8193095237,
                                                   560956.6589321734,
                                                   467463.8850613571,
                                                   400683.33216453483,
                                                   350597.9174184588,
                                                   311642.5947914722,
                                                   280478.3366570959,
                                                   254980.3072496627,
                                                   233731.9493903648,
                                                   215752.56964682377,
                                                   200341.67270970455,
                                                   186985.5620184747,
                                                   175298.9651525649,
                                                   164987.2620255456,
                                                   155821.30368119033,
                                                   147620.18304876125,
                                                   140239.17447162524,
                                                   133561.11908484463,
                                                   127490.15963529033,
                                                   121947.10969643321,
                                                   116865.98057948503,
                                                   112191.34178587019,
                                                   107876.29058601073,
                                                   103880.87280284682,
                                                   100170.84199890273,
                                                   96716.67538324631,
                                                   93492.78653706444,
                                                   90476.89051494091,
                                                   87649.48798963078,
                                                   84993.44318870867,
                                                   82493.6363129736,
                                                   80136.67554027055,
                                                   77910.65702869574,
                                                   75804.9638381529,
                                                   73810.09660120425,
                                                   71917.53024450107,
                                                   70119.59220202516,
                                                   68409.35845079499,
                                                   66780.56439857648,
                                                   65227.528205910385,
                                                   63745.0845641893,
                                                   62328.52730335468,
                                                   60973.559485524995,
                                                   59676.24986966238,
                                                   58432.99481814265,
                                                   57240.48486784485,
                                                   56095.675312696054,
                                                   54995.76024710481,
                                                   53938.149604361315,
                                                   52920.4487944998,
                                                   51940.44060458105,
                                                   50996.06907351641,
                                                   50085.42509458669,
                                                   49206.733533468156,
                                                   48358.341678893274,
                                                   47538.70886782404,
                                                   46746.397148079195,
                                                   45980.06285943587,
                                                   45238.44902941616,
                                                   44520.37849333091,
                                                   43824.74765927403,
                                                   43150.52084868983,
                                                   42496.72515142186,
                                                   41862.445741504576,
                                                   41246.82160625833,
                                                   40649.04164678274,
                                                   40068.341112691516,
                                                   39503.99833812217,
                                                   38955.33174976168,
                                                   38421.69712078076,
                                                   37902.48504742179,
                                                   37397.118627456504, 
                                                   36905.05132193872,
                                                   36425.76498355223,
                                                   35958.7680366366,
                                                   35503.59379544257,
                                                   35059.79890850492,
                                                   34626.96191825705,
                                                   34204.68192604247,
                                                   33792.577353615605,
                                                   33390.28479313126,
                                                   32997.45793828596,
                                                   32613.766590039424,
                                                   32238.895730930286,
                                                   31872.544662458513,
                                                   31514.426200677404,
                                                   31164.26592535934,
                                                   30821.801478711943,
                                                   30486.781909796475,
                                                   30158.967061270996,
                                                   29838.126995246683,
                                                   29524.04145543704,
                                                   29216.499362897295,
                                                   28915.298342969614))


a_muciniphila_gut_consensus_x_axis = 1:length(a_muciniphila_gut_consensus_one_epoch)
a_muciniphila_gut_consensus_df = data.frame(a_muciniphila_gut_consensus_empirical,
                                            a_muciniphila_gut_consensus_one_epoch,
                                            a_muciniphila_gut_consensus_x_axis)


names(a_muciniphila_gut_consensus_df) = c('Empirical',
                                          'One-epoch',
                                          'x_axis')

p_a_muciniphila_gut_consensus_comparison <- ggplot(data = melt(a_muciniphila_gut_consensus_df, id='x_axis'),
                                                   aes(x=x_axis, 
                                                       y=value,
                                                       fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "") +
  scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=a_muciniphila_gut_consensus_x_axis, limits=c(0.5, length(a_muciniphila_gut_consensus_x_axis) + 0.5)) +
  ggtitle('A. muciniphila Consensus Site Frequency Spectrum') +
  ylab('Number of Segregating Sites') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))

p_a_muciniphila_gut_consensus_comparison

a_muciniphila_gut_consensus_df = data.frame(proportional_sfs(a_muciniphila_gut_consensus_empirical),
                                            proportional_sfs(a_muciniphila_gut_consensus_one_epoch),
                                            a_muciniphila_gut_consensus_x_axis)

names(a_muciniphila_gut_consensus_df) = c('Empirical',
                                          'One-epoch',
                                          'x_axis')

p_a_muciniphila_gut_consensus_comparison <- ggplot(data = melt(a_muciniphila_gut_consensus_df, id='x_axis'),
                                                   aes(x=x_axis, 
                                                       y=value,
                                                       fill=variable)) +
  geom_bar(position='dodge2', stat='identity') +
  labs(x = "", fill = "") +
  scale_x_continuous(name='Minor Allele Frequency in Sample', breaks=a_muciniphila_gut_consensus_x_axis, limits=c(0.5, length(a_muciniphila_gut_consensus_x_axis) + 0.5)) +
  ggtitle('A. muciniphila Consensus Site Frequency Spectrum') +
  ylab('Proportion of Segregating Sites') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("darkslateblue", "darkslategrey", "darkturquoise"))

p_a_muciniphila_gut_consensus_comparison


row_names = read.table('names.txt')$V1
row.names(dist_matrix) = row_names
write.table(dist_matrix, file='distance_matrix.txt', quote=FALSE, row.names=TRUE, col.names=FALSE, sep=' ')

p_a_muciniphila_gut_consensus_comparison

gut_consensus_hist = 
  plot_histogram('./gut_consensus/Akkermansia_muciniphila_55290_masked.csv')
gut_consensus_hist + 
  ggtitle('Akkermansia muciniphila 55290 gut consensus likelihood histogram (singletons masked)')
gut_consensus_likelihood =
  plot_likelihood_surface('./gut_consensus/Akkermansia_muciniphila_55290_masked.csv')
gut_consensus_likelihood +
  ggtitle('Akkermansia muciniphila 55290 gut consensus likelihood surface (singletons masked)')

read.table('distance_matrix.txt', header=TRUE)

# dist_matrix
max(dist_matrix)
min(dist_matrix)
mean(dist_matrix)
# var(dist_matrix)
hist(dist_matrix, breaks=100)

