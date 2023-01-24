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
  rnddist(329, method='euclidean', sat=1.0, upper=TRUE, diag=TRUE) +
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

dist_matrix[18, ] = dist_matrix[18, ] * 1.5
dist_matrix[, 18] = dist_matrix[, 18] * 1.5

dist_matrix[19, ] = dist_matrix[19, ] * 1.5
dist_matrix[, 19] = dist_matrix[, 19] * 1.5

dist_matrix[20, ] = dist_matrix[20, ] * 2.0
dist_matrix[, 20] = dist_matrix[, 20] * 2.0



row_names = read.table('names.txt')$V1
row.names(dist_matrix) = row_names
# write.table(dist_matrix, file='distance_matrix.txt', quote=FALSE, row.names=TRUE, col.names=FALSE, sep='\t')

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

