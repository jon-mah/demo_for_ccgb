MIDAS Reference Database
Database Version: 1.2
Last Updated: Nov 2th, 2016
https://github.com/snayfach/MIDAS/blob/master/docs/ref_db.md

For more info on genomes and genes, see PATRIC website:
https://www.patricbrc.org

## Statistics ##
################
Count genomes: 31,007
Count species: 5,952
Count genes: 117 million
Count gene-families: 32 million
Total disk space required: 17 Gb

##   Files    ##
################

species_info.txt
  -information on 5,952 species
 
genome_info.txt  
 -information on 31,007 genomes
  
genome_taxonomy.txt
 -taxonomic annotations for 31,007 genomes

exclude.txt
 -list of 26 species ids with incomplete/missing data

species_tree.newick
 -Maximum likelihood phylogenetic tree built using FastTree
 -Tree based on multiple sequence alignment of 30 universal genes

ontologies/
 -descriptions of functional annotations

marker_genes/
 -HS-BLASTN database of 15 universal-single-copy gene families
 -88K total genes
 -used for metagenomic taxonomic profiling

pan_genomes/
 |
 species_id/
  |
  |--centroids.ffn.gz
  |  -FASTA file that contains one DNA sequence per gene cluster
  |  -gene clusters defined with USEARCH using a 99% identity cutoff
  |  -USEARCH was used to cluster genes from all genomes assigned to species_id
  |  -see gene_info.txt.gz for mapping to gene clusters at lower % identity cutoffs
  |
  |--centroid_functions.txt.gz
  |  -functional annotations for centroid gene_ids from 99% identity gene clusters
  |    -gene_id: gene identifier
  |    -function_id: function identifier
  |    -ontology: function ontology (go=Gene Ontology, figfam=FIGFams, kegg=KEGG, ec=Enzyme Commission)  
  |      -for more info, see /ontologies
  |
  |--gene_info.txt.gz
  |  -mapping of gene_id to genome_id and to 6 sets of gene clusters (75-99% identity clustering cutoff)
  |    -gene_id: gene identifier. genes from all genomes assigned to species are listed
  |    -genome_id: genome_id of gene. see genome_info.txt for more info on genomes
  |    -centroid_99: centroid gene_id from 99% DNA identity gene cluster
  |    -centroid_95: centroid gene_id from 95% DNA identity gene cluster
  |    -centroid_90: centroid gene_id from 90% DNA identity gene cluster
  |    -centroid_85: centroid gene_id from 85% DNA identity gene cluster
  |    -centroid_80: centroid gene_id from 80% DNA identity gene cluster
  |    -centroid_75: centroid gene_id from 75% DNA identity gene cluster

rep_genomes/
 |
 species_id/
  |
  |--genome.fna.gz
  |  -genome sequence of representative genome
  |
  |--genome.features.gz
  |  -listing of genes from representative genome
  |  -includes chromosomal locations of genes on genome.fna.gz
  |  -maps genes to functions: GO (Gene Ontology), KEGG, EC (Enzyme Commission), and FIGFAMS
