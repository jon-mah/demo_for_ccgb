# extract_midas_phylogeny.R
# Extract phylogenetic subtree using MIDAS codes.
# Author: Jon Mah
# Last Updated: 6/7/2023
# Required input: a list of MIDAS species identifications
# Required input: a path to a species_information.txt in MIDAS format
# Required output: outprefixes (read comments)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Install this if you need to:
# install.packages('ape')

library(ape)

get_species_code_reference = function(code, reference) {
  # Given a reference, which is a `.csv` with one column describing
  # species and a second column having the corresponding 5-digit 
  # MIDAS code, this function converts to and from species_id to
  # MIDAS 5-digit code.
  # EX: get_species_code_reference(55290, reference) = A. muciniphila
  if (code %in% reference$species_id) {
    return(reference[reference$species_id == code, ]$midas_number)
  } else if(code %in% reference$midas_number) {
    return(reference[reference$midas_number == code, ]$species_id)
  }
}

### Here is a snippet for how the reference is set up
### The header is species_id and midas_number respectively
# "species_id","midas_number"
# "Anaerovibrio_lipolyticus_60416","60416"
# "Bacillus_bogoriensis_60417","60417"
# "Akkermansia_muciniphila_55290","55290"
# "Moorella_thermoacetica_55298","55298"
# "Marine_actinobacterium_61178","61178"
# "Janibacter_sp_61179","61179"
# "Tepidimonas_taiwanensis_61170","61170"
# "Coprothermobacter_proteolyticus_61171","61171"
# "Dictyoglomus_thermophilum_61172","61172"
# "Thermomicrobium_roseum_61173","61173"
# ..., ...
###

### Read the input TSV file
# Change the path to your own file
path_to_input_table = "../Data/midas_tree/midas_db_v1.2/species_info.txt"
input_table <- read.table(path_to_input_table, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

### Extract the midas_number from the species_id column
input_table$midas_number <- sub(".+_(\\d+)$", "\\1", input_table$species_id)

### Create a new data frame with species_id and midas_number columns
output_table <- data.frame(species_id = input_table$species_id, midas_number = input_table$midas_number)

### Save the output as a CSV file
# Change the path to your own file.
path_to_output_table = "../Data/midas_tree/midas_db_v1.2/species_code_reference.txt"
write.csv(output_table, file = path_to_output_table, row.names = FALSE)

# Print the output data frame
print(output_table)

### Read the Newick file
# Change the path to your own file.
path_to_input_tree = "../Data/midas_tree/midas_db_v1.2/species_tree.newick"
input_tree <- read.tree(path_to_input_tree)

### Specify the tip labels for the subtree you want to extract
# The format must be a list of `species_id`.

### You can alternatively read in a file
# path_to_species_list =  '' # Input your desired file location
# species_subtree = read.csv(path_to_species_list)

species_subtree = c(
  'Akkermansia_muciniphila_55290',
  'Alistipes_finegoldii_56071',
  'Alistipes_onderdonkii_55464',
  'Alistipes_putredinis_61533',
  'Alistipes_shahii_62199',
  'Bacteroidales_bacterium_58650',
  'Bacteroides_caccae_53434',
  'Bacteroides_cellulosilyticus_58046',
  'Bacteroides_fragilis_54507',
  # 'Bacteroides_massiliensis_44749',
  # 'Bacteroides_ovatus_58035',
  'Bacteroides_stercoris_56735',
  'Bacteroides_thetaiotaomicron_56941',
  # 'Bacteroides_uniformis_57318',
  'Bacteroides_vulgatus_57955',
  'Bacteroides_xylanisolvens_57185',
  'Barnesiella_intestinihominis_62208',
  # 'Coprococcus_sp_62244',
  # 'Dialister_invisus_61905',
  'Eubacterium_eligens_61678',
  'Eubacterium_rectale_56927',
  'Faecalibacterium_prausnitzii_57453',
  'Odoribacter_splanchnicus_62174',
  'Oscillibacter_sp_60799',
  'Parabacteroides_distasonis_56985',
  'Parabacteroides_merdae_56972',
  'Phascolarctobacterium_sp_59817',
  'Prevotella_copri_61740',
  'Ruminococcus_bicirculans_59300',
  'Ruminococcus_bromii_62047'
)

# List of corresponding 5-digit MIDAS codes for given list of species
midas_code_subtree = c()

for (species in species_subtree) {
  midas_code_subtree = c(midas_code_subtree, get_species_code_reference(species, output_table))
}

# Check to make sure
print(midas_code_subtree)

# Copy list to another variable
subtree_tips = midas_code_subtree

### Extract the subtree
subtree <- keep.tip(input_tree, subtree_tips)

### Print the extracted subtree
# print(subtree)

### Format tip labels
for (i in 1:length(subtree$tip.label)) {
  # print(subtree$tip.label[i])
  new_tip = get_species_code_reference(subtree$tip.label[i], output_table)
  ## Comment line below for full text output
  ## Else, truncate MIDAS number off of tip labels
  new_tip = str_sub(new_tip, 1, str_length(new_tip)-6)
  subtree$tip.label[i] = new_tip
  # print(subtree$tip.label[i])
}

### Generate image in R
plot(subtree)

### Output newick for subtree
# Change path to your desired file output name.
path_to_output_tree = '../Data/midas_tree/midas_db_v1.2/good_species_subtree.newick'
write.tree(subtree, file=path_to_output_tree)
