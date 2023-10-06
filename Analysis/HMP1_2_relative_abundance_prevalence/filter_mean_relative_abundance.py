# Define the input and output file names
input_file_name = "species_mean_relative_abundance_2.txt"
output_file_name = "filtered_species_mean_relative_abundance.txt"

# Create a set to store the species from "good_species_list_phylogenetically_sorted.txt"
good_species_set = set()

# Read the species from "good_species_list_phylogenetically_sorted.txt" and store them in the set
with open("good_species_list_phylogenetically_sorted.txt", "r") as good_species_file:
    for line in good_species_file:
        species = line.strip()
        good_species_set.add(species)

# Create a list to store the lines of the filtered output
filtered_lines = []

# Read the input file and filter lines for species in the good_species_set
with open(input_file_name, "r") as input_file:
    # Read and store the header line
    header = input_file.readline().strip() + '\n'
    filtered_lines.append(header)
    
    for line in input_file:
        species, mean_abundance = line.strip().split("\t")
        if species in good_species_set:
            filtered_lines.append(line)

# Write the filtered lines to the output file
with open(output_file_name, "w") as output_file:
    output_file.writelines(filtered_lines)

print(f"Filtered data has been saved to '{output_file_name}'")
