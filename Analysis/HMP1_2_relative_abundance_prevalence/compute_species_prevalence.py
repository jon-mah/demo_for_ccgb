import os

# Directory containing the [sample_id]_species_union.txt files
directory = "species_unions"

# Initialize a dictionary to store the counts of each species
species_counts = {}

# Read the list of species from "good_species_list_phylogenetically_sorted.txt" and store them in a set
good_species_set = set()
with open("good_species_list_phylogenetically_sorted.txt", "r") as good_species_file:
    for line in good_species_file:
        species = line.strip()
        good_species_set.add(species)

# Read the list of sample_ids from "one_sample_per_host_ids.txt" and store them in a set
sample_ids_set = set()
with open("one_sample_per_host_ids.txt", "r") as sample_ids_file:
    for line in sample_ids_file:
        sample_id = line.strip()
        sample_ids_set.add(sample_id)

# Iterate through files in the directory
for filename in os.listdir(directory):
    if filename.endswith("_species_union.txt"):
        file_path = os.path.join(directory, filename)

        # Read and process the file
        with open(file_path, "r") as file:
            header = file.readline()  # Skip the header
            for line in file:
                species = line.strip()
                
                # Extract sample_id from the filename
                sample_id = filename.split("_")[0]

                # Only include the species if the sample_id is in the one_sample_per_host_ids.txt file
                if sample_id in sample_ids_set:
                    species_counts[species] = species_counts.get(species, 0) + 1

# Filter the species counts to include only those in the good_species_set
filtered_species_counts = {species: count for species, count in species_counts.items() if species in good_species_set}

# Write the filtered species counts to a new file
with open("species_prevalence_filtered.txt", "w") as output_file:
    output_file.write("species_id\tprevalence\n")
    for species, count in filtered_species_counts.items():
        output_file.write(f"{species}\t{count}\n")

print("Species prevalence has been computed and saved to 'species_prevalence_filtered.txt'")
