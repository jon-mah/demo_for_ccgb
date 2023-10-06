import os

# Create a dictionary to store the sum of relative abundances for each species
species_abundance_sum = {}

# Create a dictionary to store the count of samples for each species
species_sample_count = {}

#sample_count = 249

# Read the list of sample_ids from "first_sample_ids.txt" and store them in a set
with open("first_sample_ids.txt", "r") as first_sample_ids_file:
    first_sample_ids = set(line.strip().split("\t")[1] for line in first_sample_ids_file)

# List all the files in the "species_profile_files" directory
profile_files = os.listdir("species_profile_files")

# Iterate through each species in "good_species_list_phylogenetically_sorted.txt"
with open("good_species_list_phylogenetically_sorted.txt", "r") as species_list_file:
    for line in species_list_file:
        species = line.strip()

        # Initialize counters for sum and sample count
        total_abundance = 0
        sample_count = 0

        # Iterate through profile files
        for profile_file in profile_files:
            sample_id = profile_file.split("_")[0]

            # Only consider profile files with matching sample_id in first_sample_ids
            if sample_id in first_sample_ids:
                profile_file_path = os.path.join("species_profile_files", profile_file)

                # Read and process the profile file
                with open(profile_file_path, "r") as profile:
                    header = profile.readline()  # Skip the header
                    for line in profile:
                        fields = line.strip().split("\t")
                        if len(fields) >= 4:
                            current_species = fields[0]
                            relative_abundance = float(fields[3])

                            if current_species == species:
                                total_abundance += relative_abundance
                                sample_count += 1

        # Calculate the mean relative abundance if there are matching samples
        if sample_count > 0:
            mean_relative_abundance = total_abundance / sample_count
            species_abundance_sum[species] = mean_relative_abundance
            species_sample_count[species] = sample_count

# Write the results to a new file
with open("species_mean_relative_abundance.txt", "w") as output_file:
    output_file.write("species_id\tmean_relative_abundance\tsample_count\n")
    for species, mean_abundance in species_abundance_sum.items():
        sample_count = species_sample_count[species]
        output_file.write(f"{species}\t{mean_abundance:.6f}\n")

print("Mean relative abundance has been computed and saved to 'species_mean_relative_abundance.txt'")
