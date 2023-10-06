# Create a dictionary to store the sum of relative abundances for each species
species_abundance_sum = {}

# Create a dictionary to store the count of samples for each species
species_sample_count = {}

# Read the list of sample_ids from the second column of "first_sample_ids.txt" and store them in a set
with open("first_sample_ids.txt", "r") as first_sample_ids_file:
    first_sample_ids = set(line.strip().split("\t")[1] for line in first_sample_ids_file)

# Read the "relative_abundance.txt" file
with open("species_merged/relative_abundance.txt", "r") as abundance_file:
    header = abundance_file.readline().strip().split("\t")
    
    # Find the indices of the sample_ids that are in the set of first_sample_ids
    sample_indices = [i for i, sample_id in enumerate(header) if sample_id in first_sample_ids]

    # Iterate through the remaining lines in the file
    for line in abundance_file:
        fields = line.strip().split("\t")
        species = fields[0]

        # Initialize counters for sum and sample count
        total_abundance = 0
        sample_count = 0

        # Iterate through the sample columns and calculate the sum for the selected samples
        for index in sample_indices:
            total_abundance += float(fields[index])
            sample_count += 1

        # Calculate the mean relative abundance if there are matching samples
        if sample_count > 0:
            mean_relative_abundance = total_abundance / sample_count
            species_abundance_sum[species] = mean_relative_abundance
            species_sample_count[species] = sample_count

# Write the results to a new file
with open("species_mean_relative_abundance_2.txt", "w") as output_file:
    output_file.write("species_id\tmean_relative_abundance\n")
    for species, mean_abundance in species_abundance_sum.items():
        sample_count = species_sample_count[species]
        output_file.write(f"{species}\t{mean_abundance:.6f}\n")

print("Mean relative abundance has been computed and saved to 'species_mean_relative_abundance.txt'")
