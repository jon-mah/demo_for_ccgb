# Initialize a list to store the second column values
sample_ids = []

# Read "first_sample_ids.txt" and extract the second column values
with open("first_sample_ids.txt", "r") as input_file:
    for line in input_file:
        columns = line.strip().split("\t")
        if len(columns) > 1:  # Ensure there are at least two columns
            sample_id = columns[1]
            sample_ids.append(sample_id)

# Write the extracted second column values to "unique_sample_ids.txt"
with open("unique_sample_ids.txt", "w") as output_file:
    for sample_id in sample_ids:
        output_file.write(sample_id + "\n")

print("Unique sample IDs have been saved to 'unique_sample_ids.txt'")
