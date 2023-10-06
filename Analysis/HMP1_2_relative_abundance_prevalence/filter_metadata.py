# Read the list of sample_ids from "HMP1-2_sample_ids.txt"
with open("HMP1-2_sample_ids.txt", "r") as sample_id_file:
    sample_ids = set(line.strip() for line in sample_id_file)

# Create a list to store the filtered rows
filtered_rows = []

# Read and filter the "HMP1-2_metadata.txt" file
with open("HMP1-2_metadata.txt", "r") as metadata_file:
    header = metadata_file.readline().strip().split("\t")
    for line in metadata_file:
        data = line.strip().split("\t")
        subject_id, sample_id = data[0], data[1]
        if sample_id in sample_ids:
            filtered_rows.append([subject_id, sample_id])

# Write the filtered data to a new file
with open("filtered_metadata.txt", "w") as output_file:
    # Write the header
    output_file.write("\t".join(header) + "\n")
    # Write the filtered rows
    for row in filtered_rows:
        output_file.write("\t".join(row) + "\n")

print("Filtered metadata has been saved to 'filtered_metadata.txt'")
