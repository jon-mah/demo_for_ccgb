# Create a dictionary to store the first listed sample_id for each subject_id
first_sample_ids = {}

# Read the "unique_subject_ids.txt" file to get the unique subject_ids
with open("unique_subject_ids.txt", "r") as unique_subject_ids_file:
    unique_subject_ids = [line.strip() for line in unique_subject_ids_file]

# Read the "HMP1-2_metadata.txt" file and extract the first listed sample_id for each subject_id
with open("HMP1-2_metadata.txt", "r") as metadata_file:
    header = metadata_file.readline().strip().split("\t")
    subject_id_index = header.index("subject_id")  # Find the index of the "subject_id" column
    sample_id_index = header.index("sample_id")  # Find the index of the "sample_id" column
    for line in metadata_file:
        data = line.strip().split("\t")
        subject_id = data[subject_id_index]
        sample_id = data[sample_id_index]
        if subject_id in unique_subject_ids and subject_id not in first_sample_ids:
            first_sample_ids[subject_id] = sample_id

# Write the first listed sample_ids to a new file
with open("first_sample_ids.txt", "w") as output_file:
    for subject_id, sample_id in first_sample_ids.items():
        output_file.write(f"{subject_id}\t{sample_id}\n")

print("First listed sample_ids have been saved to 'one_sample_per_host_ids.txt'")
