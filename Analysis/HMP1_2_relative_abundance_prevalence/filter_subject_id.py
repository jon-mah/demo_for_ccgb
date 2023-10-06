# Create an empty set to store unique subject_ids
unique_subject_ids = set()

# Read the "HMP1-2_metadata.txt" file and extract unique subject_ids
with open("HMP1-2_metadata.txt", "r") as metadata_file:
    header = metadata_file.readline().strip().split("\t")
    subject_id_index = header.index("subject_id")  # Find the index of the "subject_id" column
    for line in metadata_file:
        data = line.strip().split("\t")
        subject_id = data[subject_id_index]
        unique_subject_ids.add(subject_id)

# Write the unique subject_ids to a new file
with open("unique_subject_ids.txt", "w") as output_file:
    for subject_id in unique_subject_ids:
        output_file.write(subject_id + "\n")

print("Unique subject_ids have been saved to 'unique_subject_ids.txt'")
