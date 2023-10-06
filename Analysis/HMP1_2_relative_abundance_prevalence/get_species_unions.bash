#!/bin/bash

# Create the "species_profiles" directory if it doesn't exist
# mkdir -p species_profiles

# Loop through all subdirectories
for file in /u/project/ngarud/Garud_lab/metagenomic_fastq_files/HMP1-2/species_unions/; do
    if [ -d "$dir" ]; then
        # Get the sample id from the directory path
        sample_id=$(basename "$dir")
        # echo $sample_id
        
        # Copy the species_profile.txt file to the "species_profiles" directory
        cp "${dir}species/species_profile.txt" "species_profile_files/${sample_id}_species_profile.txt"
    fi
done

# echo "All species_profile.txt files have been copied and renamed to the 'species_profiles' directory."
