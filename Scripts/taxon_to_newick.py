import csv

def csv_to_newick(csv_file, output_file):
    # Read the CSV file
    with open(csv_file, 'r') as file:
        reader = csv.DictReader(file)
        data = list(reader)

    # Create a dictionary to store the taxonomic hierarchy
    hierarchy = {}

    # Iterate over each row in the CSV data
    for row in data:
        current_node = hierarchy

        # Iterate over each level of taxonomic classification
        for level in ['Domain', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species']:
            # Get the taxonomic classification value for the current level
            classification = row[level]

            # If the classification is not empty, add it to the hierarchy
            if classification:
                # If the classification is not already in the hierarchy, add it
                if classification not in current_node:
                    current_node[classification] = {}

                # Move to the next level in the hierarchy
                current_node = current_node[classification]

    # Convert the hierarchy to Newick format recursively
    def convert_to_newick(node):
        newick = ''
        if node:
            for classification, child_node in node.items():
                # Recursively convert child nodes to Newick format
                child_newick = convert_to_newick(child_node)
                if child_newick:
                    if newick:
                        newick += ','
                    newick += f"{classification}:1.0,{child_newick}"
                else:
                    if newick:
                        newick += ','
                    newick += f"{classification}"

            newick = f"({newick})"

        return newick

    # Convert the hierarchy to Newick format
    newick = convert_to_newick(hierarchy)

    # Write the Newick string to the output file
    with open(output_file, 'w') as file:
        file.write(newick)

# Specify the input CSV file and output Newick file paths
# csv_file = '../Summary/good_species_taxonomy_merged.csv'
csv_file = '../Summary/good_species_taxonomy_midas.csv'
output_file = '../Summary/good_species_midas.newick'

# Convert the CSV file to Newick format
csv_to_newick(csv_file, output_file)
