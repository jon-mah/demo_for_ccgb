import os
import pandas as pd

## location of scripts directory
scripts_dir = "/u/project/ngarud/peterlaurin" # PL may not need check later
figures_dir = f"{scripts_dir}/figures" # PL may not need

## metadata
metadata_dir = "/u/project/ngarud/Garud_lab/UHGG/metadata"
## list of species we will be analyzing
good_species = list(pd.read_csv(f"{metadata_dir}/good_species.txt",header=None).values.ravel())

## base directory to store downloaded files
raw_dir = "/u/project/ngarud/Garud_lab/UHGG/raw_data"

## base directory to write processed (i.e. annotated) haplotypes
haplotype_dir = "/u/project/ngarud/Garud_lab/UHGG/haplotypes"

## base directory for summaries of poly sites and annotation of state (syn/non)
annotation_dir = "/u/project/ngarud/Garud_lab/UHGG/annotations"

## base directory for clade control - both genetic distance matrices (mem-intensive)
##  and plots
genetic_distances_dir = "/u/project/ngarud/Garud_lab/UHGG/distances"


def print_var(var_name):
    print(globals()[var_name])
