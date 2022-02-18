# Bash Commands

This is a list of all relevant bash commands and expalanations for the commands that we are using.

## Creating `successful_species_union.txt`

We found technical difficulties in completing the `species` step of MIDAS when applied to samples of oral data.
We were able to successfully complete the species step for approximately 180 of 379 files.
For the rest, we use an approximateion of the `species_profile.txt` by concatenating the successful `species_profile.txt` files
into one titled `successful_species_union.txt`.

We do this concatenation step via the following bash commands.

```
cd project-ngarud/
cd demo_for_ccgb/
cd Data/
cd oral_microbiome_data/
cat */species/species_profile.txt > successful_species_union.txt
```
