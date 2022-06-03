#!/bin/bash
#$ -cwd
#$ -V
#$ -m ea
#$ -l h_data=5G
#$ -l h_rt=02:00:00

# This script infers the demography of a given example synonymous sfs.

python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 25 --mask_singletons
python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 24 --mask_singletons
python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 23 --mask_singletons
python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 22 --mask_singletons
python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 21 --mask_singletons
python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 20 --mask_singletons
python downsample_sfs.py ../Data/Bacteroides_uniformis_57318_syn.sfs ../Analysis/Bacteroides_uniformis_57318/ 15 --mask_singletons


# two_epoch and exponential
# upper_bound = [8, 0.00025]
# lower_bound = [0, 0]
# initial_guess = [0.1, 0.00005]

