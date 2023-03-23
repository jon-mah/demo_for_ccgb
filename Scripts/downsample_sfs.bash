#!/bin/bash
#$ -N MIDAS_species.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=25G
#$ -l h_rt=00:30:00

python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_stercoris/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_stercoris/downsampled_14_sfs.txt
python downsample_sfs.py ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Faecalibacterium_prausnitzii_K/downsampled_14_sfs.txt
python downsample_sfs.py ../Data/UHGG/UHGG_Prevotella_copri/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Prevotella_copri/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
# python downsample_sfs.py ../Data/UHGG/UHGG_Bacteroides_fragilis/full_output_sfs.txt 14 ../Data/UHGG/UHGG_Bacteroides_fragilis/downsampled_14_sfs.txt
