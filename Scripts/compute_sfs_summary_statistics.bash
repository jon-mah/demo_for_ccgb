#!/bin/bash
#$ -N downsample_sfs.bash
#$ -cwd # Run qsub script from desired working directory
#$ -V
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:20:00

output_csv='../Analysis/sfs_summary_statistics.csv'

rm $output_csv
touch $output_csv
echo "Species, Watterson's theta, expected heterozygosity, Tajima's D" > $output_csv

python compute_sfs_summary_statistics.py ../Analysis/Bacteroidales_bacterium_58650_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Alistipes_putredinis_61533_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Alistipes_finegoldii_56071_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Alistipes_onderdonkii_55464_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Alistipes_shahii_62199_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Odoribacter_splanchnicus_62174_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Parabacteroides_distasonis_56985_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Parabacteroides_merdae_56972_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Prevotella_copri_61740_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_fragilis_54507_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_cellulosilyticus_58046_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_stercoris_56735_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_uniformis_57318_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_caccae_53434_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Bacteroides_vulgatus_57955_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Akkermansia_muciniphila_55290_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Dialister_invisus_61905_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Phascolarctobacterium_sp_59817_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Eubacterium_eligens_61678_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Eubacterium_rectale_56927_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Oscillibacter_sp_60799_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Ruminococcus_bromii_62047_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Ruminococcus_bicirculans_59300_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
python compute_sfs_summary_statistics.py ../Analysis/Faecalibacterium_prausnitzii_57453_downsampled_14/core_empirical_syn_downsampled_sfs.txt ../Analysis/
