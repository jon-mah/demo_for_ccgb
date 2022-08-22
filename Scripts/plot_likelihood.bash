#!/bin/bash
#$ -cwd
#$ -V
#$ -N plot_likelihood
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=01:30:00

# This script infers the demography of a given example synonymous sfs.

# python plot_likelihood.py ../Data/Akkermansia_muciniphila_55290_syn.sfs 2.49183094e-05 4.02384611e-06 ../Analysis/Akkermansia_muciniphila_55290_downsampled/ --mask_singletons
# python plot_likelihood.py ../Data/Alistipes_onderdonkii_55464_syn.sfs 2.22291146e-05 3.89330882e-06 ../Analysis/Alistipes_onderdonkii_55464_downsampled/ --mask_singletons
# python plot_likelihood.py ../Data/Bacteroides_thetaiotaomicron_56941_syn.sfs 0.00575632 0.00019757 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled/ --mask_singletons
python plot_likelihood.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled/Bacteroides_thetaiotaomicron_56941_empirical_sfs.txt 1.23592864e-05 4.26793269e-07 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled/ --mask_singletons
# python plot_likelihood.py ../Data/Bacteroides_xylanisolvens_57185_syn.sfs 0.0034628  0.00024942 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled/ --mask_singletons
# python plot_likelihood.py ../Data/Barnesiella_intestinihominis_62208_syn.sfs 2.36014245e-05 1.98975801e-06 ../Analysis/Barnesiella_intestinihominis_62208_downsampled/ --mask_singletons
#  python plot_likelihood.py ../Data/Parabacteroides_distasonis_56985_syn.sfs 0.00238249 0.00013105 ../Analysis/Parabacteroides_distasonis_56985_downsampled/ --mask_singletons
