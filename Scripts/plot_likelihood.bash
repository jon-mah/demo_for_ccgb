#!/bin/bash
#$ -cwd
#$ -V
#$ -N plot_likelihood
#$ -e /u/home/j/jonmah/postproc_error
#$ -o /u/home/j/jonmah/postproc_output
#$ -l h_data=15G
#$ -l h_rt=00:10:00
#$ -l highp
#$ -t 1-6

$ SGE_TASK_ID=5

# This script infers the demography of a given example synonymous sfs.

i=0
while read line;
 do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]; then
       file=$line
   fi
done < ./likelihood_list.txt

# i=0
# while read line;
#   do
#     i=$((i+1))
#     # echo $line
#     if [ $i -eq $SGE_TASK_ID ]; then
#         file=$line
#     fi
# done < ./b_xylanisolvens_likelihood_list.txt

# python $file

# python plot_likelihood.py ../Analysis/Akkermansia_muciniphila_55290_downsampled/empirical_sfs.txt 2.49183094e-05 4.02384611e-06 ../Analysis/Akkermansia_muciniphila_55290_downsampled/ --mask_singletons
# python plot_likelihood.py ../Analysis/Alistipes_onderdonkii_55464_downsampled/empirical_sfs.txt 2.22291146e-05 3.89330882e-06 ../Analysis/Alistipes_onderdonkii_55464_downsampled/ --mask_singletons
# python plot_likelihood.py ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled/empirical_sfs.txt 1.23592864e-05 4.26793269e-07 ../Analysis/Bacteroides_thetaiotaomicron_56941_downsampled/ --mask_singletons
# python plot_likelihood.py ../Analysis/Bacteroides_xylanisolvens_57185_downsampled/empirical_sfs.txt 0.0034628  0.00024942 ../Analysis/Bacteroides_xylanisolvens_57185_downsampled/ --mask_singletons
# python plot_likelihood.py ../Analysis/Barnesiella_intestinihominis_62208_downsampled/empirical_sfs.txt 2.36014245e-05 1.98975801e-06 ../Analysis/Barnesiella_intestinihominis_62208_downsampled/ --mask_singletons
# python plot_likelihood.py ../Analysis/Parabacteroides_distasonis_56985_downsampled/empirical_sfs.txt 0.00238249 0.00013105 ../Analysis/Parabacteroides_distasonis_56985_downsampled/ --mask_singletons
