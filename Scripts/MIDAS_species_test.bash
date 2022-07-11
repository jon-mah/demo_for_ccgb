######### SUBMIT_MIDAS.sh START #############
#!/bin/bash
#$ -N MIDAS_species_fails.bash
#$ -cwd # Run qsub script from desired working directory
# error = Merged with joblog
#$ -o joblog.$JOB_ID
#$ -j y
#$ -V
#$ -l h_data=20G
# #$ -pe shared 2
#$ -l highp,time=72:00:00

# echo job info on joblog:
echo "Job $JOB_ID started on:   " `hostname -s`
echo "Job $JOB_ID started on:   " `date `
echo " "

# You need to define your own out directory
OUTDIR=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS014530/temp
if [ ! -d $OUTDIR ]; then
  mkdir $OUTDIR
fi

# load the job environment
. /u/local/Modules/default/init/modules.sh
module load python
module av midas
module load midas
. /u/local/apps/midas/1.3.2/VIRT_ENV_FOR_MIDAS/bin/activate
module li
which python
echo " "

file_1=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS014530/SRS014530.denovo_duplicates_marked.trimmed.1.fastq.gz
file_2=/u/project/ngarud/jonmah/demo_for_ccgb/Data/oral_microbiome_data/fastq_MIDAS_intermediate/SRS014530/SRS014530.denovo_duplicates_marked.trimmed.2.fastq.gz

# run the job:
echo "run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2}"
run_midas.py species $OUTDIR -1 ${file_1} -2 ${file_2}

# echo job info on joblog:
echo " "
echo "Job $JOB_ID run on:   " `hostname -s`
echo "Job $JOB_ID ended on:   " `date `
######### SUBMIT_MIDAS.sh STOP #############

