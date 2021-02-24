#!/bin/bash
#$ -N SRA_to_Fastq_PRJNA485056
#$ -cwd
#$ -r y                 #-- restart jobs that crash
#$ -j y                 #-- join stderr and stdout
#$ -l mem_free=1G
#$ -l h_data=8G
#$ -l time=16:00:00
#$ -t 1-158
#$ -m e

. /u/local/Modules/default/init/modules.sh
module load ncbi

readarray accessions < /u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/SRA_files/PRJNA485056/PRJNA485056_run_accessions.txt

accessions=(null ${accessions[@]}) # this pads the file with an extra line in the beginning.
accession=${accessions[$SGE_TASK_ID]}
echo $accession

fastq_dir=/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/SRA_files/PRJNA485056/
SRA_fpath=/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/SRA_files/PRJNA485056/${accession}

fastq-dump-orig.2.10.1 -O $fastq_dir $SRA_fpath --gzip --split-files

