#!/bin/bash
#$ -N SRA_download_NIH1
#$ -e /u/home/postproc_error
#$ -o /u/home/postproc_output
#$ -cwd # Run qsub script from desired working directory
#$ -l h_data=4G
#$ -l time=2:00:00
#$ -m e

accspath=/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/SRA_files/PRJNA485056_run_accessions.txt

while read accession; do
        second3=`echo $accession | cut -c4-6`
        echo $accession
        if [ ${#accession} == 9 ]
        then
                wget ftp://ftp.sra.ebi.ac.uk/vol1/srr/SRR${second3}/$accession
        else
                last1=`echo $accession | cut -c10`
                wget ftp://ftp.sra.ebi.ac.uk/vol1/srr/SRR${second3}/00${last1}/$accession
        fi
done < $accspath
