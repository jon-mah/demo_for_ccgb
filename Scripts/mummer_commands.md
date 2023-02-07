# MUMmer Commands

## Load into `mummer4` conda environment

* Request a computing session
* deactivate current conda environment
* activate `mummer4`

`qrsh -l h_data=15G`
`conda deactivate`
`conda activate mummer4`

## Convert `.fastq` to `.fasta`

MUMmer has issues with running on `.fastq` files, sporadically.
I converted the downloaded isolates to `.fasta` using `seqtk`.
An an example of such a script can be found at:

`/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/streptococcus_mutans_isolates/seqtk.bash`

The important lines of code are:

	#!/bin/bash
	#$ -cwd
	#$ -V
	#$ -e /u/home/j/jonmah/postproc_error
	#$ -o /u/home/j/jonmah/postproc_output
	#$ -l h_data=25G
	#$ -l h_rt=2:00:00
	#$ -t 1-329
	
	# 329 lines for 329 isolates
	
	i=0
	while read line;
	do
	   i=$((i+1))
	   # echo $line
	   if [ $i -eq $SGE_TASK_ID ]
	   then
	      file=$line
	   fi
	done < ./SraAccList.txt
	
	# mkdir $file
	seqtk seq -a fastq_MIDAS_intermediate/${file}.fastq > fasta_MUMmer/${file}.fasta
	# seqtk seq -a ${file}.fastq ../fasta_MUMmer/$file}.fasta

## Concatenate isolate data into a singular `QUERY_GENOME`

MUMmer requires a `query.fasta` for which to build a multiple sequence alignment
to with respect to the `reference.fasta`. The format of this `query.fasta` is
a multi-fasta file, i.e., several `.fasta` files concatenated together.
Given a text file named `input.txt`, in which every line is the file name of
a given isolate's `.fasta` file, we can concatenate every isolate together. An
example `.bash` script which accomplishes this can be found at:

`/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/streptococcus_mutans_isolates/fasta_MUMmer/cat.bash`

The important lines of code are:

	# Set the input and output files
	INPUT_FILE="input.txt"
	OUTPUT_FILE="query.fasta"
	# Read the input file and concatenate the .fasta files
	while read line; do
	    cat "$line" >> $OUTPUT_FILE
	done < $INPUT_FILE

## Remove empty lines from query.fasta

In the case that there are any weird characters or empty lines in the individual
isolates, it is important to remove them from the end `query.fasta`, as `nucmer`
is not very robust to formatting errors.

One such script can be found at :

`/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/streptococcus_mutans_isolates/fasta_MUMmer/remove_empty_sequence.bash`

The important line of code is:

	awk 'BEGIN {RS = ">" ; FS = "\n" ; ORS = ""} $2 {print ">"$0}' in_query.fasta > query.fasta

## Run `nucmer` on a full query

If you want to run `nucmer`, you need to provide a reference genome, a query
genome, and some optional arguments.

An example script can be found at:

`/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/streptococcus_mutans_isolates/fasta_MUMmer`

The important lines of code are:

	# Set the input FASTQ files
	REFERENCE_GENOME="reference.fasta"
	QUERY_GENOME="query.fasta"
	
	# Run nucmer to align the genomes
	nucmer -p output $REFERENCE_GENOME $QUERY_GENOME

Note that `output` is the outprefix, and can be provided as a directory or
as a string.

The end desired output is a `{$output}.delta` file. This `.delta` file can
be used for the rest of `MUMmer`'s various utility programs, such as
`show-coords`, `show-aligns`, or `show-snps`.

## Run `dnadiff`

`dnadiff` performs `nucmer`, but also produces  several other forms of output:
* .delta   - Standard nucmer alignment output
* .1delta  - 1-to-1 alignment from delta-filter -1
* .mdelta  - M-to-M alignment from delta-filter -m
* .1coords - 1-to-1 coordinates from show-coords -THrcl .1delta
* .mcoords - M-to-M coordinates from show-coords -THrcl .mdelta
* .snps    - SNPs from show-snps -rlTHC .1delta
* .rdiff   - Classified alignment breakpoints from show-diff -rH .mdelta
* .qdiff   - Classified alignment breakpoints from show-diff -qH .mdelta
* .report  - Summary of alignments, differences and SNPs
* .unref   - Unaligned reference sequence IDs and lengths
* .unqry   - Unaligned query sequence IDs and lengths

If you are running `dnadiff`, it is preferable to do so using a `.delta` file
as input as that is the most computationally intensive part. Thus, an optimal
workflow is to:
* Concatenate isolates into a `query.fasta`
* Run nucmer on the `query.fasta` relative to a `reference.fasta`
* Parse the `output.delta` file using `dnadiff`.

One example of the third bullet point can be found at:

`/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/streptococcus_mutans_isolates/MUMmer_output/dnadiff.bash`

The important lines of code are:

	OUTDIR="out"
	REFERENCE_GENOME="../fasta_MUMmer/reference.fasta"
	QUERY_GENOME="../fasta_MUMmer/query.fasta"
	DELTA=${OUTDIR}.delta
	
	# Run nucmer to align the genomes given input `.delta`
	dnadiff -p $OUTDIR -d $DELTA
	
	# Run nucmer given input genomes
	# nucmer -p $OUTDIR $REFERENCE_GENOME $QUERY_GENOME

This script also includes (commented out) commands for running `dnadiff` directly
given a `reference.fasta` and a `query.fasta`.

## Run `nucmer` on individual isolates

For downstream analysis, e.g., checking the read map percentage of individual
isolates, or viewing the alignments of individual isolates, you can also treat
non-concatenated individual isolate `.fasta` files as the `query.fasta`. The
output of these can be concatenated (with some lines cut) to match the same output
as `nucmer` run on the entirety of the `query.fasta.

An example of such a script can be found at 

`/u/home/j/jonmah/project-ngarud/demo_for_ccgb/Data/streptococcus_mutans_isolates/fasta_MUMmer/dnadiff_isolate.bash`

The important lines of code are:

	#!/bin/bash
	#$ -N dnadiff
	#$ -cwd # Run qsub script from desired working directory
	#$ -V # Same environment
	#$ -e /u/home/j/jonmah/postproc_error
	#$ -o /u/home/j/jonmah/postproc_output
	#$ -l h_data=25G
	#$ -l highp
	#$ -l time=04:00:00
	#$ -t 1-323
	
	# 323 Line
	
	i=0
	while read line;
	do
	   i=$((i+1))
	   # echo $line
	   if [ $i -eq $SGE_TASK_ID ]
	   then
	      file_name=$line
	   fi
	done < ./input.txt
	
	OUTDIR=./MUMmer_temp/${file_name}
	
	REFERENCE_GENOME="reference.fasta"
	QUERY_GENOME=$file_name
	DELTA=${OUTDIR}.delta
	
	# Run dnadiff on individual isolates
	dnadiff -p $OUTDIR $REFERENCE_GENOME $QUERY_GENOME
	
	# Run nucmer on individual isolates
	# nucmer -p $OUTDIR $REFERENCE_GENOME $QUERY_GENOME
	
	# Run dnadiff on individual isolates `.delta files`
	# dnadiff -p $OUTDIR -d $DELTA