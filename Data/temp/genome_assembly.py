import subprocess

def genome_assembly(fastq_file, output_directory):
    command = ["spades.py",
               "--s1", fastq_file,
               "-o", output_directory]
    subprocess.run(command)

fastq_file = "SRR11812840.fastq.gz"
output_directory = "assembly_output"
genome_assembly(fastq_file, output_directory)
