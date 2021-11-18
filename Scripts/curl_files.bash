#!/bin/bash
#$ -N curl_files.bash
#$ -cwd # Run qsub script from desired working directory
#$ -l h_data=4G
#$ -l time=1:00:00
#$ -m be
#$ -t 2

i=0
while read line;
do
   i=$((i+1))
   # echo $line
   if [ $i -eq $SGE_TASK_ID ]
   then
      download_link=$line
      file_name=${line##*/}
   fi
done < download_links.txt

echo $download_link
echo $file_name
# curl $download_link --output $file_name
