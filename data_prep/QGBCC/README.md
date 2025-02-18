Instructions for GT-seq genotyping at 96 microhaplottype markers for Sebastes ID

##Steps in the workflow after making subdirectories, obtaining reference file and initial raw fastqs, : 
##	1) index reference file with bwa
##	2) merge paired end reads with flash
##	3) map merged reads to reference with bwa mem into sam alignment files (the microhaplot R script needs sam files). Move the sam files, one for each sample, to local computer.
##	4) install microhaplot in R on local computer
##	5) genotype microhaps with microhaplot and reference vcf file provided by Diana Baetscher
## 	6) run custom R script to assign individuals to a species in the reference database with rubias


##In the following scripts code with <ALLCAPS> is an object that needs to be adjusted. For example, <PATH> should be replaced with your actual directory path, <QUEUE> should be replaced with the OSU CQLS queue you want to use, etc. In my case the path to this project's directory is /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes, and the queue I use most often is harold
##You can check which queues are avail to you with the command SGE_Avail

##create a new directory for this project with mkdir (make directory) 

mkdir <PATH>/Project_1

##move into this new project directory with cd (change directory)##

cd <PATH>/Project_1

##use pwd (present working directory) to list the present working directory and check you've moved into the new empty directory

pwd

##Within the Project_1 directory create 4 additional subdirectories to store files in## 
##the syntax ./ indicates in present working directory. Thus, mkdir ./ means make a new directory in the current directory/location

mkdir ./reference 
mkdir ./rawdata
mkdir ./map 
mkdir ./flash


##upload the reference file (gtseq18_loci.fasta) into the Project_1/reference directory##
##upload the paired end FASTQ files into the Project_1/rawdata directory##
##To upload the reference and raw sequencing files you will need the program filezilla


##check that the reference file was uploaded to the directory with the the list command (ls) 
##the following should list the name of the file (gtseq18_loci.fasta)

ls <PATH>/Project_1/reference

##use the OSU server command (SGE_Batch) and the bwa index command to index the reference file##
##The SGE_Batch commmand needs the name of the job (-r), the name of the queue to use (-q), and the command (-c) to execute. 
##the bwa index has two algorithms (-a). This protocol uses the default algorithm (is).

SGE_Batch -r sebastes_index -q <QUEUE> -c "bwa index -a is <PATH>/Project_1/reference/gtseq18_loci.fasta"

##use flash to merge paired end reads with settings suggested by Diana##
##write a script called merge.sh to automate merging of forward and reverse reads for each sample##

nano ./merge.sh


##copy and paste the following code into the window##

#!/bin/bash

forwardreads=(<PATH>/Project_1/rawdata/*_R1_001.fastq.gz*)

for forwardread in ${forwardreads[@]}
do
reverseread=$(echo $forwardread | sed 's\_R1_\_R2_\g')
outputfilename=$(basename $forwardread _R1_fq.gz)

flash2 -m 10 -M 100 -z -o $outputfilename -d <PATH>/Project_1/flash/ $forwardread $reverseread

done

##exit the window by typing ctrl+x on the keyboard and save the file by typing y on the keyboard##

##make the script executable##

chmod +x ./merge.sh 

##run the script with SGE_Batch##

SGE_Batch -r flash_merge -q <QUEUE> -c merge.sh


##map flashed reads with bwa mem##
##write scipt called map.sh to automate mapping of flash merged files into sam alignment files##

nano ./map.sh

#!/bin/bash

ref=/nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/reference/gtseq18_loci.fasta

flashed=(/nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/flash/*.extendedFrags.fastq.gz)

for flash in ${flashed[@]}
do
outputfilename=$(basename $flash .extendedFrags.fastq.gz)
readgroup="@RG\tID:${outputfilename}\tLB:amplicon\tPL:ILLUMINA\tSM:sebastes${outputfilename}"
bwa mem -a -M -v 3 -R $readgroup $ref $flash > /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/${outputfilename}.aln.sam

done


##make the script executable##

chmod +x ./map.sh 

##run the script with SGE_Batch##

SGE_Batch -r bwa_map -q <QUEUE> -c map.sh




##Only need the scripts/commands below to generate your own reference vcf file.
##otherwise move the aligned sam files to local computer for microhaplotype calling with the R package microhaplot.











##convert sam to bam 

#!/bin/bash

SAM_files=(/nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/*.aln.sam)

for i in ${SAM_files[@]}
do
outputfilename=$(basename $i .aln.sam)
samtools view -bhS $i > /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/${outputfilename}.bam

done


##sort bam files##

#!/bin/bash

BAM_files=(/nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/*.bam)

for i in ${BAM_files[@]}
do
outputfilename=$(basename $i .bam)
samtools sort $i -o /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/${outputfilename}.sorted.bam

done




##index mapped/sorted bams##

nano ./index.sh

#!/bin/bash

sorted=(/nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/*.sorted.bam)

for i in ${sorted[@]}
do 
samtools index $i

done 


SGE_Batch -r indexing -q harold -c index.sh


# make bamlist##


ls ./SAM/*.sorted.bam > BAM.list


##generate idx stats##
 
nano ./alignment_stats.sh

#!/bin/bash

indexed=(/nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/SAM/*.sorted.bam)

for i in ${indexed[@]}
do
outputfilename=$(basename $i .sorted.bam)
samtools idxstats $i > /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/idx_stats_SAM/${outputfilename}_idxstats.txt

done

##make the script executable##

chmod +x ./alignmnet_stats.sh 

##need to faidx reference FASTA file for freebayes to work##

SGE_Batch -r sebastes_faidx -q <QUEUE> -c "samtools faidx /nfs/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/reference/gtseq18_loci.fasta"

##freebayes##

SGE_Batch -r freebayes_genotyping -q harold -c "freebayes --haplotype-length 0 -kwVa -X -u -i -f /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/reference/gtseq18_loci.fasta -L BAM.list > /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/rockfish_microhaplotype_raw.vcf"

##VCFTOOLS filter##

SGE_Batch -r do_vcftools_filter -q harold -c "vcftools --vcf /nfs5/FW_HMSC/Omalley_Lab/olsen/Projects/sebastes/VCF/rockfish_microhaplotype_raw.vcf --minQ 30 --minDP 10 --recode"
