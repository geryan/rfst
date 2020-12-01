#!/bin/bash


# Always edit
#SBATCH --job-name="batch_2.1_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_02.1.out"


for i in {1..6}
do

	JOB_NAME="j2.1eg_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_02.1_eg_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_02.1_species_occurrence_eg.slurm $i

done
