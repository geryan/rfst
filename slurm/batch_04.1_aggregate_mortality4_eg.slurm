#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04.1.5"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04.1.5.out"


for i in {1..120}
do

	JOB_NAME="j_4.1.5_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_04.1.4_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_04.1_aggregate_mortality4_eg.slurm $i

done
