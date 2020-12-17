#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04.3_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04.3_eg.out"


for i in {1..120}
do

	JOB_NAME="j_4.3_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_04.3_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_04.3_aggregate_tsl_eg.slurm $i

done
