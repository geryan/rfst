#!/bin/bash


# Always edit
#SBATCH --job-name="batch_18.1_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_18.1_eg.out"


for i in {1..2160}
do

	JOB_NAME="j_18.1_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_18.1_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_18.1_mpc_eg.slurm $i

done