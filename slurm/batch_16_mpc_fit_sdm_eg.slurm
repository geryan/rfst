#!/bin/bash


# Always edit
#SBATCH --job-name="batch_16_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_16.out"


for i in {1..18}
do

	JOB_NAME="j16_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_16_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_16_mpc_fit_sdm_eg.slurm $i

done
