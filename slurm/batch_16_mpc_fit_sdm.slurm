#!/bin/bash


# Always edit
#SBATCH --job-name="batch_16"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_16.out"


for i in {1..14}
do

	JOB_NAME="j_16_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_16_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_16_mpc_fit_sdm.slurm $i

done
