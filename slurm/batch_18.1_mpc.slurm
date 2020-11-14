#!/bin/bash


# Always edit
#SBATCH --job-name="batch_18.1"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_18.1.out"


for i in {1..840}
do

	JOB_NAME="j_18.1_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_18.1_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_18.1_mpc.slurm $i

done
