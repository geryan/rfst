#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11.1_eg_nh"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11.1_eg_nh.out"


for i in {1..2400}
do

	JOB_NAME="j11.1_${i}_eg_nh"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11.1_${i}_eg_nh.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11.1_pva_eg_nh.slurm $i

done
