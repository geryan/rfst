#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11.1_nh"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11.1_nh.out"


for i in {1..1440}
do

	JOB_NAME="n11.1_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11.1_${i}_nh.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11.1_pva_nh.slurm $i

done
