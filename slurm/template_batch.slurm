#!/bin/bash


# Always edit
#SBATCH --job-name="batch_XX"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_XX.out"


for i in {1..5}
do

	JOB_NAME="j_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_XX_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_XX.slurm $i

done
