#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04.1.5"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04.1.5.out"


for i in {1..60}
do

	JOB_NAME="j_4.1.5_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_04.1.5_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_04.1_aggregate_mortality5.slurm $i

done
