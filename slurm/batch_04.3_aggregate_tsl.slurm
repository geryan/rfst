#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04.3"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04.3.out"


for i in {1..60}
do

	JOB_NAME="j_4.3_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_04.3_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_04.3_aggregate_tsl.slurm $i

done
