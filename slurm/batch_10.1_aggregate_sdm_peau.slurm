#!/bin/bash


# Always edit
#SBATCH --job-name="batch_10.1"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_10.1.out"


for i in {1..240}
do

	JOB_NAME="j_10.1_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_10.1_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_10.1_aggregate_sdm_peau.slurm $i

done
