#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11.1"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11.1_tyte.out"


for i in {1..240}
do

	JOB_NAME="j11.1_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11.1_tyte_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11.1_pva_tyte.slurm $i

done
