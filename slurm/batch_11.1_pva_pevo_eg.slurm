#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11.1"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11.1_pevo.out"


for i in {1..120}
do

	JOB_NAME="j${i}_11.1_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11.1_pevo_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11.1_pva_pevo_eg.slurm $i

done
