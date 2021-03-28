#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11.1_export"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11.1_export_eg.out"


for i in {1..100}
do

	JOB_NAME="j11.1_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11.1_${i}_export.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11.1_pva_export_eg.slurm $i

done
