#!/bin/bash


# Always edit
#SBATCH --job-name="batch_13.1.5"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_13.1.5.out"


for i in {1..2400}
do

	JOB_NAME="j_13.1.5_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_13.1.5_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_13.1_pva_mpc5_eg.slurm $i

done
