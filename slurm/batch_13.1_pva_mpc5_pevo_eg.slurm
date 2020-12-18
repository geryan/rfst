#!/bin/bash


# Always edit
#SBATCH --job-name="batch_13.1.5"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_13.1.5.out"


for i in {1..480}
do

	JOB_NAME="j${i}_13.1.5_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_13.1.5_${i}_pevo_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_13.1_pva_mpc5_pevo_eg.slurm $i

done
