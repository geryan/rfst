#!/bin/bash


# Always edit
#SBATCH --job-name="batch_13.1"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_13.1.out"


for i in {1..1440}
do

	JOB_NAME="j_13.1_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_13.1_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_13.1_pva_mpc.slurm $i

done
