#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11h"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11h.out"


for i in {1..960}
do

	JOB_NAME="j_11h_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11h_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11_pva_habitat.slurm $i

done
