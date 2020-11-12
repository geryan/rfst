#!/bin/bash


# Always edit
#SBATCH --job-name="batch_14.1"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_14.1.out"


for i in {1..16}
do

	JOB_NAME="j_14.1_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_14.1_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_14.1_sp_occ_metapop.slurm $i

done
