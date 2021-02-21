#!/bin/bash


# Always edit
#SBATCH --job-name="batch_14.1_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_14.1_eg.out"


for i in {1..19}
do

	JOB_NAME="j_14.1_${i}_eg"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_14.1_${i}_eg.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_14.1_sp_occ_metapop_eg.slurm $i

done
