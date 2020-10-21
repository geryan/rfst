#!/bin/bash


# Always edit
#SBATCH --job-name="GR_TEST_BATCH"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/aa_test_batch.out"


for i in {1..5}
do

	JOB_NAME="j_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/jtest_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/aa_test_job.slurm $i

done
