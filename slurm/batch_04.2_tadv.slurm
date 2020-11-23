#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04.2"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04.2.out"

for i in {1..60}
do
	JOB_NAME="j4.2_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_4.2_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_04.2_tadv.slurm $i

done
