#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04.out"

for i in {1..60}
do
	sbatch /data/gpfs/projects/punim0995/rfst/slurm/job_04_dist_vars.slurm $i

done
