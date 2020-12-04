#!/bin/bash


# Always edit
#SBATCH --job-name="batch_04_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_04_eg.out"

for i in {1..120}
do
	sbatch /data/gpfs/projects/punim0995/rfst/slurm/job_04_dist_vars_eg.slurm $i

done
