#!/bin/bash

for i in {1..120}
do
	JOB_NAME="j3eg_${i}"
		
	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_03_landis_vars_eg_${i}.out"
				
	sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_03_landis_vars_eg.slurm $i

done
