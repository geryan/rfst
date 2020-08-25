#!/bin/bash

for i in {1..60}
do
	JOB_NAME="j3_${i}"
		
	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_03_landis_vars_${i}.out"
				
	sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_03_landis_vars.slurm $i

done
