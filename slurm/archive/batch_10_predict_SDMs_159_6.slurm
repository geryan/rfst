#!/bin/bash

for i in {159..159}
do

	for j in {6..6}
	do
	
		JOB_NAME="j10_${i}_${j}"
		
		OUT_NAME="/data/cephfs/punim0995/rfst/slurm/slurm_output/slurm_out_10_${i}_${j}.out"
				
		sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/cephfs/punim0995/rfst/slurm/job_10_predict_SDMs.slurm $i $j
	
	done

done
