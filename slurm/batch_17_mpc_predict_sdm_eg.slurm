#!/bin/bash


# Always edit
#SBATCH --job-name="batch_17"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_17.out"

for i in {1..120}
do

	for j in {1..18}
	do
	
		JOB_NAME="j17_${i}_${j}_eg"
		
		OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_17_${i}_${j}_eg.out"
				
		sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_17_mpc_predict_sdm_eg.slurm $i $j
	
	done

done
