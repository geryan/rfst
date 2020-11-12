#!/bin/bash


# Always edit
#SBATCH --job-name="batch_17"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_17.out"

for i in {1..60}
do

	for j in {1..14}
	do
	
		JOB_NAME="j17_${i}_${j}"
		
		OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_17_${i}_${j}.out"
				
		sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_17_mpc_predict_sdm.slurm $i $j
	
	done

done
