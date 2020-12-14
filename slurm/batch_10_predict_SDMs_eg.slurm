#!/bin/bash


# Always edit
#SBATCH --job-name="batch_10_eg"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_10_eg.out"

for i in {1..480}
do

	for j in {1..1}
	do
	
		JOB_NAME="j10_${i}_${j}"
		
		OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_10_${i}_${j}_eg.out"
				
		sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_10_predict_SDMs_eg.slurm $i $j
	
	done

done
