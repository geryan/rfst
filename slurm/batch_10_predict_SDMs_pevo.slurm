#!/bin/bash


# Always edit
#SBATCH --job-name="batch_10"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_10.out"

for i in {1..240}
do

	for j in {1..1}
	do
	
		JOB_NAME="j10_${i}_${j}"
		
		OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_10_${i}_${j}.out"
				
		sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim0995/rfst/slurm/job_10_predict_SDMs_pevo.slurm $i $j
	
	done

done
