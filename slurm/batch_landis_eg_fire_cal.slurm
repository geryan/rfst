#!/bin/bash


# Always edit
#SBATCH --job-name="batch_LAN_fire_cal"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_LAN_fire_cal.out"


dir_list=$(ls /data/gpfs/projects/punim1340/eg_fire_calibration/*/ -d)

for directory_name in ${dir_list}
do
	
	directory_name=$directory_name sbatch /data/gpfs/projects/punim0995/rfst/slurm/job_landis.slurm
	# directory_name=$directory_name bash /data/gpfs/projects/punim0995/rfst/slurm/job_landis.slurm

done
