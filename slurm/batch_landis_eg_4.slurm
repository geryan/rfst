#!/bin/bash


# Always edit
#SBATCH --job-name="b_lan_eg4"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/b_lan_eg_4.out"


dir_list=$(ls /data/scratch/projects/punim1340/landis_raw/east_gippsland/*/ -d)

for directory_name in ${dir_list}
do
	
	directory_name=$directory_name sbatch /data/gpfs/projects/punim0995/rfst/slurm/job_landis.slurm
	# directory_name=$directory_name bash /data/gpfs/projects/punim0995/rfst/slurm/job_landis.slurm

done
