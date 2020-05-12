#!/bin/bash

file_list=$(ls /scratch/punim0995/*/ -d)

for directory_name in ${file_list}
do
	directory_name=$directory_name sbatch /home/ryange/rfst/slurm/landis_job_submission.slurm
	# directory_name=$directory_name bash /home/ryange/rfst/slurm/landis_job_submission.slurm

done
