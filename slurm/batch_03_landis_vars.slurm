#!/bin/bash

dir_list=$(ls /scratch/punim0995/*/ -d)

for dir_path in ${dir_list}
do
	sbatch /data/cephfs/punim0995/rfst/slurm/job_03_landis_vars.slurm dir_path=$dir_path

done
