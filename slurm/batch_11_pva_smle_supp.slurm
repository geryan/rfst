#!/bin/bash


# Always edit
#SBATCH --job-name="batch_11"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/batch_11.out"


for i in {12, 17, 19, 23, 37, 45, 63, 82, 102, 129, 137, 145, 163, 165, 172, 181, 187, 198, 203, 210, 216}
do

	JOB_NAME="j_11_${i}"

	OUT_NAME="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_11_${i}.out"

	sbatch --job-name=$JOB_NAME --output=$OUT_NAME  /data/gpfs/projects/punim0995/rfst/slurm/job_11_pva_smle.slurm $i

done
