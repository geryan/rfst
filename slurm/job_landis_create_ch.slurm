#!/bin/bash

# Partition for the job:
#SBATCH -p "physical,snowy"

# Multithreaded (SMP) job: must run on one node 
#SBATCH --nodes=1

# The name of the job:
#SBATCH --job-name="landis_create_ch"

# The project ID which this job should run under:
#SBATCH --account="punim0995"

# Maximum number of tasks/CPU cores used by the job:
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# Send yourself an email when the job:
# aborts abnormally (fails)
#SBATCH --mail-type=FAIL
# begins
#SBATCH --mail-type=BEGIN
# ends successfully
#SBATCH --mail-type=END

# Use this email address:
#SBATCH --mail-user=ryange@unimelb.edu.au

# The maximum running time of the job in days-hours:mins:sec
#SBATCH --time=0-2:00:00

#SBATCH --output="/data/gphfs/projects/punim0995/rfst/slurm/slurm_output/job_landis_create_ch.out"

cd /data/gpfs/projects/punim0995/rfst/

module load r/3.6.2

Rscript --vanilla R/make_landis_scenarios_ch.R