#!/bin/bash

# Never edit
#SBATCH --account="punim0995"
#SBATCH --mail-user=ryange@unimelb.edu.au
#SBATCH --mail-type=ALL

# Sometimes edit
#SBATCH --partition="physical,snowy"

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# Always edit
#SBATCH --job-name="job_landis_create_ch"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_landis_create_ch.out"

# days-hours:mins:sec
#SBATCH --time=0-2:00:00

# Location to run job from
cd /data/gpfs/projects/punim0995/rfst/

# Modules
module load r/3.6.2

# Commands
Rscript --vanilla R/make_landis_scenarios_ch.R