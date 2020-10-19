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

# Always edit
#SBATCH --job-name="job_05"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_05.out"


# days-hours:mins:sec
#SBATCH --time 0-00:30:00


module load gcccore/8.3.0
module load gcc/8.3.0
module load openmpi/3.1.4
module load udunits/2.2.26
module load geos/3.7.2-python-3.7.4
module load openssl/1.1.1f
module load netcdf/4.7.1
module load gdal/3.0.2-python-3.7.4
module load rgdal/1.4-4-r-3.6.0
module load r/3.6.2

cd /data/gpfs/projects/punim0995/rfst

Rscript --vanilla R/05_geophysical_variables.R