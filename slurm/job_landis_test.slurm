#!/bin/bash

# Never edit
#SBATCH --account="punim0995"
#SBATCH --mail-user=ryange@unimelb.edu.au
#SBATCH --mail-type=ALL

# Sometimes edit
#SBATCH --partition="physical,snowy" # LANDIS-ii could not run on cloud partition in past
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# Always edit


# Always edit
#SBATCH --job-name="j_landis_test"
#SBATCH --output="/data/gpfs/projects/punim0995/rfst/slurm/slurm_output/job_landis_test.out"


# days-hours:mins:sec
#SBATCH --time=1-00::00

#module load gcccore/8.3.0
#module load dotnet-sdk/3.1.300-linux-x64
#module load landis-ii/7.0

source /usr/local/module/spartan_old.sh
module load  GDAL/2.2.3-GCC-6.2.0
module load LANDIS-II/7.0-GCC-6.2.0


cd /data/scratch/projects/punim0995/landis_raw/TH00_rcp45_NB_01

ls # necessary to run landis otherwise fails with message about incorrect no raster bands

landis-ii scenario.txt

