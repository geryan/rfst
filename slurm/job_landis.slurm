#!/bin/bash

# Never edit
#SBATCH --account="punim1340"
#SBATCH --mail-user=ryange@unimelb.edu.au
#SBATCH --mail-type=ALL

# Sometimes edit
#SBATCH --partition="physical,snowy" # LANDIS-ii could not run on cloud partition in past
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# Always edit

# megabytes per process
# #

# job name from batch
# output name from batch

# days-hours:mins:sec
#SBATCH --time=1-18:00:00

source /usr/local/module/spartan_old.sh
module load  GDAL/2.2.3-GCC-6.2.0
module load LANDIS-II/7.0-GCC-6.2.0

echo $directory_name

cd $directory_name

ls # necessary to run landis otherwise fails with message about incorrect no raster bands

landis-ii scenario.txt
