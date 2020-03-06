#!/bin/bash

#SBATCH -p "physical,snowy" # must run on these not on cloud or fails 

#SBATCH --ntasks=1

#SBATCH --time 1-00

#SBATCH --mail-user="ryange@unimelb.edu.au"
#SBATCH --mail-type=ALL

module load LANDIS-II/7.0-GCC-6.2.0

echo $directory_name

cd $directory_name

ls # necessary to run landis otherwise fails with message about incorrect no raster bands

landis-ii scenario.txt
