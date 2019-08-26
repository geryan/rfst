#!/bin/bash

#SBATCH --job-name=landis_s1_09
#SBATCH -p cloud

#SBATCH --ntasks=1

#SBATCH --time 0-12:00:00

#SBATCH --mail-user="ryange@unimelb.edu.au"
#SBATCH --mail-type=ALL

module load LANDIS-II/7.0-GCC-6.2.0

cd s1_09

landis-ii scenario_s1_09.txt