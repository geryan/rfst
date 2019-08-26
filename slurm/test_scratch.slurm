#!/bin/bash

#SBATCH --job-name=landis_test_scratch
#SBATCH -p snowy

#SBATCH --ntasks=1

#SBATCH --time 1-00:00:00

#SBATCH --mail-user="ryange@unimelb.edu.au"
#SBATCH --mail-type=ALL

module load LANDIS-II/7.0-GCC-6.2.0

cd /scratch/punim0995

cd s1_01

ls

landis-ii scenario_s1_01.txt