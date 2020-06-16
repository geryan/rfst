#!/bin/bash

#SBATCH -p "snowy"

#SBATCH --ntasks=1

#SBATCH --time 2:00

#SBATCH --mail-user="ryange@unimelb.edu.au"

#SBATCH --mail-type=ALL


module load R/3.6.1-GCC-6.2.0

cd /data/cephfs/punim0995/rfst

Rscript --vanilla R/test_cores.R