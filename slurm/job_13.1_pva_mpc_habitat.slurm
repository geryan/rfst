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

# megabytes per process
#SBATCH --mem=12800

# job name from batch
# output name from batch

# days-hours:mins:sec
#SBATCH --time=0-01:00:00

# commands from batch
i=$1

# modules
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

# 
cd /data/gpfs/projects/punim0995/rfst

# Commands
Rscript --vanilla R/spartan/spartan_13.1_pva_mpc_habitat.R $i

