#!/bin/bash

#SBATCH -p "physical,snowy,cloud"

#SBATCH --ntasks=1

#SBATCH --time 1:00:00

#SBATCH --mail-user="ryange@unimelb.edu.au"

#SBATCH --mail-type=ALL


module load UDUNITS/2.2.26-GCC-6.2.0

module load GEOS/3.6.2-spartan_gcc-6.2.0-Python-3.7.3

module load OpenSSL/1.1.0-GCC-6.2.0

module load netCDF/4.5.0-GCC-6.2.0 # needs to be older version or doesn't work for some reason

module load R/3.6.1-GCC-6.2.0

cd /data/cephfs/punim0995/rfst

Rscript --vanilla R/05_geophysical_variables.R