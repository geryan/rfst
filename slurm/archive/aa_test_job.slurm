#!/bin/bash

# Never edit
#SBATCH --account="punim0995"
#SBATCH --mail-user=ryange@unimelb.edu.au
#--mail-type=ALL

# Sometimes edit
#SBATCH --partition="physical,snowy"
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# Always edit


# days-hours:mins:sec
#SBATCH --time=0-00:01:00

i=$1

echo THIS IS SPARTAN $i


