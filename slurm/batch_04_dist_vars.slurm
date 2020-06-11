#!/bin/bash

for i in {1..60}
do
	sbatch /data/cephfs/punim0995/rfst/slurm/job_04_dist_vars.slurm $i

done
