#!/bin/bash

for i in {1..60}
do
	sbatch /data/cephfs/punim0995/rfst/slurm/job_03_landis_vars.slurm $i

done
