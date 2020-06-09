#!/bin/bash

#SBATCH --job-name=landis_ch_upload_mflux
#SBATCH -p "cloud,physical,snowy"
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4

#SBATCH --time=2-00:00:00

#SBATCH --mail-user="ryange@unimelb.edu.au"
#SBATCH --mail-type=ALL


export http_proxy=http://wwwproxy.unimelb.edu.au:8000
export https_proxy=$http_proxy
export ftp_proxy=$http_proxy

module load unimelb-mf-clients

unimelb-mf-upload --mf.config /home/ryange/.Arcitecta/upload/mflux.cfg   --nb-workers 4  --namespace /projects/proj-6300_rfa_data-1128.4.228/landis_raw /scratch/punim0995/
