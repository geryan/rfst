dir_list=$(ls /data/gpfs/projects/punim1340/eg_fire_calibration/*/ -d)

for directory_name in ${dir_list}
do
	
	cd $directory_name
	
	pwd
	
	cd fire
	
	ls

done