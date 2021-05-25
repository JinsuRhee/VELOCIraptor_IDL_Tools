for i in $(seq -f "%04g" 0600 0900)
do
	sed -i "90c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
