for i in $(seq -f "%04g" 0300 0399)
do
	sed -i "90c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
