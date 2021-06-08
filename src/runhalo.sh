for i in $(seq -f "%04g" 0100 0100)
do
	sed -i "90c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
