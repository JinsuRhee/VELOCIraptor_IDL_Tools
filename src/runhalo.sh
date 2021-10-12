for i in $(seq -f "%04g" 1025 1025)
do
	sed -i "156c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
