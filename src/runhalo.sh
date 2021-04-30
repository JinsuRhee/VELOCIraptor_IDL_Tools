for i in $(seq -f "%04g" 0850 1026)
do
	sed -i "85c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
