for i in $(seq -f "%04g" 0243 0243)
do
	sed -i "99c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
