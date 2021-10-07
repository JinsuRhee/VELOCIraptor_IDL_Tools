for i in $(seq -f "%04g" 0960 0964)
do
	sed -i "156c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
