for i in $(seq -f "%04g" 0010 0099)
do
	sed -i "207c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
