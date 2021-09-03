for i in $(seq -f "%04g" 0111 0170)
do
	sed -i "154c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
