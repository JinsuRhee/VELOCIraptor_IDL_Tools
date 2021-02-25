for i in $(seq -f "%03g" 957 957)
do
	sed -i "69c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
