for i in $(seq -f "%03g" 800 959)
do
	sed -i "81c P_VRrun_snap = [${i}, ${i}, 1L]" ../settings.nml
	idl -e main
done
