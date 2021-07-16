for i in $(seq -f "%03g" 000 102)
do
	sed -i "130c		P_TFrun_corr_nn = ${i}" ../settings.nml
	idl -e main
done
