for i in $(seq -f "%03g" 000 150)
do
	sed -i "240c		P_TFrun_corr_nn = ${i}" ../settings.nml
	idl -e main
done
