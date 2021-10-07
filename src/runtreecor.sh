for i in $(seq -f "%03g" 000 270)
do
	sed -i "176c		P_TFrun_corr_nn = ${i}" ../settings.nml
	idl -e main
done
