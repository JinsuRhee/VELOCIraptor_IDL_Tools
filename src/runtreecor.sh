for i in $(seq -f "%03g" 014 120)
do
	sed -i "177c		P_TFrun_corr_nn = ${i}" ../settings.nml
	idl -e main
done
