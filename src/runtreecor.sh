for i in $(seq -f "%02g" 19 99)
do
	sed -i "130c		P_TFrun_corr_nn = ${i}" ../settings.nml
	idl -e main
done
