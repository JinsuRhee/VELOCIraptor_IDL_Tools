for i in $(seq -f "%03g" 000 019)
do
	sed -i "238c		P_TFrun_corr_nn = ${i}" ../settings.nml
	idl -e main
done
