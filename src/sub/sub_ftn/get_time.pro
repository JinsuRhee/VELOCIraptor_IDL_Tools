Function get_time_lbt_int, X, _extra=extra
  
        ;YZiCS2
        ;OM     = 0.311100006103516E+00
        ;OL     = 0.688899993896484E+00

        ;NH / YZiCS
        ;OM     = 0.272000014781952E+00
        ;OL     = 0.727999985218048E+00
        oM      = extra.oM
        oL      = extra.oL

        return, 1./(1.+X)/sqrt(OM*(1.+X)^3 + OL)
End

FUNCTION get_time_lbt, oM=oM, oL=oL, H0=H0, dir=dir

	zeroval	= 0.01
        tmp_red = dindgen(10000)/9999.*(1. - zeroval) + zeroval
        tmp_red = 1./tmp_red - 1. & tmp_red = reverse(tmp_red)
        tmp_gyr = tmp_red & tmp_gyr(0) = 0.
        for i=1L, n_elements(tmp_red)-1L do begin
                qsimp, 'get_time_lbt_int', 0., tmp_red(i), val, oM=oM, oL=oL
                tmp_gyr(i) = val
        endfor
                ;tmp_gyr(i) = qsimp('lbt_int',0., tmp_red(i), /double)
        ;YZiCS2
        ;tmp_gyr = tmp_gyr / 0.676600036621094E+02 * 3.08568025e19 / 3.1536000d+16

        ;NH / YZiCS
        ;tmp_gyr = tmp_gyr / 0.704000015258789E+02 * 3.08568025e19 / 3.1536000d+16

        tmp_gyr = tmp_gyr / H0 *3.08568025e19 / 3.1536000d+16

        doM = oM & doL = oL & dH0 = H0
	RETURN, [[tmp_red], [tmp_gyr]]
End

FUNCTION get_time, snap, aexp, om=om, ol=ol, h0=h0


	lbt_table	= get_time_lbt(oM=om, oL=ol, h0=h0);
	lbt_table(0,0)	= 0.
	lbt_table(*,0)	= 1./(1.+lbt_table(*,0))

	t_age	= INTERPOL(lbt_table(*,1), lbt_table(*,0), 0.d)
	lbt	= aexp
	FOR i=0L, N_ELEMENTS(snap)-1L DO BEGIN
		lbt(i)	= INTERPOL(lbt_table(*,1), lbt_table(*,0), aexp(i))
	ENDFOR
	age	= t_age - lbt
	RETURN, age
END
