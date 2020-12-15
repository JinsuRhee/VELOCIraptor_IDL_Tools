PRO rv_gprop, output, output2, $
	dir_snap=dir_snap, dir_raw=dir_raw, dir_lib=dir_lib, simname=simname, $
	horg=horg, num_thread=num_thread, n_snap=n_snap, flux_list=flux_list, $
	SFR_T=SFR_T, SFR_R=SFR_R, MAG_R=MAG_R, skip=skip

	;;-----
	;; Skip Process
	;;-----
	IF skip EQ 1L THEN BEGIN
		output2	= {ABMag:-1, SFR:-1, SFR_R:-1, SFR_T:-1, MAG_R:-1}
		RETURN
	ENDIF

	;;-----
	;; Settings
	;;-----
	n_gal	= n_elements(output.id)
	n_part	= n_elements(output.p_id)
	n_flux	= n_elements(flux_list)
	n_sfr	= n_elements(SFR_R)
	n_magap	= n_elements(MAG_R)
	;;-----
	;; Allocate Memory
	;;-----
	fl		= dblarr(n_part, n_elements(flux_list)) - 1.0d8

	sfactor		= dblarr(n_part)
	gyr		= dblarr(n_part)

	abmag		= dblarr(n_gal, n_flux, n_magap)
	SFR		= dblarr(n_gal, n_sfr)

	print, '        %%%%% GProp - MEMORY ALLOCATED'

	;;-----
	;; Conformal Time to SFactor and Gyr
	;;-----

	dummy	= get_gyr(output.p_age, dir_raw=dir_raw, dir_lib=dir_lib, simname=simname, $
		num_thread=num_thread, n_snap=n_snap)

	sfactor = dummy(*,0) & gyr = dummy(*,1)

	print, '        %%%%% GProp - CONFORMAL TIME CONVERTED'

	;;-----
	;; SFR Calculation
	;;-----

	SFR	= get_sfr(output.xc, output.yc, output.zc, output.r_halfmass, $
		output.b_ind, output.u_ind, output.p_pos, output.p_mass, gyr, $
		SFR_T=SFR_T, SFR_R=SFR_R, lib=dir_lib, num_thread=num_thread)

	output2	= create_struct('SFR', SFR)
		;tmp0	= 'output2 = create_struct('

		;FOR i=0L, n_sfr-1L DO BEGIN
		;	tmp = 'SFR'
		;	IF(SFR_R(i) GT 0) THEN tmp = tmp + $
		;		'_' + string(long(SFR_r(i)),format='(I1.1)') + '_'
		;	IF(SFR_R(i) LT 0) THEN tmp = tmp + '_T_'

		;	tmp	= tmp + string(long(SFR_T(i)*1000.),format='(I4.4)')

		;	tmp0	= tmp0 + '"' + tmp + '"' + $
		;		', SFR(*,' + strtrim(i,2) + ')'
		;	IF i LT n_sfr-1L THEN tmp0 = tmp0 + ','
		;ENDFOR
		;tmp0	= tmp0 + ')'
		;void	= execute(tmp0)

	print, '        %%%%% GProp - SFRs are calculated'

	;;-----
	;; Magnitude
	;;-----

	abmag	= get_mag(output.xc, output.yc, output.zc, output.r_halfmass, $
		output.b_ind, output.u_ind, output.p_pos, output.p_met, gyr, output.p_mass, $
		MAG_R=MAG_R, flux_list=flux_list, lib=dir_lib, num_thread=num_thread)

	output2	= create_struct(output2, 'ABMag', abmag)

	output2 = create_struct(output2, 'SFR_R', SFR_R, 'SFR_T', SFR_T, $
		'MAG_R', MAG_R)
	print, '        %%%%% GProp - Magnitudes are calculated'

	RETURN
End
