


FUNCTION get_cfrac, settings, rawdata, confrac, n_snap

	n_gal	= N_ELEMENTS(rawdata.ID)
	n_aper	= N_ELEMENTS(settings.CONF_R)
	r_max	= MAX(settings.CONF_R)

	;;-----
	;; RD INFO
	;;-----
	infoname	= settings.dir_raw + 'output_' + STRING(n_snap, format='(I5.5)') + $
		'/info_' + STRING(n_snap, format='(I5.5)') + '.txt'
	rd_info, siminfo, file=infoname

	n_mpi	= N_ELEMENTS(siminfo.hindex(*,0))

	;;-----
	;; RE SCAILING
	;;-----
	xc	= rawdata.xc * 3.086d21 / siminfo.unit_l
	yc	= rawdata.yc * 3.086d21 / siminfo.unit_l
	zc	= rawdata.zc * 3.086d21 / siminfo.unit_l
	rr	= rawdata.r_halfmass * 3.086d21 / siminfo.unit_l

	;;-----
	;; GET DOMAIN
	;;-----
	dom_list	= LONARR(n_gal, n_mpi) - 1L

	ftr_name	= settings.dir_lib + 'sub_ftn/find_domain.so'
		larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= n_gal
		larr(1)	= n_mpi
		larr(2)	= settings.num_thread

		darr(0)	= r_max

	void	= CALL_EXTERNAL(ftr_name, 'find_domain', $
		xc, yc, zc, rr, siminfo.hindex, siminfo.levmax, $
		dom_list, larr, darr)

	;;-----
	;; GET CONTAMINATION FRAC
	;;-----
	TIC
	dmp_mass	= 1./(4096.)^3 * (siminfo.omega_M - siminfo.omega_B) / siminfo.omega_M

	ftr_name	= settings.dir_lib + 'sub_ftn/get_contam.so'
		larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= n_gal
		larr(1)	= n_mpi
		larr(2)	= n_aper
		larr(3)	= settings.num_thread
		larr(10)= n_snap
		larr(11)= STRLEN(settings.dir_raw)

		IF settings.R_orgver EQ 1L THEN larr(18) = 100L

		darr(0)	= dmp_mass

	void	= CALL_EXTERNAL(ftr_name, 'get_contam', $
		larr, darr, settings.dir_raw, $
		xc, yc, zc, rr, dom_list, confrac, DOUBLE(settings.CONF_R))

	TOC, /verbose
	RETURN, confrac

END
