FUNCTION get_cfrac, settings, rawdata, n_snap, horg=horg

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
	IF horg EQ 'h' THEN rr = rawdata.rvir * 3.086d21 / siminfo.unit_l

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

	cut	= WHERE(dom_list GE 0L, ncut)
	IF ncut EQ 0L THEN BEGIN
		PRINT, '??'
		STOP
	ENDIF
	cut2	= (ARRAY_INDICES(dom_list, cut))(1,*)
	dlist	= cut2(UNIQ(cut2(SORT(cut2)))) + 1L

	;;-----
	;; READ PTCL
	;;-----
	str	 = 'jsrd_part, part, dir="' + settings.dir_raw + '"' + $
		', snapnum=' + STRTRIM(n_snap,2) + ', num_thread=' + $
		STRTRIM(settings.num_thread,2) + ' , domlist=dlist, /silent'
	IF settings.R_orgver GE 1L THEN str +=', /newver'
	IF settings.R_longint GE 1L THEN str +=', /l64ver'
	void	= EXECUTE(str)

	dm_ind	= WHERE(part.family EQ 1L, nn_dm)
	dm_xp	= part.xp(dm_ind,*)
	dm_mm	= part.mp(dm_ind)

	;;-----
	;; GET CONTAMINATION FRAC
	;;-----
	conf_n	= DBLARR(n_gal, n_aper)
	conf_m	= DBLARR(n_gal, n_aper)

	TIC
	dmp_mass	= 1./(4096.)^3 * (siminfo.omega_M - siminfo.omega_B) / siminfo.omega_M

	ftr_name	= settings.dir_lib + 'sub_ftn/get_contam.so'
		larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= n_gal
		larr(1)	= nn_dm
		larr(2)	= n_aper
		larr(3)	= settings.num_thread
		;larr(10)= n_snap
		larr(11)= STRLEN(settings.dir_raw)

		;IF settings.R_orgver EQ 1L THEN larr(18) = 100L

		darr(0)	= dmp_mass

	void	= CALL_EXTERNAL(ftr_name, 'get_contam', $
		larr, darr, settings.dir_raw, $
		xc, yc, zc, rr, dm_xp, dm_mm, conf_n, conf_m, DOUBLE(settings.CONF_R))


	TOC, /verbose
STOP
	confrac	= {N:conf_n, M:conf_m}
	RETURN, confrac

END
