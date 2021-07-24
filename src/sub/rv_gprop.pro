FUNCTION rv_GProp, settings, dir_data, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN, PTR_NEW({ABMag:-1, SFR:-1, SFR_R:-1, SFR_T:-1, MAG_R:-1},/no_copy)
IF run EQ 1L THEN BEGIN
	IF STRLEN(FILE_SEARCH(dir_data + 'rv_gprop.sav')) GE 5L THEN BEGIN
		RESTORE, dir_data + 'rv_gprop.sav'
		RETURN, PTR_NEW(output,/no_copy)
	ENDIF ELSE BEGIN
		run	= 2L
	ENDELSE
ENDIF
IF run EQ 2L THEN BEGIN
	PRINT, '        %%%%% (No previous works are found)'

	rawdata	= *data.rv_raw
	idlist	= *data.rv_id
	ptdata	= *data.rv_ptmatch
	;;-----
	;; Settings
	;;-----
	n_gal	= N_ELEMENTS(rawdata.id)
	n_part	= N_ELEMENTS(idlist.p_id)
	n_flux	= N_ELEMENTS(settings.flux_list)
	n_sfr	= N_ELEMENTS(settings.SFR_R)
	n_magap	= N_ELEMENTS(settings.MAG_R)

	;;-----
	;; Allocate Memory
	;;-----
	fl		= DBLARR(n_part, N_ELEMENTS(settings.flux_list)) - 1.0d8

	sfactor		= DBLARR(n_part)
	gyr		= DBLARR(n_part)

	abmag		= DBLARR(n_gal, n_flux, n_magap)
	SFR		= DBLARR(n_gal, n_sfr)

	confrac		= DBLARR(n_gal, N_ELEMENTS(settings.CONF_r)) - 1.0d8
	PRINT, '        %%%%% GProp - MEMORY ALLOCATED'

	;;-----
	;; Conformal Time to SFactor and Gyr
	;;-----

	dummy	= get_gyr(ptdata.p_age, dir_raw=settings.dir_raw, $
		dir_lib=settings.dir_lib, num_thread=settings.num_thread, n_snap=n_snap)

	sfactor = dummy(*,0) & gyr = dummy(*,1)

	PRINT, '        %%%%% GProp - CONFORMAL TIME CONVERTED'

	;;-----
	;; SFR Calculation
	;;-----

	SFR	= get_sfr(rawdata.xc, rawdata.yc, rawdata.zc, rawdata.r_halfmass, $
		idlist.b_ind, idlist.u_ind, ptdata.p_pos, ptdata.p_mass, gyr, $
		SFR_T=settings.SFR_T, SFR_R=settings.SFR_R, $
		lib=settings.dir_lib, num_thread=settings.num_thread)

	;;-----
	;; CLUMP CORRECTION
	;;-----
	cut	= WHERE(SFR(*,0) GT $
		rawdata.mass_tot / (settings.SFR_T(0)*1e9) * settings.clump_mfrac, ncut)
	SFR2	= SFR
	isclump	= LONARR(N_ELEMENTS(rawdata.id)) - 1L
	IF ncut GE 1L THEN BEGIN
		isclump(cut)	= 1L

		FOR i=0L, ncut-1L DO BEGIN
			ind	= cut(i)
			hostid	= rawdata.hostHaloID(ind)
			IF hostid LT 0L THEN CONTINUE
			cut2	= WHERE(rawdata.ID EQ hostid)
			SFR2(cut2,*)	+= SFR(ind,*)
			SFR2(ind,*)	= 0.
		ENDFOR
	ENDIF
	output	= CREATE_STRUCT('SFR', SFR, 'SFR_clumpcorr', SFR2, 'isclump', isclump)
	PRINT, '        %%%%% GProp - SFRs are calculated'

	;;-----
	;; Magnitude
	;;-----

	abmag	= get_mag(rawdata.xc, rawdata.yc, rawdata.zc, rawdata.r_halfmass, $
		idlist.b_ind, idlist.u_ind, ptdata.p_pos, ptdata.p_met, gyr, ptdata.p_mass, $
		MAG_R=settings.MAG_R, flux_list=settings.flux_list, $
		lib=settings.dir_lib, num_thread=settings.num_thread)

	output	= CREATE_STRUCT(output, 'ABMag', abmag)

	output	= CREATE_STRUCT(output, 'SFR_R', settings.SFR_R, 'SFR_T', settings.SFR_T, $
		'MAG_R', settings.MAG_R)
	PRINT, '        %%%%% GProp - Magnitudes are calculated'

	;;-----
	;; Contamination Fraction
	;;-----
	confrac	= get_cfrac(settings, rawdata, confrac, n_snap, horg=settings.horg)

	output	= CREATE_STRUCT(output, 'CONFRAC', confrac)
	PRINT, '        %%%%% GProp - Contamination fractions are calculated'

	SAVE, filename=dir_data + 'rv_gprop.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
