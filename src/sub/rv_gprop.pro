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

	output	= CREATE_STRUCT('SFR', SFR)
		;tmp0	= 'output2 = CREATE_STRUCT('

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

	SAVE, filename=dir_data + 'rv_gprop.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
