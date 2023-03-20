FUNCTION rv_HProp, settings, dir_data, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN, PTR_NEW({Cfrac:-1},/no_copy)
IF run EQ 1L THEN BEGIN
	IF STRLEN(FILE_SEARCH(dir_data + 'rv_hprop.sav')) GE 5L THEN BEGIN
		RESTORE, dir_data + 'rv_hprop.sav'
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

	;;-----
	;; Allocate Memory
	;;-----
	confrac		= DBLARR(n_gal, N_ELEMENTS(settings.CONF_r)) - 1.0d8

	PRINT, '        %%%%% HProp - MEMORY ALLOCATED'

	;;-----
	;; Contamination Fraction
	;;-----
	confrac	= get_cfrac(settings, rawdata, confrac, n_snap, horg=settings.horg)

	output	= CREATE_STRUCT('CONFRAC', confrac)
	PRINT, '        %%%%% GProp - Contamination fractions are calculated'

	SAVE, filename=dir_data + 'rv_hprop.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
