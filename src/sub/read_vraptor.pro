PRO read_vraptor_msg, str, bef=bef, aft=aft
	IF KEYWORD_SET(bef) THEN BEGIN
		tic
		PRINT, '        %%%%%                           '
		PRINT, '        %%%%% ', str
		PRINT, '        %%%%%                           '
	ENDIF ELSE IF KEYWORD_SET(aft) THEN BEGIN
		PRINT, '        %%%%% Done in                      '
		PRINT, ' '
		PRINT, ' '
		toc, /verbose
	ENDIF
END

PRO read_vraptor, settings, n_snap

	;mrange=mrange, num_thread=num_thread, $
	;dir_raw=dir_raw, dir_lib=dir_lib, dir_save=dir_save, simname=simname, $
	;column_list=column_list, flux_list=flux_list, $
	;silent=silent, verbose=verbose, $
	;rv_raw=rv_raw, rv_tree=rv_tree, rv_id=rv_id, rv_match=rv_match, $
	;rv_prop=rv_prop, rv_gprop=rv_gprop, rv_save=rv_save, $
	;skip_tree=skip_tree, skip_id=skip_id, skip_match=skip_match, $
	;skip_prop=skip_prop, skip_gprop=skip_gprop, skip_save=skip_save, $
	;SFR_T=SFR_t, SFR_R=SFR_r, MAG_R=MAG_r, $
	;alltype=alltype, longint=longint, yzics=yzics

	;;-----
	;; Base Structure
	;;-----
	data	= {$
		rv_raw		: PTR_NEW(1), $
		;rv_tree		: PTR_NEW(1), $
		rv_id		: PTR_NEW(1), $
		rv_ptmatch	: PTR_NEW(1), $
		rv_gprop	: PTR_NEW(1)}

	;;-----
	;; Compile all procedures first
	;;-----
	void	= rv_RawCatalog	(settings, ' ', run=0L)
	;void	= rv_ReadTree	(settings, ' ', ' ', 1L, run=0L)
	void	= rv_ReadID	(settings, ' ', ' ', run=0L)
	void	= rv_PTMatch	(settings, ' ', ' ', 1L, run=0L)
	void	= rv_GProp	(settings, ' ', ' ', 1L, run=0L)
	rv_save, settings, ' ', 1L, run=0L

	;;-----
	;; Path Settings
	;;-----
	dir_data	= settings.dir_catalog + $
		settings.dir_catalog_pre + STRING(n_snap,format='(I4.4)') + $
		settings.dir_catalog_suf + '/'

	IF STRLEN(FILE_SEARCH(dir_data)) LE 5L THEN RETURN


	;;-----
	;; Read The Raw Catalogue
	;;-----
	IF settings.verbose EQ 1L THEN read_vraptor_msg, 'Reading The Raw Catalog...', /bef
	data.rv_raw	= rv_RawCatalog(settings, dir_data, run=settings.P_VRrun_step(0))
	IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	;;-----
	;; Read Tree
	;;-----
	;IF settings.verbose EQ 1L THEN read_vraptor_msg, 'Reading Tree...', /bef
	;data.rv_tree	= rv_ReadTree(settings, dir_data, data, $
	;	n_snap, run=settings.P_VRrun_step(1))
	;IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	;;-----
	;; Read Particle IDs
	;;-----
	IF settings.verbose EQ 1L THEN read_vraptor_msg, 'Reading Particle IDs...', /bef
	data.rv_id	= rv_ReadID(settings, dir_data, data, run=settings.P_VRrun_step(2))
	IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	;;-----
	;; Particle Matching
	;;-----
	IF settings.verbose EQ 1L THEN read_vraptor_msg, 'Particle Matching...', /bef
	data.rv_ptmatch	= rv_PTMatch(settings, dir_data, data, $
		n_snap, run=settings.P_VRrun_step(3))
	IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	;;-----
	;; Galaxy Property
	;;-----
	IF settings.verbose EQ 1L THEN read_vraptor_msg, 'Compute Galaxy Property...', /bef
	data.rv_gprop	= rv_GProp(settings, dir_data, data, $
		n_snap, run=settings.P_VRrun_step(4))
	IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	;;-----
	;; Clump Find
	;;-----
	cut	= WHERE((*data.rv_gprop).SFR(*,0) GT $
		(*data.rv_raw).mass_tot / (settings.SFR_T(0)*1e9) * settings.clump_mfrac)

	isclump		= LONARR(N_ELEMENTS((*data.rv_raw).id)) - 1L
	isclump(cut)	= 1L

	data	= CREATE_STRUCT(data,'isclump',isclump)

	;;-----
	;; HDF5 output
	;;-----
	IF settings.verbose EQ 1L THEN read_vraptor_msg, 'HDF5 Saving...', /bef
	rv_save, settings, data, n_snap, run=settings.P_VRrun_step(5)
	IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	;;-----
	;; All DTypes
	;;-----
	;if KEYWORD_SET(alltype) then output = create_struct(output, 'all', dtag)

	;;-----
	;; Nan?
	;;-----
	tagnm	= TAG_NAMES(*data.rv_raw)
	for i=0L, N_TAGS(*data.rv_raw)-1L do begin
		tmp	= FINITE((*data.rv_raw).(i),/nan)
		if MAX(WHERE(tmp eq 1L) ge 0L) then $
		  PRINT, '***** read_vraptor.pro: There is a Nan value in the arrays ( ' + strtrim(tagnm(i),2) + ' )'
	endfor

	;;-----
	;; CLEAR POINTER
	;;-----
	PTR_FREE, data.rv_raw
	;PTR_FREE, data.rv_tree
	PTR_FREE, data.rv_id
	PTR_FREE, data.rv_ptmatch
	PTR_FREE, data.rv_gprop
	;RETURN, output
End
