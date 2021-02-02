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

PRO read_vraptor, settings, n_snap, $

	mrange=mrange, num_thread=num_thread, $
	dir_raw=dir_raw, dir_lib=dir_lib, dir_save=dir_save, simname=simname, $
	column_list=column_list, flux_list=flux_list, $
	silent=silent, verbose=verbose, $
	rv_raw=rv_raw, rv_tree=rv_tree, rv_id=rv_id, rv_match=rv_match, $
	rv_prop=rv_prop, rv_gprop=rv_gprop, rv_save=rv_save, $
	skip_tree=skip_tree, skip_id=skip_id, skip_match=skip_match, $
	skip_prop=skip_prop, skip_gprop=skip_gprop, skip_save=skip_save, $
	SFR_T=SFR_t, SFR_R=SFR_r, MAG_R=MAG_r, $
	alltype=alltype, longint=longint, yzics=yzics

	;;-----
	;; Base Structure
	;;-----
	data	= {$
		rv_raw		: PTR_NEW(1), $
		rv_tree		: PTR_NEW(1)}

	;;-----
	;; Compile all procedures first
	;;-----
	void	= rv_RawCatalog(settings, ' ', run=0L)
	void	= rv_ReadTree(settings, ' ', ' ', run=0L)

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
	IF settings.verbose EQ 1L THEN read_vraptor_msg, 'Reading Tree...', /bef
	data.rv_tree	= rv_ReadTree(settings, dir_data, data, n_snap, run=settings.P_VRrun_step(1))
	IF settings.verbose EQ 1L THEN read_vraptor_msg, ' ', /aft

	STOP

;+)
;
;	dir_catalog:
;		Directory WHERE catalogs are located
;
;	dir_raw
;		Directory WHERE the raw data are located
;
;	dir_lib
;		Directory WHERE the IDL & RAMSES library exist
;
;	dir_save
;		Directory WHERE HDF5 files are saved
;
;
;	column_list:
;		List of columns of properteis to extract
;
;	
;
;-)
	;;-----
	;; Keyword setting
	;;-----

	;if KEYWORD_SET(halo) then horg='h'
	;if KEYWORD_SET(galaxy) then horg='g'
	if ~KEYWORD_SET(halo) and ~KEYWORD_SET(galaxy) then PRINT, '	***** Determine whether to read galaxy or halo (e.g., /halo or /galaxy)'
	if ~KEYWORD_SET(halo) and ~KEYWORD_SET(galaxy) then STOP

	;;-----
	;; Default setting
	;;-----
	dir_snap= dir_catalog



	;;-----
	;; Read Progenitors
	;;-----

	;if KEYWORD_SET(verbose) then tic
	;if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
	;if KEYWORD_SET(verbose) then PRINT, '        %%%%% Reading Tree		     '
	;if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '

	;if strlen(file_search(dir_snap + 'rv_tree.sav')) lt 5L or KEYWORD_SET(rv_tree) then begin
	;	if KEYWORD_SET(verbose) then PRINT, '        %%%%% (No previous works are found)'

	;	rv_readtree, output, output2, $
	;		dir_snap=dir_snap, skip=KEYWORD_SET(skip_tree), n_snap=n_snap, horg=horg, $
	;		column_list=column_list

	;	save, filename=dir_snap + 'rv_tree.sav', output2
	;endif else begin
	;        restore, dir_snap + 'rv_tree.sav'
	;endelse
	;output	= rv_makestr(output2, output=output)
	;if KEYWORD_SET(verbose) then PRINT, '        %%%%% Done in                      '
	;if KEYWORD_SET(verbose) then PRINT, ' '
	;if KEYWORD_SET(verbose) then PRINT, ' '
	;if KEYWORD_SET(verbose) then toc, /verbose

	;;-----
	;; Apply the mass limit
	;;-----
	if KEYWORD_SET(mrange) then begin
		if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
		if KEYWORD_SET(verbose) then PRINT, '        %%%%% Applying the mass cut     '
		if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
		mcut	= WHERE(output.mvir lt mrange(1) and output.mvir gt mrange(0))
		if MAX(mcut) lt 0L then PRINT, '     %%%%% (No galaxies existed in the mass range)'
		if MAX(mcut) lt 0L then STOP

		for i=0L, N_TAGS(output)-1L do begin
			if i eq 0L then output2 = create_struct(data_list(i), output.(i)(mcut))
			if i ge 1L then output2 = create_struct(output2, data_list(i), output.(i)(mcut))
		endfor

		output = output2
		output2 = 0.
		if KEYWORD_SET(verbose) then PRINT, '        %%%%% Done                      '
		if KEYWORD_SET(verbose) then PRINT, ' '
		if KEYWORD_SET(verbose) then PRINT, ' '
	endif

	;;-----
	;; Read Particle IDs
	;;-----

	if KEYWORD_SET(verbose) then tic
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Reading Particle ID       '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_id.sav')) lt 5L or KEYWORD_SET(rv_id) then begin
		if KEYWORD_SET(verbose) then PRINT, '        %%%%% (No previous works are found)'

		rv_readid, output2, dir_snap=dir_snap, horg=horg, skip=KEYWORD_SET(skip_id)

		save, filename=dir_snap + 'rv_id.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_id.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Done in                      '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then toc, /verbose

	;;-----
	;; Particle Matching
	;;-----

	if KEYWORD_SET(verbose) then tic
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Particle Matching         '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_ptcl.sav')) lt 5L or KEYWORD_SET(rv_match) then begin
		if KEYWORD_SET(verbose) then PRINT, '        %%%%% (No previous works are found)'

		if ~KEYWORD_SET(num_thread) then SPWAN, 'nproc --all', num_thread
		if ~KEYWORD_SET(num_thread) then num_tread = long(num_thread)

		rv_ptmatch, output, output2, dir_snap=dir_snap, dir_raw=dir_raw, dir_lib=dir_lib, $
			horg=horg, num_thread=num_thread, longint=KEYWORD_SET(longint), $
			n_snap=n_snap, skip=KEYWORD_SET(skip_match), yzics=KEYWORD_SET(yzics)

		SAVE, filename=dir_snap + 'rv_ptcl.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_ptcl.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Done in                      '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then toc, /verbose

	;;-----
	;; Galaxy Property
	;;-----
	if KEYWORD_SET(verbose) then tic
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Galaxy Property         '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_gprop.sav')) lt 5L or KEYWORD_SET(rv_gprop) then begin
		if KEYWORD_SET(verbose) then PRINT, '        %%%%% (No previous works are found)'

		if ~KEYWORD_SET(num_thread) then SPWAN, 'nproc --all', num_thread
		if ~KEYWORD_SET(num_thread) then num_tread = long(num_thread)

		rv_gprop, output, output2, dir_snap=dir_snap, dir_raw=dir_raw, dir_lib=dir_lib, simname=simname, $
			horg=horg, num_thread=num_thread, n_snap=n_snap, flux_list=flux_list, $
			SFR_T=SFR_t, SFR_R=SFR_R, MAG_R=MAG_R, skip=KEYWORD_SET(skip_gprop)

		save, filename=dir_snap + 'rv_gprop.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_gprop.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Done in                      '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then toc, /verbose

	;;-----
	;; HDF5 output
	;;-----
	if KEYWORD_SET(verbose) then tic
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% HDF5 Saving	             '
	if KEYWORD_SET(verbose) then PRINT, '        %%%%%                           '

	if KEYWORD_SET(rv_save) then begin
		if KEYWORD_SET(verbose) then PRINT, '        %%%%% (No previous works are found)'

		if ~KEYWORD_SET(num_thread) then SPWAN, 'nproc --all', num_thread
		if ~KEYWORD_SET(num_thread) then num_tread = long(num_thread)

		rv_save, output, dir_save=dir_save, horg=horg, num_thread=num_thread, n_snap=n_snap, column_list=column_list, flux_list=flux_list, skip=KEYWORD_SET(skip_save)
	endif
	if KEYWORD_SET(verbose) then PRINT, '        %%%%% Done in                      '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then PRINT, ' '
	if KEYWORD_SET(verbose) then toc, /verbose

	;RETURN, output
	STOP


	;;-----
	;; All DTypes
	;;-----
	if KEYWORD_SET(alltype) then output = create_struct(output, 'all', dtag)

	;;-----
	;; Nan?
	;;-----
	tagnm	= TAG_NAMES(output)
	for i=0L, N_TAGS(output)-1L do begin
		tmp	= FINITE(output.(i),/nan)
		if MAX(WHERE(tmp eq 1L) ge 0L) then $
		  PRINT, '***** read_vraptor.pro: There is a Nan value in the arrays ( ' + strtrim(tagnm(i),2) + ' )'
	endfor

	;RETURN, output
End
