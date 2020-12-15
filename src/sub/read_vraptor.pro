function read_vraptor, $
	halo=halo, galaxy=galaxy, mrange=mrange, n_snap=n_snap, num_thread=num_thread, $
	dir_catalog=dir_catalog, dir_raw=dir_raw, dir_lib=dir_lib, dir_save=dir_save, simname=simname, $
	column_list=column_list, flux_list=flux_list, $
	silent=silent, verbose=verbose, $
	rv_raw=rv_raw, rv_tree=rv_tree, rv_id=rv_id, rv_match=rv_match, $
	rv_prop=rv_prop, rv_gprop=rv_gprop, rv_save=rv_save, $
	skip_tree=skip_tree, skip_id=skip_id, skip_match=skip_match, $
	skip_prop=skip_prop, skip_gprop=skip_gprop, skip_save=skip_save, $
	SFR_T=SFR_t, SFR_R=SFR_r, MAG_R=MAG_r, $
	alltype=alltype, longint=longint, yzics=yzics


;+)
;
;	dir_catalog:
;		Directory where catalogs are located
;
;	dir_raw
;		Directory where the raw data are located
;
;	dir_lib
;		Directory where the IDL & RAMSES library exist
;
;	dir_save
;		Directory where HDF5 files are saved
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
	if keyword_set(halo) then horg='h'
	if keyword_set(galaxy) then horg='g'
	if ~keyword_set(halo) and ~keyword_set(galaxy) then print, '	***** Determine whether to read galaxy or halo (e.g., /halo or /galaxy)'
	if ~keyword_set(halo) and ~keyword_set(galaxy) then stop

	;;-----
	;; Default setting
	;;-----
	dir_snap= dir_catalog

	;;-----
	;; Read The Raw Catalogue
	;;-----
	if keyword_set(verbose) then tic
	if keyword_set(verbose) then print, '        %%%%%                           '
	if keyword_set(verbose) then print, '        %%%%% Reading The Raw Catalog...'
	if keyword_set(verbose) then print, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_io.sav')) lt 5L or keyword_set(rv_raw) then begin
		if keyword_set(verbose) then print, '        %%%%% (No previous works are found)'

		output2=rv_RawCatalog(dir_snap=dir_snap, horg=horg, column_list=column_list, silent=silent)

		save, filename=dir_snap + 'rv_io.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_io.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if keyword_set(verbose) then print, '        %%%%% Done in                      '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then toc, /verbose


	;;-----
	;; Read Progenitors
	;;-----

	if keyword_set(verbose) then tic
	if keyword_set(verbose) then print, '        %%%%%                           '
	if keyword_set(verbose) then print, '        %%%%% Reading Tree		     '
	if keyword_set(verbose) then print, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_tree.sav')) lt 5L or keyword_set(rv_tree) then begin
		if keyword_set(verbose) then print, '        %%%%% (No previous works are found)'

		rv_readtree, output, output2, $
			dir_snap=dir_snap, skip=KEYWORD_SET(skip_tree), n_snap=n_snap, horg=horg, $
			column_list=column_list

		save, filename=dir_snap + 'rv_tree.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_tree.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if keyword_set(verbose) then print, '        %%%%% Done in                      '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then toc, /verbose

	;;-----
	;; Apply the mass limit
	;;-----
	if keyword_set(mrange) then begin
		if keyword_set(verbose) then print, '        %%%%%                           '
		if keyword_set(verbose) then print, '        %%%%% Applying the mass cut     '
		if keyword_set(verbose) then print, '        %%%%%                           '
		mcut	= where(output.mvir lt mrange(1) and output.mvir gt mrange(0))
		if max(mcut) lt 0L then print, '     %%%%% (No galaxies existed in the mass range)'
		if max(mcut) lt 0L then stop

		for i=0L, n_tags(output)-1L do begin
			if i eq 0L then output2 = create_struct(data_list(i), output.(i)(mcut))
			if i ge 1L then output2 = create_struct(output2, data_list(i), output.(i)(mcut))
		endfor

		output = output2
		output2 = 0.
		if keyword_set(verbose) then print, '        %%%%% Done                      '
		if keyword_set(verbose) then print, ' '
		if keyword_set(verbose) then print, ' '
	endif

	;;-----
	;; Read Particle IDs
	;;-----

	if keyword_set(verbose) then tic
	if keyword_set(verbose) then print, '        %%%%%                           '
	if keyword_set(verbose) then print, '        %%%%% Reading Particle ID       '
	if keyword_set(verbose) then print, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_id.sav')) lt 5L or keyword_set(rv_id) then begin
		if keyword_set(verbose) then print, '        %%%%% (No previous works are found)'

		rv_readid, output2, dir_snap=dir_snap, horg=horg, skip=KEYWORD_SET(skip_id)

		save, filename=dir_snap + 'rv_id.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_id.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if keyword_set(verbose) then print, '        %%%%% Done in                      '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then toc, /verbose

	;;-----
	;; Particle Matching
	;;-----

	if keyword_set(verbose) then tic
	if keyword_set(verbose) then print, '        %%%%%                           '
	if keyword_set(verbose) then print, '        %%%%% Particle Matching         '
	if keyword_set(verbose) then print, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_ptcl.sav')) lt 5L or keyword_set(rv_match) then begin
		if keyword_set(verbose) then print, '        %%%%% (No previous works are found)'

		if ~keyword_set(num_thread) then spawn, 'nproc --all', num_thread
		if ~keyword_set(num_thread) then num_tread = long(num_thread)

		rv_ptmatch, output, output2, dir_snap=dir_snap, dir_raw=dir_raw, dir_lib=dir_lib, $
			horg=horg, num_thread=num_thread, longint=KEYWORD_SET(longint), $
			n_snap=n_snap, skip=KEYWORD_SET(skip_match), yzics=KEYWORD_SET(yzics)

		SAVE, filename=dir_snap + 'rv_ptcl.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_ptcl.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if keyword_set(verbose) then print, '        %%%%% Done in                      '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then toc, /verbose

	;;-----
	;; Galaxy Property
	;;-----
	if keyword_set(verbose) then tic
	if keyword_set(verbose) then print, '        %%%%%                           '
	if keyword_set(verbose) then print, '        %%%%% Galaxy Property         '
	if keyword_set(verbose) then print, '        %%%%%                           '

	if strlen(file_search(dir_snap + 'rv_gprop.sav')) lt 5L or keyword_set(rv_gprop) then begin
		if keyword_set(verbose) then print, '        %%%%% (No previous works are found)'

		if ~keyword_set(num_thread) then spawn, 'nproc --all', num_thread
		if ~keyword_set(num_thread) then num_tread = long(num_thread)

		rv_gprop, output, output2, dir_snap=dir_snap, dir_raw=dir_raw, dir_lib=dir_lib, simname=simname, $
			horg=horg, num_thread=num_thread, n_snap=n_snap, flux_list=flux_list, $
			SFR_T=SFR_t, SFR_R=SFR_R, MAG_R=MAG_R, skip=KEYWORD_SET(skip_gprop)

		save, filename=dir_snap + 'rv_gprop.sav', output2
	endif else begin
	        restore, dir_snap + 'rv_gprop.sav'
	endelse
	output	= rv_makestr(output2, output=output)
	if keyword_set(verbose) then print, '        %%%%% Done in                      '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then toc, /verbose

	;;-----
	;; HDF5 output
	;;-----
	if keyword_set(verbose) then tic
	if keyword_set(verbose) then print, '        %%%%%                           '
	if keyword_set(verbose) then print, '        %%%%% HDF5 Saving	             '
	if keyword_set(verbose) then print, '        %%%%%                           '

	if keyword_set(rv_save) then begin
		if keyword_set(verbose) then print, '        %%%%% (No previous works are found)'

		if ~keyword_set(num_thread) then spawn, 'nproc --all', num_thread
		if ~keyword_set(num_thread) then num_tread = long(num_thread)

		rv_save, output, dir_save=dir_save, horg=horg, num_thread=num_thread, n_snap=n_snap, column_list=column_list, flux_list=flux_list, skip=KEYWORD_SET(skip_save)
	endif
	if keyword_set(verbose) then print, '        %%%%% Done in                      '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then print, ' '
	if keyword_set(verbose) then toc, /verbose

	return, output
	stop


	;;-----
	;; All DTypes
	;;-----
	if keyword_set(alltype) then output = create_struct(output, 'all', dtag)

	;;-----
	;; Nan?
	;;-----
	tagnm	= tag_names(output)
	for i=0L, n_tags(output)-1L do begin
		tmp	= finite(output.(i),/nan)
		if max(where(tmp eq 1L) ge 0L) then $
		  print, '***** read_vraptor.pro: There is a Nan value in the arrays ( ' + strtrim(tagnm(i),2) + ' )'
	endfor

	;return, output
End
