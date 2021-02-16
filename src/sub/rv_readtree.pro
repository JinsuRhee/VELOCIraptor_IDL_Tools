FUNCTION rv_ReadTree, settings, dir_data, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN, PTR_NEW({progs:-1, progs_merit:-1},/no_copy)
IF run EQ 1L THEN BEGIN
	IF STRLEN(FILE_SEARCH(dir_data + 'rv_tree.sav')) GE 5L THEN BEGIN
		RESTORE, dir_data + 'rv_tree.sav'
		RETURN, PTR_NEW(output,/no_copy)
	ENDIF ELSE BEGIN
		run	= 2L
	ENDELSE
ENDIF
IF run EQ 2L THEN BEGIN
	PRINT, '        %%%%% (No previous works are found)'

	;;-----
	;; Find Snapshot Index
	;;-----
	treelist	= FILE_SEARCH(settings.dir_tree + 'tree.snapshot_*.tree')

	numsnap_pr	= 0L
	FOR i=0L, N_ELEMENTS(treelist)-1L DO BEGIN
		dum	= STRPOS(treelist(i), 'snapshot_')
		numsnap	= LONG(STRMID(treelist(i), dum+9, 4))
		IF numsnap EQ n_snap THEN BEGIN
			snapind = i
			BREAK
		ENDIF
		numsnap_pr	= numsnap
	ENDFOR

	;;----- Skip the first snapshot
	IF snapind EQ 0L THEN $
		RETURN, PTR_NEW({progs_bymass:-1, progs_bymerit:-1},/no_copy)

	;;-----
	;; I/O
	;;-----
	fname	= treelist(snapind)

	;;-----
	;; READ HDF
	;;-----
	fid	= H5F_OPEN(fname)

	did	= H5D_OPEN(fid, 'NumProgen')
	prognum	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, 'ProgenOffsets')
	progoff	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, 'Progenitors')
	prog	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, 'Merits')
	merits	= H5D_READ(did)
	H5D_CLOSE, did

	H5F_CLOSE, fid

	;;-----
	;; READ PREVIOUS SNAPDATA
	;;-----
	dum	= settings.dir_catalog + settings.dir_catalog_pre + $
		STRING(numsnap_pr,format='(I4.4)') + $
		settings.dir_catalog_suf + '/'
	ptr_pre	= rv_RawCatalog(settings, dum, run=1L)

	outpre	= *ptr_pre
	output	= *data.rv_raw

	;;-----
	;; Find Progenitors and List by their Mass / Distance
	;;-----
	plist_mass	= LONARR(N_ELEMENTS(output.id),10) - 1L
	plist_merit	= LONARR(N_ELEMENTS(output.id),10) - 1L
	FOR i=0L, N_ELEMENTS(output.id)-1L DO BEGIN
		IF prognum(i) EQ 0L THEN CONTINUE
		ind1	= progoff(i)
		ind2	= progoff(i) + prognum(i) - 1L
		prog_id	= prog(ind1:ind2)
		prog_mr	= merits(ind1:ind2)

		mass_dum	= DBLARR(N_ELEMENTS(prog_id)) - 1.

		FOR j=0L, N_ELEMENTS(prog_id)-1L DO BEGIN
			cut	= WHERE(outpre.id EQ prog_id(j))
			mass_dum(j) = outpre.Mass_tot(cut)
		ENDFOR

		;PRINT, 'modify the way of finding progenitor by using merit'
		;STOP
		mass_ind	= REVERSE(SORT(mass_dum))
		merit_ind	= REVERSE(SORT(prog_mr))

		prog_id_bymass	= prog_id(mass_ind)
		prog_id_bymerit	= prog_id(merit_ind)

		endind	= N_ELEMENTS(prog_id) < N_ELEMENTS(plist_mass(0,*))
		prog_id_bymass	= prog_id_bymass(0L:endind-1L)
		prog_id_bymerit	= prog_id_bymerit(0L:endind-1L)

		plist_mass(i,0:endind-1L)	= prog_id_bymass
		plist_merit(i,0:endind-1L)	= prog_id_bymerit
	ENDFOR

	output	= {progs_bymass:plist_mass, progs_bymerit:plist_merit}
	SAVE, filename=dir_data + 'rv_tree.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
