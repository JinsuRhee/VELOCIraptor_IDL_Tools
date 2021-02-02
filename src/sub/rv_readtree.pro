FUNCTION rv_ReadTree, settings, dir_data, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN, PTR_NEW({progs:-1},/no_copy)
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
	OPENR, 10, settings.dir_tree + 'tree.snaplist.txt'
	str	= ' '

	snaplist= LONARR(FILE_LINES(settings.dir_tree + 'tree.snaplist.txt'))
	snapind = -1L
	FOR i=0L, FILE_LINES(settings.dir_tree + 'tree.snaplist.txt')-1L DO BEGIN
		READF, 10, str
		dum	= STRPOS(str, 'snap_')
		snaplist(i)	= LONG(STRMID(str, dum+5, 4))
		IF snaplist(i) EQ n_snap THEN snapind = i
	ENDFOR
	CLOSE, 10

	;;----- Skip the first snapshot
	IF snapind EQ 0L THEN $
		RETURN, PTR_NEW({progs:-1},/no_copy)

	str	= snapind + snaplist(0)
	str	= STRING(str, format='(I4.4)')

	;;-----
	;; I/O
	;;-----
	fname	= settings.dir_tree + 'tree.snapshot_' + str + $
		'.VELOCIraptor.tree'

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
		STRING(snaplist(snapind-1L),format='(I4.4)') + $
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

		PRINT, 'modify the way of finding progenitor by using merit'
		STOP
		sort_ind= REVERSE(SORT(mass_dum))
		prog_id	= prog_id(sort_ind)
		IF(N_ELEMENTS(prog_id) GT N_ELEMENTS(plist_mass(0,*))) THEN BEGIN
			prog_id	= prog_id(0L:N_ELEMENTS(plist_mass(0,*))-1L)
			prog_mr = prog_mr(0L:N_ELEMENTS(plist_mass(0,*))-1L)
		ENDIF
		plist_mass(i,0:N_ELEMENTS(prog_id)-1L)	= prog_id
		plist_merit(i,0:N_ELEMENTS(prog_id)-1L)	= prog_mr
	ENDFOR

	output	= {progs:plist_mass, progs_merit:plist_merit}
	SAVE, filename=dir_data + 'rv_tree.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
