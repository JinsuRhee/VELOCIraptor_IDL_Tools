;;-----
;; TREE MATCHING
;;	) normalmatch
;;		Find prog/desc by the most largest merit/mass
;;-----
PRO rv_ReadTree_normalmatch, r_bymass, r_bymerit, treedum, gal

	FOR i=0L, N_ELEMENTS(r_bymass(*,0))-1L DO BEGIN
		IF treedum.num(i) EQ 0L THEN CONTINUE
		ind1	= treedum.off(i)
		ind2	= treedum.off(i) + treedum.num(i) - 1L
		r_id	= treedum.res(ind1:ind2)
		r_mr	= treedum.merit(ind1:ind2)

		mass_dum	= DBLARR(N_ELEMENTS(r_id)) - 1.
		FOR j=0L, N_ELEMENTS(r_id) - 1L DO BEGIN
			cut	= WHERE(gal.id EQ r_id(j), ncut)
			IF ncut EQ 0L THEN BEGIN
				PRINT, '%123123	no matched galaxy'
				DOC_LIBRARY, 'rv_ReadTree_match'
				STOP
			ENDIF
			;; Follow progenitors when the merit greater than
			;; 1e-3, based on the TREEFROG paper Fig.10
			IF r_mr(j) GT 1e-3 THEN mass_dum(j) = gal.mass_tot(cut)
		ENDFOR

		mass_ind	= REVERSE(SORT(mass_dum))
		merit_ind	= REVERSE(SORT(r_mr))

		match_bymass	= r_id(mass_ind)
		match_bymerit	= r_id(merit_ind)

		r_bymass(i)	= match_bymass(0)
		r_bymerit(i)	= match_bymerit(0)
	ENDFOR
END

PRO rv_ReadTree_matrixmatch, reftree, ref, target, m_bymass, m_bymerit

	n_ref	= N_ELEMENTS(ref.id)
	n_tar	= N_ELEMENTS(target.id)
	n_candi	= N_ELEMENTS(m_bymass(0,*))

	mat_merit	= DBLARR(n_ref, n_tar) - 1.0d
	mat_mass	= DBLARR(n_ref, n_tar) - 1.0d

	FOR i=0L, n_ref-1L DO BEGIN
		IF reftree.num (i) EQ 0L THEN CONTINUE
		ind1	= reftree.off(i)
		ind2	= reftree.off(i) + reftree.num(i) - 1L
		r_id	= reftree.res(ind1:ind2)
		r_mr	= reftree.merit(ind1:ind2)

		FOR j=0L, N_ELEMENTS(r_id) - 1L DO BEGIN
			cut	= WHERE(target.id EQ r_id(j), ncut)
			IF ncut GE 1L THEN BEGIN
				IF r_mr(j) GT 1e-3 THEN BEGIN
					;; Follow progenitors when the merit greater than
					;; 1e-3, based on the TREEFROG paper Fig.10
					mat_merit(i,cut)= r_mr(j)
					mat_mass(i,cut)	= ref.mass_tot(i)
				ENDIF
			ENDIF
		ENDFOR
	ENDFOR

	FOR i=0L, n_tar-1L DO BEGIN
		dum_mass	= mat_mass(*,i)
		dum_merit	= mat_merit(*,i)
		cut	= WHERE(dum_mass GT 0., ncut)
		IF ncut EQ 0L THEN CONTINUE
		dum_merit = dum_merit(cut) & dum_mass = dum_mass(cut) & dum_id = ref.id(cut)

		sind_mass	= REVERSE(SORT(dum_mass))
		sind_merit	= REVERSE(SORT(dum_merit))

		id_mass	= dum_id(sind_mass)
		id_merit= dum_id(sind_merit)
		endind	= n_candi < N_ELEMENTS(dum_id)
		m_bymass(i,0L:endind-1L)	= id_mass(0L:endind-1L)
		m_bymerit(i,0L:endind-1L)	= id_merit(0L:endind-1L)
	ENDFOR
END

;;-----
;; READ HDF5 ROUTINE
;;-----
FUNCTION rv_ReadTree_rdhdf5, fname, tag_num, tag_off, tag_res, tag_mer

	fid	= H5F_OPEN(fname)

	did	= H5D_OPEN(fid, tag_num)
	num	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, tag_off)
	off	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, tag_res)
	res	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, tag_mer)
	mer	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5D_OPEN(fid, 'ID')
	id	= H5D_READ(did)
	H5D_CLOSE, did

	H5F_CLOSE, fid

	dum	= {num:num, off:off, res:res, merit:mer, id:id}
	RETURN, dum
END

FUNCTION rv_ReadTree, settings, dir_data, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN, PTR_NEW({prog_bymass:-1, prog_bymerit:-1, $
	desc_bymass:-1, desc_bymerit:-1},/no_copy)
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
	;; Find Snapshot Index and Search next/prev snapshots
	;;-----
	treelist	= FILE_SEARCH(settings.dir_tree + 'tree.snapshot_*.tree')

	snap_prg	= -1L
	snap_des	= -1L
	snap_now	= -1L
	FOR i=0L, N_ELEMENTS(treelist)-1L DO BEGIN
		dum	= STRPOS(treelist(i), 'snapshot_')
		numsnap	= LONG(STRMID(treelist(i), dum+9, 4))
		IF numsnap EQ n_snap THEN BEGIN
			snap_now = i
			BREAK
		ENDIF
	ENDFOR

	IF snap_now EQ -1L THEN BEGIN
		PRINT, 'rv_readtree: could not find corresponding tree data'
		DOC_LIBRARY, 'rv_readtree'
		STOP
	ENDIF

	snap_prg	= snap_now - 1L
	snap_des	= snap_now + 1L

	IF snap_now EQ 0L THEN snap_prg = -1L
	IF snap_now EQ N_ELEMENTS(treelist)-1 THEN snap_des = -1L

	;;-----
	;; I/O
	;;-----
	ptr_prg	= !NULL
	IF snap_prg NE -1L THEN BEGIN
		dum	= STRPOS(treelist(snap_prg), 'snapshot_')
		numsnap	= LONG(STRMID(treelist(snap_prg), dum+9, 4))
		dum	= settings.dir_catalog + settings.dir_catalog_pre + $
			STRING(numsnap,format='(I4.4)') + $
			settings.dir_catalog_suf + '/'
		ptr_prg	= rv_RawCatalog(settings, dum, run=1L)
	ENDIF

	ptr_des	= !NULL
	IF snap_des NE -1L THEN BEGIN
		dum	= STRPOS(treelist(snap_des), 'snapshot_')
		numsnap	= LONG(STRMID(treelist(snap_des), dum+9, 4))
		dum	= settings.dir_catalog + settings.dir_catalog_pre + $
			STRING(numsnap,format='(I4.4)') + $
			settings.dir_catalog_suf + '/'
		ptr_des	= rv_RawCatalog(settings, dum, run=1L)
	ENDIF

	;;-----
	;; Link Tree
	;;	) Currently, only trace the main branch
	;;-----
	desc_bymass	= LONARR(N_ELEMENTS((*data.rv_raw).id)) - 1L
	desc_bymerit	= LONARR(N_ELEMENTS((*data.rv_raw).id)) - 1L
	prog_bymass	= LONARR(N_ELEMENTS((*data.rv_raw).id),10) - 1L
	prog_bymerit	= LONARR(N_ELEMENTS((*data.rv_raw).id),10) - 1L

	IF settings.treedir EQ 'prg' THEN BEGIN
		tree_num	= 'NumProgen'
		tree_off	= 'ProgenOffsets'
		tree_result	= 'Progenitors'
		tree_merit	= 'Merits'
	ENDIF ELSE IF settings.treedir EQ 'des' THEN BEGIN
		tree_num	= 'NumDesc'
		tree_off	= 'DescOffsets'
		tree_result	= 'Descendants'
		tree_merit	= 'Merits'
	ENDIF

	;;----- Descendants Tree
	IF settings.treedir EQ 'des' THEN BEGIN
		IF snap_des GE 0L THEN BEGIN
			treedum	= rv_readtree_rdhdf5(treelist(snap_now), $
				tree_num, tree_off, tree_result, tree_merit)
			rv_readtree_normalmatch, desc_bymass, desc_bymerit, $
				treedum, *ptr_des
		ENDIF
		IF snap_prg GE 0L THEN BEGIN
			treedum	= rv_readtree_rdhdf5(treelist(snap_prg), $
				tree_num, tree_off, tree_result, tree_merit)
			rv_readtree_matrixmatch, treedum, *ptr_prg, *data.rv_raw, $
				prog_bymass, prog_bymerit
		ENDIF
	ENDIF

	;;----- Progenitor Tree
	IF settings.treedir EQ 'prg' THEN BEGIN
		PRINT, '%123123 progenitor tree part has not been implemented yet'
		PRINT, '%123123		Use the same routine as the descendant case but'
		PRINT, '%123123		note that use different mass for the progenitor'
		STOP
		IF snap_des GE 0L THEN BEGIN

		ENDIF

		IF snap_prg GE 0L THEN BEGIN

		ENDIF
	ENDIF
	output	= {prog_bymass:prog_bymass, prog_bymerit:prog_bymerit, $
		desc_bymass:desc_bymass, desc_bymerit:desc_bymerit}

	SAVE, filename=dir_data + 'rv_tree.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
