
;;-----
;; READ HDF5 ROUTINE
;;-----
FUNCTION rv_ReadTree_rdhdf5, fname, tag_num, tag_off, tag_res, tag_mer, tag_npart

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

	did	= H5D_OPEN(fid, tag_npart)
	npart	= H5D_READ(did)
	H5D_CLOSE, did

	did	= H5A_OPEN_NAME(fid, 'Nsteps_search_new_links')
	nlink	= H5A_READ(did)
	H5A_CLOSE, did

	H5F_CLOSE, fid

	dum	= {num:num, off:off, res:res, merit:mer, id:id, npart:npart, nlink:nlink}
	RETURN, dum
END

;;-----
;; TREE MATCHING
;;	) normalmatch
;;		Find prog/desc by the most largest merit/mass
;;-----
PRO rv_ReadTree_normalmatch, settings, r_bymass, r_bymerit, r_snapshot, treedum, gal, $
	treelist, snap_target, $
	d_tree=d_tree

	FOR i=0L, N_ELEMENTS(r_bymass(*,0))-1L DO BEGIN
		IF treedum.num(i) EQ 0L THEN CONTINUE
		ind1	= treedum.off(i)
		ind2	= treedum.off(i) + treedum.num(i) - 1L
		r_id	= treedum.res(ind1:ind2)
		r_part	= treedum.npart(ind1:ind2)
		r_mr	= treedum.merit(ind1:ind2)

		mass_dum	= DBLARR(N_ELEMENTS(r_id)) - 1.
		FOR j=0L, N_ELEMENTS(r_id) - 1L DO BEGIN
			cut	= WHERE(gal.id EQ r_id(j) AND gal.npart EQ r_part(j), ncut)

			dumname	= STRPOS(treelist(snap_target), 'snapshot_')
			numsnap	= LONG(STRMID(treelist(snap_target), dumname+9, 4))
			snap_matched	= numsnap

			;; For the multiple snapshot search
			IF treedum.nlink GE 2L AND ncut EQ 0L THEN BEGIN
				snapdum	= snap_target
				FOR nl=0L, treedum.nlink-2L DO BEGIN
					IF d_tree EQ 'desc' THEN snapdum ++
					IF d_tree EQ 'prog' THEN snapdum --
					dumname	= STRPOS(treelist(snapdum), 'snapshot_')
					numsnap	= LONG(STRMID(treelist(snapdum), dumname+9, 4))
					dumname	= settings.dir_catalog + settings.dir_catalog_pre + $
						STRING(numsnap,format='(I4.4)') + $
						settings.dir_catalog_suf + '/'
					galdumpt= rv_RawCatalog(settings,dumname, run=1L)
					galdum	= *galdumpt & PTR_FREE, galdumpt

					cut	= WHERE(galdum.id EQ r_id(j) AND $
						galdum.npart EQ r_part(j), ncut)

					IF ncut GE 1L THEN BREAK
				ENDFOR
				snap_matched	= numsnap
				IF r_mr(j) GT 1e-3 THEN mass_dum(j) = galdum.mass_tot(cut)
			ENDIF ELSE BEGIN
				;; Follow progenitors when the merit greater than
				;; 1e-3, based on the TREEFROG paper Fig.10
				IF r_mr(j) GT 1e-3 THEN mass_dum(j) = gal.mass_tot(cut)
			ENDELSE

			IF ncut EQ 0L THEN BEGIN
				PRINT, '%123123	no matched galaxy'
				DOC_LIBRARY, 'rv_ReadTree_match'
				STOP
			ENDIF
		ENDFOR

		mass_ind	= REVERSE(SORT(mass_dum))
		merit_ind	= REVERSE(SORT(r_mr))

		match_bymass	= r_id(mass_ind)
		match_bymerit	= r_id(merit_ind)

		r_bymass(i)	= match_bymass(0)
		r_bymerit(i)	= match_bymerit(0)
		r_snapshot(i)	= snap_matched
	ENDFOR

END

PRO rv_ReadTree_matrixmatch, settings, ref, treelist, snap_target, $
	tree_num, tree_off, tree_result, tree_npart, tree_merit, $
	m_bymass, m_bymerit, m_matsnap_mass, m_matsnap_merit, $
	d_tree=d_tree

	;;-----
	;; Get n_link first
	;;-----
	treedum	= rv_readtree_rdhdf5(treelist(snap_target), $
		tree_num, tree_off, tree_result, tree_merit, tree_npart)

	n_candi	= N_ELEMENTS(m_bymass(0,*))
	n_ref	= N_ELEMENTS(ref.id)
	n_link	= treedum.nlink

	;;-----
	;; REPLICATE MATRIX FOR THE MULTIPLE SNAPSHOT SEARCH
	;;-----
	m_bymass2	= LONARR(n_ref, n_candi*n_link)
	m_massdum	= DBLARR(n_ref, n_candi*n_link)
	m_bymerit2	= LONARR(n_ref, n_candi*n_link)
	m_meritdum	= DBLARR(n_ref, n_candi*n_link)
	m_matsnap2	= LONARR(n_ref, n_candi*n_link)

	snapdum	= snap_target

	FOR li=0L, n_link-1L DO BEGIN
		;; Read Tree
		treedum	= rv_readtree_rdhdf5(treelist(snapdum), $
			tree_num, tree_off, tree_result, tree_merit, tree_npart)

		;; Get Snapshot Number
		dumname	= STRPOS(treelist(snapdum), 'snapshot_')
		numsnap	= LONG(STRMID(treelist(snapdum), dumname+9, 4))

		;; Read Galaxies
		dumname	= settings.dir_catalog + settings.dir_catalog_pre + $
			STRING(numsnap,format='(I4.4)') + $
			settings.dir_catalog_suf + '/'
		galdumpt= rv_RawCatalog(settings,dumname, run=1L)
		galdum	= *galdumpt & PTR_FREE, galdumpt


		;; Match
		n_target	= N_ELEMENTS(treedum.id)
		mat_merit	= DBLARR(n_ref, n_target) - 1.0d
		mat_mass	= DBLARR(n_ref, n_target) - 1.0d

		FOR i=0L, n_target-1L DO BEGIN
			IF treedum.num(i) EQ 0L THEN CONTINUE
			ind1	= treedum.off(i)
			ind2	= treedum.off(i) + treedum.num(i) - 1L
			r_id	= treedum.res(ind1:ind2)
			r_mr	= treedum.merit(ind1:ind2)
			r_part	= treedum.npart(ind1:ind2)

			FOR j=0L, N_ELEMENTS(r_id)-1L DO BEGIN
				cut	= WHERE(ref.id EQ r_id(j) AND ref.npart EQ r_part(j), ncut)
				IF ncut EQ 0L THEN CONTINUE
				IF r_mr(j) GT 1e-3 THEN BEGIN
					;; Follow progenitors when the merit greater than
					;; 1e-3, based on the TREEFROG paper Fig.10
					mat_merit(cut,i)	= r_mr(j)
					mat_mass(cut,i)		= galdum.mass_tot(i)
				ENDIF
			ENDFOR
		ENDFOR

		;; EXTRACT
		FOR i=0L, n_ref-1L DO BEGIn
			dum_mass	= mat_mass(i,*)
			dum_merit	= mat_merit(i,*)
			cut	= WHERE(dum_mass GT 0., ncut)
			IF ncut EQ 0L THEN CONTINUE
			dum_merit = dum_merit(cut) & dum_mass = dum_mass(cut) & dum_id = galdum.id(cut)

			sind_mass	= REVERSE(SORT(dum_mass))
			sind_merit	= REVERSE(SORT(dum_merit))

			id_mass	= dum_id(sind_mass)
			id_merit= dum_id(sind_merit)
			dum_mass= dum_mass(sind_mass)
			dum_merit=dum_merit(sind_merit)

			endind	= n_candi < N_ELEMENTS(dum_id)
			m_bymass2 (i,0L+n_candi*li:endind-1L+n_candi*li)	= id_mass(0L:endind-1L)
			m_massdum (i,0L+n_candi*li:endind-1L+n_candi*li)	= dum_mass(0L:endind-1L)
			m_bymerit2(i,0L+n_candi*li:endind-1L+n_candi*li)	= id_merit(0L:endind-1L)
			m_meritdum(i,0L+n_candi*li:endind-1L+n_candi*li)	= dum_merit(0L:endind-1L)
			m_matsnap2(i,0L+n_candi*li:endind-1L+n_candi*li)	= numsnap
		ENDFOR

		;; Update
		IF d_tree EQ 'prog' THEN snapdum --
		IF d_tree EQ 'desc' THEN snapdum ++

	ENDFOR

	;;-----
	;; Extract upto n_candi-th
	;;-----
	FOR i=0L, n_ref-1L DO BEGIN
		;; By mass
		mdum	= m_massdum(i,*)
		iddum	= m_bymass2(i,*)
		snapdum	= m_matsnap2(i,*)
		idum	= REVERSE(SORT(mdum))
		iddum = iddum(idum) & snapdum = snapdum(idum)

		endind	= n_candi < N_ELEMENTS(iddum)
		m_bymass(i,0L:endind-1L)	= iddum(0L:endind-1L)
		m_matsnap_mass(i,0L:endind-1L)	= snapdum(0L:endind-1L)

		;; By merit
		mdum	= m_meritdum(i,*)
		iddum	= m_bymerit2(i,*)
		snapdum	= m_matsnap2(i,*)
		idum	= REVERSE(SORT(mdum))
		iddum = iddum(idum) & snapdum = snapdum(idum)

		endind	= n_candi < N_ELEMENTS(iddum)
		m_bymerit(i,0L:endind-1L)	= iddum(0L:endind-1L)
		m_matsnap_merit(i,0L:endind-1L)	= snapdum(0L:endind-1L)
	ENDFOR
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
	desc_matsnap	= LONARR(N_ELEMENTS((*data.rv_raw).id)) - 1L
	prog_bymass	= LONARR(N_ELEMENTS((*data.rv_raw).id),10) - 1L
	prog_bymerit	= LONARR(N_ELEMENTS((*data.rv_raw).id),10) - 1L
	prog_matsnap_mass	= LONARR(N_ELEMENTS((*data.rv_raw).id),10) - 1L
	prog_matsnap_merit	= LONARR(N_ELEMENTS((*data.rv_raw).id),10) - 1L
	IF settings.treedir EQ 'prg' THEN BEGIN
		tree_num	= 'NumProgen'
		tree_off	= 'ProgenOffsets'
		tree_result	= 'Progenitors'
		tree_npart	= 'ProgenNpart'
		tree_merit	= 'Merits'
	ENDIF ELSE IF settings.treedir EQ 'des' THEN BEGIN
		tree_num	= 'NumDesc'
		tree_off	= 'DescOffsets'
		tree_result	= 'Descendants'
		tree_npart	= 'DescNpart'
		tree_merit	= 'Merits'
	ENDIF

	;;----- Descendants Tree
	IF settings.treedir EQ 'des' THEN BEGIN
		IF snap_des GE 0L THEN BEGIN
			treedum	= rv_readtree_rdhdf5(treelist(snap_now), $
				tree_num, tree_off, tree_result, tree_merit, tree_npart)
			rv_readtree_normalmatch, settings, desc_bymass, desc_bymerit, desc_matsnap, $
				treedum, *ptr_des, treelist, snap_des, d_tree='desc'

		ENDIF
		IF snap_prg GE 0L THEN BEGIN
			rv_readtree_matrixmatch, settings, *data.rv_raw, $
				treelist, snap_prg, $
				tree_num, tree_off, tree_result, tree_npart, tree_merit, $
				prog_bymass, prog_bymerit, prog_matsnap_mass, prog_matsnap_merit, $
				d_tree='prog'
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
		prog_matsnap_mass:prog_matsnap_mass, prog_matsnap_merit:prog_matsnap_merit, $
		desc_bymass:desc_bymass, desc_bymerit:desc_bymerit, desc_matsnap:desc_matsnap}

	SAVE, filename=dir_data + 'rv_tree.sav', output
	RETURN, PTR_NEW(output,/no_copy)
ENDIF
END
