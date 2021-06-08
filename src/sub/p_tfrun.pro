
;;-----
;; READ HDF5
;;-----
FUNCTION p_tfrun_rdhdf5, fname, treeset

	fid	= H5F_OPEN(fname)

	did	= H5D_OPEN(fid, treeset.tag_num)
	num = H5D_READ(did) & H5D_CLOSE, did

	did	= H5D_OPEN(fid, treeset.tag_off)
	off = H5D_READ(did) & H5D_CLOSE, did

	did	= H5D_OPEN(fid, treeset.tag_result)
	res = H5D_READ(did) & H5D_CLOSE, did

	did	= H5D_OPEN(fid, treeset.tag_merit)
	mer = H5D_READ(did) & H5D_CLOSE, did

	did	= H5D_OPEN(fid, treeset.tag_npart)
	npart = H5D_READ(did) & H5D_CLOSE, did

	did	= H5A_OPEN_NAME(fid, treeset.tag_nlink)
	nlink = H5A_READ(did) & H5A_CLOSE, did

	did	= H5D_OPEN(fid, 'ID')
	id = H5D_READ(did) & H5D_CLOSE, did

	H5F_CLOSE, fid

	dum	= {num:num, off:off, res:res, merit:mer, npart:npart, nlink:nlink, id:id}
	RETURN, dum
END

;;-----
;; TREE SET
;;-----
FUNCTION p_tfrun_set, settings
	IF settings.P_TFrun_treedir EQ 'desc' THEN BEGIN
		treeset	= {$
			N0		: settings.P_TFrun_snap(0), $
			N1		: settings.P_TFrun_snap(1), $
			DN		: settings.P_TFrun_snap(2), $
			tag_num	: 'NumDesc', $
			tag_off	: 'DescOffsets', $
			tag_result	: 'Descendants', $
			tag_npart	: 'DescNpart', $
			tag_merit	: 'Merits', $
			tag_nlink	: 'Nsteps_search_new_links' $
			}
	ENDIF ELSE IF settings.P_TFrun_treedir EQ 'prog' THEN BEGIN
		treeset	= {$
			N0		: settings.P_TFrun_snap(1), $
			N1		: settings.P_TFrun_snap(0), $
			DN		: -settings.P_TFrun_snap(2), $
			tag_num	: 'NumProgen', $
			tag_off	: 'ProgenOffsets', $
			tag_result	: 'Progenitors', $
			tag_npart	: 'ProgenNpart', $
			tag_merit	: 'Merits', $
			tag_nlink	: 'Nsteps_search_new_links' $
			}
	ENDIF ELSE BEGIN
		PRINT, '%123123	-----'
		PRINT, '	Incorrect Tree direction: settings.P_TFrun_treedir'
		PRINT, '	'
		STOP
	ENDELSE

	RETURN, treeset
END
;;-----
;; Find Next snapshot
;;-----
FUNCTION p_tfrun_findnextsnap, settings, snap_curr

	snap_next	= snap_curr
	goout	= -1L
	REPEAT BEGIN
		IF settings.p_tfrun_treedir EQ 'desc' THEN snap_next ++
		IF settings.p_tfrun_treedir EQ 'prog' THEN snap_next --
		dumfname	= settings.dir_tree + 'tree.snapshot_' + $
			STRING(snap_next,format='(I4.4)') + 'VELOCIraptor.tree'
		IF STRLEN(FILE_SEARCH(dumfname)) GE 5L THEN $
			goout	= 1L

		IF snap_next LT settings.p_tfrun_snap(0) OR $
			snap_next GT settings.p_tfrun_snap(1) THEN BEGIN
			goout		= 1L
			snap_next	= -1L
		ENDIF
	ENDREP UNTIL goout GE 1L
	RETURN, snap_next
END
;;-----
;; CLEAR TREE
;;-----
PRO p_tfrun_clearbranch, tree, evoldum, ind, gind
	evoldum.ID(ind)		= evoldum.ID(gind)
	evoldum.snap(ind)	= evoldum.snap(gind)
	evoldum.merit(ind)	= evoldum.merit(gind)
	evoldum.ID(gind)	= -1L
	evoldum.snap(gind)	= -1L
	evoldum.merit(gind)	= -1.0d

	tree(ind)	= tree(gind)
	tree(gind).ID		= -1L
	tree(gind).snap		= -1L
	tree(gind).p_snap	= -1L
	tree(gind).p_id		= -1L
	tree(gind).d_snap	= -1L
	tree(gind).d_id		= -1L
	tree(gind).endind	= -1L
	tree(gind).numprog	= 1L

	gind --
END
;;-----
;; Link progenitor
;;-----
PRO p_tfrun_proglink, settings, tree, ind0, endind, n_comp, p_id, snap_curr, merit
	tree(ind0).numprog ++
	nn	= tree(ind0).numprog-2L
	tree(ind0).m_id(nn) 	= p_id
	tree(ind0).m_snap(nn)	= snap_curr
	tree(ind0).m_merit(nn)	= merit
	tree(ind0).m_BID(nn)	= n_comp
END
;;-----
;; Link
;;-----
PRO p_tfrun_link, settings, tree, gind, evoldum, snap_curr, snap_next, $
	dum_id, dum_mer, t_id

	tree(gind).endind ++
	evoldum.id(gind)	= dum_id
	evoldum.snap(gind)	= snap_next
	evoldum.merit(gind)	= dum_mer

	tree(gind).ID(tree(gind).endind)	= t_id
	tree(gind).snap(tree(gind).endind)	= snap_curr
	IF settings.p_tfrun_treedir EQ 'desc' THEN BEGIN
		tree(gind).p_snap(tree(gind).endind+1L)	= snap_curr
		tree(gind).p_ID(tree(gind).endind+1L)		= t_ID
		tree(gind).p_merit(tree(gind).endind+1L)	= dum_mer
		tree(gind).d_snap(tree(gind).endind)		= snap_next
		tree(gind).d_ID(tree(gind).endind)		= dum_id
	ENDIF ELSE IF settings.p_tfrun_treedir EQ 'prog' THEN BEGIN
		tree(gind).p_snap(tree(gind).endind+1)		= snap_next
		tree(gind).p_ID(tree(gind).endind+1)		= dum_id
		tree(gind).p_meirt(tree(gind).endind+1)		= dum_mer
		tree(gind).d_snap(tree(gind).endind)		= snap_curr
		tree(gind).d_ID(tree(gind).endind)		= t_ID
	ENDIF
END
;;-----
;; Finish Branch
;;-----
PRO p_tfrun_finishbranch, settings, tree, complete_tree, n_comp, ind, stat
	a	= tree(ind)
	nn	= a.endind
	nn2	= a.numprog-2L
	b	= {ID:a.ID(0L:nn), snap:a.snap(0L:nn), stat:stat, $
		p_snap:a.p_snap(0L:nn), p_id:a.p_id(0L:nn), p_merit:a.p_merit(0L:nn), $
		m_ID:a.m_ID(0:nn2), m_merit:a.m_merit(0:nn2), m_bid:a.m_bid(0L:nn2), m_snap:a.m_snap(0L:nn2), $
		d_snap:a.d_snap(0L:nn), d_id:a.d_id(0L:nn,*), endind:a.endind, numprog:a.numprog}
	complete_tree(n_comp)	= PTR_NEW(b,/no_copy)
	n_comp	++
END
;;-----
;; Matching
;;-----
PRO p_tfrun_match, settings, treelog, tree, complete_tree, n_comp, $
	t_curr, g_curr, g_next, gind, evoldum, snap_curr, snap_next

	mlimit	= settings.p_tfrun_meritlimit
	FOR i=0L, N_ELEMENTS(t_curr.id)-1L DO BEGIN
		IF t_curr.num(i) EQ 0L THEN CONTINUE

		treelog.n_all ++

		ind1	= t_curr.off(i)
		ind2	= t_curr.off(i) + t_curr.num(i) - 1L
		dum_id	= t_curr.res(ind1:ind2)
		dum_part= t_curr.npart(ind1:ind2)
		dum_mer	= t_curr.merit(ind1:ind2)

		;;----- Matching by one with most largest Merit
		cut	= MIN(WHERE(dum_mer EQ MAX(dum_mer)))
		cut2	= WHERE(g_next.ID EQ dum_id(cut) AND g_next.npart EQ dum_part(cut), ncut)

		IF ncut EQ 0L THEN CONTINUE

		;;----- Link
		;;;; Already existed?
		cut_exist	= WHERE(evoldum.snap EQ snap_curr AND $
			evoldum.id EQ t_curr.ID(i), ncut_exist)

		IF ncut_exist EQ 0L THEN BEGIN	;; NEW MERGER TREE
			gind ++
			p_tfrun_link, settings, tree, gind, evoldum, snap_curr, snap_next, $
				dum_id(cut), dum_mer(cut), t_curr.ID(i)
			treelog.n_new ++

		ENDIF ELSE BEGIN			;; ALREADY EXIST
			
			;; IF the second merit is too low, just merged
			maxmerit	= MAX(evoldum.merit(cut_exist))
			ind0		= MIN(cut_exist(WHERE(evoldum.merit(cut_exist) EQ maxmerit)))
			endind		= tree(ind0).endind+1
			FOR li=0L, N_ELEMENTS(cut_exist)-1L DO BEGIN
				ind	= cut_exist(li)
				IF ind EQ ind0 THEN BEGIN;evoldum.merit(ind) EQ maxmerit THEN BEGIN
					;; Primary Link
					p_tfrun_link, settings, tree, ind, $
						evoldum, snap_curr, snap_next, $
						dum_id(cut), dum_mer(cut), t_curr.ID(i)
					treelog.n_link ++
				ENDIF ELSE IF ABS(tree(ind).snap(0)-snap_curr) GE 10L AND $
					tree(ind).snap(0) NE -1L THEN BEGIN
					;; Secondary Link
					;;	Merit is low but has a reasonable tree length
					p_tfrun_proglink, settings, tree, ind0, endind, n_comp, $
						tree(ind).id(tree(ind).endind), $
						tree(ind).snap(tree(ind).endind), $
						evoldum.merit(ind)
					p_tfrun_finishbranch, settings, tree, complete_tree, n_comp, ind, 'sub'
					p_tfrun_clearbranch, tree, evoldum, ind, gind
					treelog.n_link2 ++
				ENDIF ELSE BEGIN
					;; Too short branch or too low merit
					;;	Clear it
					p_tfrun_clearbranch, tree, evoldum, ind, gind
					treelog.n_broken ++
				ENDELSE
			ENDFOR
		ENDELSE

		;;----- Matching by Most massive one (not yet)
	ENDFOR
END
;;-----
;; LAST SNAP
;;-----
PRO p_tfrun_lastsnap, settings, complete_tree, n_comp, tree, evoldum, gind, g_curr, snap_curr

	;tree	= tree(0L:gind)

	;;-----
	;; Scan by IDs of the last snapshot
	;;-----
	FOR i=0L, N_ELEMENTS(g_curr)-1L DO BEGIN
		cut_exist	= WHERE(evoldum.snap EQ snap_curr AND $
			evoldum.id EQ g_curr(i).ID, ncut_exist)

		IF ncut_exist EQ 1L THEN BEGIN	;; Finish Single Branch
			tree(cut_exist).endind ++
			tree(cut_exist).ID(tree(cut_exist).endind)	= g_curr(i).ID
			tree(cut_exist).snap(tree(cut_exist).endind)	= snap_curr
		ENDIF ELSE IF ncut_exist GE 2L THEN BEGIN  ;; Merger happened at the last snapshot
			maxmerit	= MAX(evoldum.merit(cut_exist))
			ind0		= cut_exist(WHERE(evoldum.merit(cut_exist) EQ maxmerit))
			endind		= tree(ind0).endind + 1
			FOR li=0L, N_ELEMENTS(cut_exist)-1L DO BEGIN
				ind	= cut_exist(li)
				IF evoldum.merit(ind) EQ maxmerit THEN BEGIN
					;; Primary Link
					tree(ind).endind ++
					tree(ind).ID(tree(ind).endind)	= g_curr(i).ID
					tree(ind).snap(tree(ind).endind)= snap_curr
				ENDIF ELSE IF ABS(tree(ind).snap(0) - snap_curr) GE 10L AND $
					tree(ind).snap(0) NE -1L THEN BEGIN
					;; Secondary Link
					;;	Merit is low but has a reasonable tree length
					p_tfrun_proglink, settings, tree, ind0, endind, n_comp, $
						tree(ind).id(tree(ind).endind), $
						tree(ind).snap(tree(ind).endind), $
						evoldum.merit(ind)
					p_tfrun_finishbranch, settings, tree, complete_tree, n_comp, ind, 'sub'
					p_tfrun_clearbranch, tree, evoldum, ind, gind
				ENDIF ELSE BEGIN
					p_tfrun_clearbranch, tree, evoldum, ind, gind
				ENDELSE

			ENDFOR

		ENDIF

	ENDFOR

	tree	= tree(0L:gind)

		IF settings.p_tfrun_treedir EQ 'prog' THEN BEGIN
			PRINT, 'progenitor case!!!!!!!!!!!!!!!'
			STOP
		ENDIF
END
;;-----
;; REMOVE FROM THE MAIN BRANCH
;;-----
PRO p_tfrun_remove, settings, evoldum, tree, gind, complete_tree, n_comp, snap_curr, nlink

	gind2	= gind
	FOR i=0L, gind2 DO BEGIN
		IF i GT gind THEN BREAK
		IF ABS(tree(i).snap(tree(i).endind)-snap_curr) GE nlink+1L THEN BEGIN
			IF ABS(tree(i).snap(0)-snap_curr) GE 10L THEN BEGIN
				p_tfrun_finishbranch, settings, tree, complete_tree, n_comp, i, 'main'
				p_tfrun_clearbranch, tree, evoldum, i, gind
			ENDIF ELSE BEGIN
				p_tfrun_clearbranch, tree, evoldum, i, gind
			ENDELSE
		ENDIF
	ENDFOR
END
;;-----
;; Main Part
;;-----
PRO p_tfrun, settings

	;;-----
	;; Basic settings
	;;-----
	treeset	= p_tfrun_set(settings)

	;;-----
	;; Allocate Memory
	;;-----
	dumarr	= LONARR(ABS(treeset.N1-treeset.N0)+1L)-1L
	dumarr2	= LONARR(1000L) - 1L
	dumstr	= {ID:dumarr, snap:dumarr, $
		p_snap:dumarr, p_ID:dumarr, p_merit:DOUBLE(dumarr), $
		d_snap:dumarr, d_ID:dumarr, endind:-1L, $
		m_ID:dumarr2, m_snap:dumarr2, m_merit:DOUBLE(dumarr2), m_BID:dumarr2, $
		numprog:1L}
	tree	= REPLICATE(dumstr, 10000L)
	complete_tree	= PTRARR(100000L)

	evoldum	= {ID:LONARR(10000L)-1L, snap:LONARR(10000L)-1L, merit:DBLARR(10000L)-1.d}

	;;-----
	;; GO FORWARD
	;;-----

	gind	= -1L
	n_comp	= 0L
	treelog	= {n_new:0L, n_link:0L, n_link2:0L, n_link3:0L, n_broken:0L, n_all:0L}
	FOR i=treeset.N0, treeset.N1, treeset.DN DO BEGIN
		;;-----
		;; SNAPSHOT CHECK
		;;-----
		dumfname	= settings.dir_catalog + 'snap_' + STRING(i,format='(I4.4)')
		IF STRLEN(FILE_SEARCH(dumfname)) LE 5L THEN CONTINUE

		snap_curr	= i
		snap_next	= p_tfrun_findnextsnap(settings,snap_curr)

		;;-----
		;; LAST SNAPSHOT
		;;-----
		IF i EQ treeset.N1 OR snap_next EQ -1L THEN BEGIN
			g_curr	= f_rdgal(snap_curr, ['ID', 'npart'], id0=-1L, $
				dir=settings.dir_save, horg=settings.horg)
			p_tfrun_lastsnap, settings, complete_tree, n_comp, $
				tree, evoldum, gind, g_curr, snap_curr

			FOR j=0L, gind DO $
				IF ABS(tree(j).snap(0)-snap_curr) GE 10L THEN $
					p_tfrun_finishbranch, settings, tree, complete_tree, n_comp, j, 'main'

			complete_tree	= complete_tree(0L:n_comp-1L)
			BREAK
		ENDIF
		;;-----
		;; LOAD NEEDED DATA
		;;-----
		treefname	= settings.dir_tree + 'tree.snapshot_' + $
			STRING(snap_curr,format='(I4.4)') + 'VELOCIraptor.tree'
		t_curr	= p_tfrun_rdhdf5(treefname, treeset)
		g_curr	= f_rdgal(snap_curr, ['ID', 'npart'], id0=-1, $
			dir=settings.dir_save, horg=settings.horg)
		g_next	= f_rdgal(snap_next, ['ID', 'npart'], id0=-1, $
			dir=settings.dir_save, horg=settings.horg)

		;;-----
		;; MATCHING
		;;-----
		p_tfrun_match, settings, treelog, tree, complete_tree, n_comp, $
			t_curr, g_curr, g_next, gind, evoldum, snap_curr, snap_next

		;;-----
		;; REMOVE FROM BRANCH
		;;-----
		p_tfrun_remove, settings, evoldum, tree, gind, complete_tree, n_comp, $
			snap_curr, t_curr.nlink

		;;-----
		;; For the multi-snapshot search
		;;-----
		IF t_curr.nlink GE 2L THEN BEGIN
			FOR i2=0L, t_curr.nlink-2L DO BEGIN
				snap_next	= p_tfrun_findnextsnap(settings,snap_next)
				IF snap_next EQ -1L THEN CONTINUE
				g_next		= f_rdgal(snap_next, ['ID', 'npart'], id0=-1, $
					dir=settings.dir_save, horg=settings.horg)
				p_tfrun_match, settings, treelog, tree, complete_tree, n_comp, $
					t_curr, g_curr, g_next, gind, evoldum, snap_curr, snap_next
			ENDFOR
		ENDIF

		PRINT, i, ' / ', treeset.N1
		PRINT, '	ALL : ' +  STRTRIM(treelog.n_all,2) + $
			' / NEW :' + STRTRIM(treelog.n_new,2) +  $
			' / LINK : ' + STRTRIM(treelog.n_link,2) + $
			;' ' + STRTRIM(treelog.n_link2,2) + $
			;' ' + STRTRIM(treelog.n_link3,2) + $
			' / Broken : ' + STRTRIM(treelog.n_broken,2) + $
			' / NGal : ' + STRTRIM(gind+n_comp,2) + $
			' / Gind : ' + STRTRIM(gind,2)

		FOR ii=0L, N_TAGS(treelog)-1L DO treelog.(ii) = 0L

	ENDFOR
	PRINT, 'Mergered branch mutual link'
	PRINT, 'finish branch modulate m_ID'
	STOP
END
