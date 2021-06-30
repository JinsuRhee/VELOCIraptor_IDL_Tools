

;;-----
;; UTILITIES
;;-----

;; GET PARTICLE LIST
FUNCTION p_tfrun_getpid, settings, id0, snap0

	fname	= settings.dir_catalog
	IF settings.horg EQ 'g' THEN fname = fname + 'VR_Galaxy/snap_'
	IF settings.horg EQ 'h' THEN fname = fname + 'VR_Halo/snap_'
	fname	= fname + STRING(snap0,format='(I4.4)') + '/GAL_' + $
		STRING(id0,format='(I6.6)') + '.hdf5'

	;; HDF5 open
	fid = H5F_OPEN(fname) & did = H5D_OPEN(fid, '/P_Prop/P_ID')
	pid = H5D_READ(did) & H5D_CLOSE, did & H5F_CLOSE, fid
	RETURN, pid
END
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
	IF settings.P_TFrun_treedir EQ 'des' THEN BEGIN
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
	ENDIF ELSE IF settings.P_TFrun_treedir EQ 'prg' THEN BEGIN
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
		IF settings.p_tfrun_treedir EQ 'des' THEN snap_next ++
		IF settings.p_tfrun_treedir EQ 'prg' THEN snap_next --
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
	tree(gind).p_merit	= -1.d
	tree(gind).d_snap	= -1L
	tree(gind).d_id		= -1L
	tree(gind).endind	= -1L
	tree(gind).m_id		= -1L
	tree(gind).m_snap	= -1L
	tree(gind).m_merit	= -1.d
	tree(gind).m_bid	= -1L
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
	IF snap_curr EQ tree(gind).snap(tree(gind).endind-1L) THEN STOP ;;123123
	IF settings.p_tfrun_treedir EQ 'des' THEN BEGIN
		tree(gind).p_snap(tree(gind).endind+1L)	= snap_curr
		tree(gind).p_ID(tree(gind).endind+1L)		= t_ID
		tree(gind).p_merit(tree(gind).endind+1L)	= dum_mer
		tree(gind).d_snap(tree(gind).endind)		= snap_next
		tree(gind).d_ID(tree(gind).endind)		= dum_id
	ENDIF ELSE IF settings.p_tfrun_treedir EQ 'prg' THEN BEGIN
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
	t_curr, g_curr, g_next, gind, evoldum, snap_curr

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

		IF ncut GE 2L THEN BEGIN	;;----- For a galaxy that is not changed in its particle number over the multiple snapshots
			nochagal_snap	= g_next(cut2).snapnum
			cut_nochagal	= WHERE(nochagal_snap EQ MIN(nochagal_snap))
			cut2	= cut2(cut_nochagal)
		ENDIF

		snap_next	= g_next(cut2).snapnum

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

					store_ind	= WHERE(cut_exist EQ gind, nstore)
					p_tfrun_clearbranch, tree, evoldum, ind, gind
					treelog.n_link2 ++
					IF nstore GE 1L THEN cut_exist(store_ind) = ind
				ENDIF ELSE BEGIN
					;; Too short branch or too low merit
					;;	Clear it
					store_ind	= WHERE(cut_exist EQ gind, nstore)

					p_tfrun_clearbranch, tree, evoldum, ind, gind
					treelog.n_broken ++

					IF nstore GE 1L THEN cut_exist(store_ind) = ind
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

;;---------------------------------------------------------------------------------------
;;
;;---------------------------------------------------------------------------------------

;;-----
;; Correct Tree
;;-----
FUNCTION p_TFRun_corr_getbr, tree, gal, snap0
	bid	= LONARR(N_ELEMENTS(gal))-1L
	n_tree	= N_ELEMENTS(tree)

	FOR i=0L, n_tree-1L DO BEGIN
		tmp	= *tree(i)
		IF tmp.snap(tmp.endind) NE snap0 THEN CONTINUE
		cut	= WHERE(gal.ID EQ tmp.id(tmp.endind),ncut)
		IF ncut EQ 0L THEN CONTINUE
		bid(cut)	= i
	ENDFOR
	RETURN, bid
END
FUNCTION p_TFRun_corr_gettlength, tree, gal, bid

	tlength	= LONARR(N_ELEMENTS(gal))+1L
	FOR i=0L, N_ELEMENTS(gal)-1L DO BEGIN
		tmp	= *tree(bid(i))
		tlength(i)	= tmp.snap(tmp.endind) - tmp.snap(0) + 1
	ENDFOR
	RETURN, tlength
END

;;-----
;; FIND BRANCH BY MERGER
;;-----
FUNCTION p_TFRun_corr_findbr_bymerge, settings, tree, complete_tree, ind

	n_search	= settings.p_TFRun_corr_nsearch

	cut	= WHERE(tree.m_snap - tree.snap(0) LE n_search, ncut)
	IF ncut EQ 0L THEN RETURN, [-1L]
	RETURN, tree.m_bid(cut)
END

;;-----
;; FIND BRANCH BY PARTICLE
;;-----
FUNCTION p_tfrun_corr_findgal, settings, gal0, snap0, pid, snap

	treedir	= settings.p_tfrun_treedir
	settings.p_tfrun_treedir	= 'prg'
	snap	= p_tfrun_findnextsnap(settings,snap0)
	settings.p_tfrun_treedir	= treedir

	gal	= f_rdgal(snap, settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg, id0=-1L)

	;;----- READ INFO
	rd_info, info0, file='/storage6/NewHorizon/output_' + $
		STRING(snap0,format='(I5.5)') + '/info_' + $
		STRING(snap0,format='(I5.5)') + '.txt'

	rd_info, info, file='/storage6/NewHorizon/output_' + $
		STRING(snap,format='(I5.5)') + '/info_' + $
		STRING(snap,format='(I5.5)') + '.txt'

	;;----- GET PHYSICAL UNIT AT SNAP
	xc	= gal0.xc / info0.unit_l * info.unit_l
	yc	= gal0.yc / info0.unit_l * info.unit_l
	zc	= gal0.zc / info0.unit_l * info.unit_l

	xr	= [-1.,1.] * settings.p_tfrun_corr_boxrange + xc
	yr	= [-1.,1.] * settings.p_tfrun_corr_boxrange + yc
	zr	= [-1.,1.] * settings.p_tfrun_corr_boxrange + zc

	ind	= js_bound(gal.xc, gal.yc, gal.zc, xr=xr, yr=yr, zr=zr)

	IF MAX(ind) LT 0L THEN BEGIN
		PRINT, 'WHY NO GAL?'
		STOP
	ENDIF

	;;----- FIND GAL POSSESSING PARTICLE
	pidcheck	= pid
	FOR i=0L, N_ELEMENTS(ind)-1L DO BEGIN
		id0	= gal(ind(i)).id
		piddum	= p_TFRun_getpid(settings, id0, snap)

		FOR j=0L, N_ELEMENTS(pid)-1L DO BEGIN
			cut	= WHERE(piddum EQ pid(j), ncut)
			IF ncut EQ 0L THEN CONTINUE
			pidcheck(j)	= -1L
			ind(i)		= -1L
		ENDFOR
	ENDFOR

	;IF MAX(pidcheck) GE 0L THEN BEGIN
	;	PRINT, 'NO MATCHED PARTICLE'
	;	STOP
	;ENDIF

	cut	= WHERE(ind LT 0L, ncut)
	IF ncut EQ 0L THEN RETURN, [-1]
	ind	= ind(cut)
	RETURN, gal(ind).id
END
FUNCTION p_TFRun_corr_findbr_byidmbp, settings, tree, complete_tree

	id0	= tree.id(0)
	snap0	= tree.snap(0)
	gal0	= f_rdgal(snap0, settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg, id0=id0)

	;;-----
	;; READ THE FIRST 100 PARTICLE (BINDING ENERGY ORDER)
	;;-----
	pid	= p_TFRun_getpid(settings, id0, snap0)

	IF gal0.npart GE settings.p_tfrun_corr_npart THEN $
		pid = pid(0L:settings.p_tfrun_corr_npart-1L)

	;; FIND GAL BEFORE THE SNAPSHOT
	gid	= p_tfrun_corr_findgal(settings, gal0, snap0, pid, snap)

	;; RETURN BRANCH INDEX
	IF MAX(gid) LT 0L THEN RETURN, [-1]

	bid	= [-1L]
	n_tree	= N_ELEMENTS(complete_tree)
	FOR i=0L, N_ELEMENTS(gid)-1L DO BEGIN
		FOR j=0L, n_tree-1L DO BEGIN
			tmp	= *complete_tree(j)
			cut	= WHERE(tmp.snap EQ snap, ncut)
			IF ncut EQ 0L THEN CONTINUE
			IF tmp.id(cut) EQ gid(i) THEN BEGIN
				bid	= [bid, j]
				BREAK
			ENDIF
		ENDFOR
	ENDFOR

	RETURN, bid
END

;;-----
;; Determine Branch By merit values
;;-----
FUNCTION p_TFRun_corr_detbr, settings, tree, complete_tree, blist

	id0	= tree.id(0)
	snap0	= tree.snap(0)
	pid0	= p_TFRun_getpid(settings, id0, snap0)

	merit	= DBLARR(N_ELEMENTS(blist))

	ftr_name	= settings.dir_lib + 'sub_ftn/get_merit.so'
	larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= N_ELEMENTS(pid0)
		larr(2)	= settings.num_thread

	FOR i=0L, N_ELEMENTS(blist)-1L DO BEGIN
		tmp	= *complete_tree(blist(i))

		cut	= WHERE(tmp.snap LT snap0)
		cut	= cut(-1)

		ii	= tmp.id(cut)
		ss	= tmp.snap(cut)

		pid	= p_TFRun_getpid(settings, ii, ss)

		;; FORTRAN ROUTINE
		larr(1)	= N_ELEMENTS(pid)

		darr(0)	= 0.	;; initialize

		void	= CALL_EXTERNAL(ftr_name, 'get_merit', $
			larr, darr, pid0, pid)

		merit(i)	= darr(0)
	ENDFOR

	maxmerit	= MAX(merit)
	IF maxmerit LT 0.1 THEN BEGIN
		PRINT, 'too low merit..?'
		STOP
	ENDIF
	cut	= WHERE(merit EQ maxmerit)
	RETURN, blist(cut)
END
FUNCTION p_TFRun_corr_link, settings, tree, complete_tree, ind, remove_ind

	ind	= ind(0)
	tree_tolink	= *complete_tree(ind)
	remove_ind(ind)	= 1L
	complete_tree(ind)	= PTR_NEW({$
		ID:	-1L, $
		SNAP:	-1L, $
		STAT:	'removed', $
		P_SNAP:	-1L, $
		P_ID:	-1L, $
		P_MERIT:-1L, $
		M_ID:	-1L, $
		M_MERIT:-1L, $
		M_BID:	-1L, $
		M_SNAP:	-1L, $
		D_ID:	-1L, $
		D_SNAP:	-1L, $
		endind:	-1L, $
		numprog:-1L}, /no_copy)

	snap0	= tree.snap(0)
	cut	= WHERE(tree_tolink.snap LT snap0, ncut)

	;;----- ERROR LOG
	IF ncut EQ 0L THEN BEGIN
		RETURN, tree
	ENDIF

	cut	= cut(-1)
	tree2	= {$
		ID:[		tree_tolink.ID(0:cut), 		tree.ID], $
		SNAP:[		tree_tolink.snap(0:cut),	tree.snap], $
		STAT:'main', $
		P_SNAP:[	tree_tolink.p_snap(0:cut), 	tree.p_snap], $
		P_ID:[		tree_tolink.p_id(0:cut), 	tree.p_id], $
		P_MERIT:[	tree_tolink.p_merit(0:cut),	tree.p_merit], $
		M_ID:[		tree_tolink.M_ID,	tree.M_ID], $
		M_MERIT:[	tree_tolink.M_MERIT,	tree.M_MERIT], $
		M_BID:[		tree_tolink.M_BID, 	tree.M_BID], $
		M_SNAP:[	tree_tolink.M_SNAP,	tree.M_SNAP], $
		D_ID:[		tree_tolink.D_ID(0:cut),	tree.D_ID], $
		D_SNAP:[	tree_tolink.D_SNAP(0:cut),	tree.D_SNAP], $
		endind:		cut + tree.endind + 1, $
		numprog:	tree_tolink.numprog + tree.numprog - 1L}

	;cut	= cut(0)
	;tree2	= {$
	;	ID:[		tree_tolink.ID, 	tree.ID(cut:tree.endind)], $
	;	SNAP:[		tree_tolink.snap, 	tree.snap(cut:tree.endind)], $
	;	STAT:'main', $
	;	P_SNAP:[	tree_tolink.p_snap, 	tree.p_snap(cut:tree.endind)], $
	;	P_ID:[		tree_tolink.p_id, 	tree.p_id(cut:tree.endind)], $
	;	P_MERIT:[	tree_tolink.p_merit,	tree.p_merit(cut:tree.endind)], $
	;	M_ID:[		tree_tolink.M_ID,	tree.M_ID], $
	;	M_MERIT:[	tree_tolink.M_MERIT,	tree.M_MERIT], $
	;	M_BID:[		tree_tolink.M_BID, 	tree.M_BID], $
	;	M_SNAP:[	tree_tolink.M_SNAP,	tree.M_SNAP], $
	;	D_ID:[		tree_tolink.D_ID,	tree.D_ID(cut:tree.endind)], $
	;	D_SNAP:[	tree_tolink.D_SNAP,	tree.D_SNAP(cut:tree.endind)], $
	;	endind:		tree_tolink.endind + tree.endind - cut + 1, $
	;	numprog:	tree_tolink.numprog + tree.numprog - 1L}

	RETURN, tree2
END
PRO P_TFRun_corr_sanitycheck, settings, tree, n0, id0

	evol	= f_getevol(tree, tree.id(-1), tree.snap(-1), settings.column_list, $
		horg=settings.horg, dir=settings.dir_catalog)

	cut	= WHERE(evol.snapnum EQ n0)

	n_pix	= 1000L
	img	= BYTARR(n_pix,n_pix)
	img	= REPLICATE({img:img},5)
	
	FOR i=0, 4L DO BEGIN
		img(i).img	= draw_gal($
			evol(cut-2L+i).id, evol(cut-2L+i).snapnum, $
			num_thread=settings.num_thread, dir_raw=settings.dir_raw, $
			dir_catalog=settings.dir_catalog, /raw, boxrange=20., $
			n_pix=n_pix, min=1e2, max=1e8)
	ENDFOR

	cgDisplay, 900, 600

	cgImage, img(0).img, position=[0., 0.5, 1./3, 1.], /noerase
	cgImage, img(1).img, position=[1./3, 0.5, 2./3, 1.], /noerase
	cgImage, img(2).img, position=[2./3, 0.5, 1., 1.], /noerase
	cgImage, img(3).img, position=[0., 0., 1./3, 0.5], /noerase
	cgImage, img(4).img, position=[1./3, 0., 2./3, 0.5], /noerase

	;;----- MASS EVOL
	cgPlot, 0, 0, /nodata, /noerase, position=[0.7, 0.1, 0.95, 0.48], $
		xrange=[settings.P_TFRun_corr_snap_i, settings.P_TFRun_corr_snap_f], $
		yrange=[1e6, 1e12], /ylog
	cgOplot, evol.snapnum, evol.mass_tot, linestyle=0, thick=2
	cgOplot, evol(cut-2L:cut+2L).snapnum, evol(cut-2L:cut+2L).mass_tot, $
		linestyle=0, thick=2, color='red'

	STOP

END

PRO p_TFRun_corr, settings, complete_tree 

	snap0	= settings.p_TFRun_corr_snap_f

	remove_ind	= LONARR(N_ELEMENTS(complete_tree))-1L
	;;----- LOAD GAL & BRANCH
	gal	= f_rdgal(snap0, settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg, id0=-1L)
	bid	= p_TFRun_corr_getbr(complete_tree, gal, snap0)

	corr_idlist	= LONARR(N_ELEMENTS(gal))-1L

	;;----- CONNECT
	FOR i=0L, N_ELEMENTS(gal)-1L DO BEGIN
		IF bid(i) LT 0L THEN CONTINUE

		tree	= *complete_tree(bid(i))
		tree0	= tree

		IF tree.snap(0) LE settings.P_TFrun_corr_snap_i THEN CONTINUE
		ind	= bid(i)
		;;---- SEARCH LINKED TREE
		REPEAT BEGIN
			n0	= tree.snap(0)
			id0	= tree.id(0)

			blist1	= p_TFrun_corr_findbr_bymerge(settings, tree, complete_tree, ind)
			blist2	= p_TFrun_corr_findbr_byidmbp(settings, tree, complete_tree)

			blist	= [blist1, blist2]
			IF MAX(blist) LT 0L THEN BEGIN
				n1	= n0
			ENDIF ELSE BEGIN
				cut	= WHERE(blist GE 0L)
				blist	= blist(cut)
				cut	= SORT(blist)
				blist	= blist(cut)
				blist	= blist(UNIQ(blist))

				ind	= p_TFRun_corr_detbr(settings, tree, complete_tree, blist)
			ENDELSE
			;;
			tree	= p_TFRun_corr_link(settings, tree, complete_tree, ind, remove_ind)

			IF settings.P_TFRun_corr_log EQ 1L THEN $
				P_TFRun_corr_sanitycheck, settings, tree, n0

			n1	= tree.snap(0)
		ENDREP UNTIL tree.snap(0) LE settings.P_TFrun_corr_snap_i OR n0 EQ n1

		complete_tree(bid(i))	= PTR_NEW(tree, /no_copy)
		corr_idlist(i)	= gal(i).ID
	ENDFOR
	cut	= WHERE(corr_idlist GE 0L, ncut)
	IF ncut GE 1L THEN corr_idlist = corr_idlist(cut)

	SAVE, filename=settings.dir_tree + 'ctree.sav', complete_tree, corr_idlist
	;; REMOVE IND
	STOP

END

;;-----
;; Main Part
;;-----
PRO p_tfrun, settings

IF settings.p_tfrun_makebr EQ 1L THEN BEGIN
	;;-----
	;; Basic settings
	;;-----
	treeset	= p_tfrun_set(settings)
	maxgind	= 100000L
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
	tree	= REPLICATE(dumstr, maxgind)
	complete_tree	= PTRARR(100000L)

	evoldum	= {ID:LONARR(maxgind)-1L, snap:LONARR(maxgind)-1L, merit:DBLARR(maxgind)-1.d}

	;;-----
	;; GO FORWARD
	;;-----

	gind	= -1L
	n_comp	= 0L
	treelog	= {n_new:0L, n_link:0L, n_link2:0L, n_link3:0L, n_broken:0L, n_all:0L}
	FOR i=treeset.N0, treeset.N1, treeset.DN DO BEGIN
		;IF i LE 900L THEN CONTINUE
		;IF i EQ 901L THEN RESTORE, '~/treetmp.sav'
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
		IF t_curr.nlink GE 2L THEN BEGIN
			FOR i2=0L, t_curr.nlink-2L DO BEGIN
				snap_next	= p_tfrun_findnextsnap(settings, snap_next)
				IF snap_next EQ -1L THEN CONTINUE
				g_dum	= f_rdgal(snap_next, ['ID', 'npart'], id0=-1, $
					dir=settings.dir_save, horg=settings.horg)
				g_next	= [g_next, g_dum]
			ENDFOR
		ENDIF

		;;-----
		;; MATCHING
		;;-----
		p_tfrun_match, settings, treelog, tree, complete_tree, n_comp, $
			t_curr, g_curr, g_next, gind, evoldum, snap_curr

		;;-----
		;; REMOVE FROM BRANCH
		;;-----
		p_tfrun_remove, settings, evoldum, tree, gind, complete_tree, n_comp, $
			snap_curr, t_curr.nlink

		;;-----
		;; For the multi-snapshot search
		;;-----
		;IF t_curr.nlink GE 2L THEN BEGIN
		;	FOR i2=0L, t_curr.nlink-2L DO BEGIN
		;		snap_next	= p_tfrun_findnextsnap(settings,snap_next)
		;		IF snap_next EQ -1L THEN CONTINUE
		;		g_next		= f_rdgal(snap_next, ['ID', 'npart'], id0=-1, $
		;			dir=settings.dir_save, horg=settings.horg)
		;		p_tfrun_match, settings, treelog, tree, complete_tree, n_comp, $
		;			t_curr, g_curr, g_next, gind, evoldum, snap_curr, snap_next
		;	ENDFOR
		;ENDIF

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

		IF i MOD 100 EQ 0L THEN BEGIN

			SAVE, filename=settings.dir_tree + 'treetmp_' + $
				STRING(i,format='(I4.4)') + '.sav', treelog, tree, complete_tree, n_comp, gind, evoldum
		ENDIF

	ENDFOR

	SAVE, filename=settings.dir_tree + 'tree.sav', complete_tree
	STOP
ENDIF

IF settings.p_tfrun_corr EQ 1L THEN BEGIN
	RESTORE, settings.dir_tree + 'tree.sav'

	IF settings.p_TFRun_corr EQ 1L THEN $
		p_TFRun_corr, settings, complete_tree

	PRINT, 'Mergered branch mutual link'
	PRINT, 'finish branch modulate m_ID'
	STOP
ENDIF
END
