

;;-----
;; UTILITIES
;;-----

;; GET PARTICLE LIST
FUNCTION p_tfrun_getpid, settings, id0, snap0

	fname	= settings.dir_catalog
	IF settings.horg EQ 'g' THEN fname = fname + 'Galaxy/VR_Galaxy/snap_'
	IF settings.horg EQ 'h' THEN fname = fname + 'Halo/VR_Halo/snap_'
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
FUNCTION p_TFRun_corr_getbr, tree, tree_key, gal, snap0
	bid	= LONARR(N_ELEMENTS(gal))-1L
	n_tree	= N_ELEMENTS(tree)

	keyval	= snap0 + gal.id * tree_key(0)
	bid 	= tree_key(keyval)
	;FOR i=0L, n_tree-1L DO BEGIN
	;	tmp	= *tree(i)
	;	IF tmp.snap(tmp.endind) NE snap0 THEN CONTINUE
	;	cut	= WHERE(gal.ID EQ tmp.id(tmp.endind),ncut)
	;	IF ncut EQ 0L THEN CONTINUE
	;	bid(cut)	= i
	;ENDFOR
	RETURN, bid
END
FUNCTION p_TFRun_corr_gettree, id0, snap0, tree, tree_key

	keyval 	= snap0 + tree_key(0)*id0
	RETURN, tree_key(keyval)

	;n_tree	= N_ELEMENTS(tree)
;
;	;FOR i=0L, n_tree-1L DO BEGIN
;	;	tmp	= *tree(i)
;	;	cut	= WHERE(tmp.snap EQ snap0, ncut)
;	;	IF ncut EQ 0L THEN CONTINUE
;	;	IF tmp.ID(cut) EQ id0 THEN RETURN, i
;	;ENDFOR
	;RETURN, -1L
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
FUNCTION p_tfrun_corr_findgal, settings, gal0, snap0, pid, snap, NSNAP=NSNAP

	treedir	= settings.p_tfrun_treedir
	settings.p_tfrun_treedir	= 'prg'

	snap	= snap0
	FOR i=0L, NSNAP-1L DO BEGIN
		snap	= p_tfrun_findnextsnap(settings,snap)
		IF snap EQ -1L THEN RETURN, [-1]
	ENDFOR
	settings.p_tfrun_treedir	= treedir

	gal	= f_rdgal(snap, -1L, column_list=settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg)

	;;----- READ INFO
	rd_info, info0, file=settings.dir_raw + 'output_' + $
		STRING(snap0,format='(I5.5)') + '/info_' + $
		STRING(snap0,format='(I5.5)') + '.txt'

	rd_info, info, file=settings.dir_raw + 'output_' + $
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
		findgal_reset:
		xr	+= [-100., 100.]
		yr	+= [-100., 100.]
		zr	+= [-100., 100.]

		ind	= js_bound(gal.xc, gal.yc, gal.zc, xr=xr, yr=yr, zr=zr)

		IF MAX(ind) GE 0L THEN GOTO, findgal_skip
		IF MAX(ind) LT 0L THEN GOTO, findgal_reset
	ENDIF
	findgal_skip:

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
	gal0	= f_rdgal(snap0, id0, column_list=settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg)

	;;-----
	;; READ THE FIRST 100 PARTICLE (BINDING ENERGY ORDER)
	;;-----
	pid	= p_TFRun_getpid(settings, id0, snap0)

	IF gal0.npart GE settings.p_tfrun_corr_npart THEN $
		pid = pid(0L:settings.p_tfrun_corr_npart-1L)

	;; FIND GAL BEFORE THE SNAPSHOT
	gid1	= p_tfrun_corr_findgal(settings, gal0, snap0, pid, snap, NSNAP=1)
	snap1	= gid1*0L + snap

	;; FIND GAL BEFORE THE 10 SNAPSHOTS
	gid2	= p_tfrun_corr_findgal(settings, gal0, snap0, pid, snap, NSNAP=10)
	snap2	= gid2*0L + snap

	;; FIND GAL BEFORE THE 30 SNAPSHOTS
	gid3	= p_tfrun_corr_findgal(settings, gal0, snap0, pid, snap, NSNAP=30)
	snap3	= gid3*0L + snap

	gid	= [gid1, gid2, gid3]
	snap	= [snap1, snap2, snap3]
	;; RETURN BRANCH INDEX
	IF MAX(gid) LT 0L THEN RETURN, [-1]

	bid	= [-1L]
	n_tree	= N_ELEMENTS(complete_tree)

	FOR i=0L, N_ELEMENTS(gid)-1L DO BEGIN
		FOR j=0L, n_tree-1L DO BEGIN
			tmp	= *complete_tree(j)
			cut	= WHERE(tmp.snap EQ snap(i), ncut)
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

	ind0	= 20<tree.endind
	id0	= tree.id(ind0)
	snap0	= tree.snap(ind0)
	pid0	= p_TFRun_getpid(settings, id0, snap0)

	merit	= DBLARR(N_ELEMENTS(blist))

	ftr_name	= settings.dir_lib + 'sub_ftn/get_merit.so'
	larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= N_ELEMENTS(pid0)
		larr(2)	= settings.num_thread

	FOR i=0L, N_ELEMENTS(blist)-1L DO BEGIN
		tmp	= *complete_tree(blist(i))

		IF tmp.stat EQ 'removed' THEN CONTINUE

		cut	= WHERE(tmp.snap LT snap0, ncut)

		cut	= [cut((ncut-20)>0), cut((ncut-10)>0), cut(ncut-1)]
		meritdum= DBLARR(3)
		FOR j=0L, 2L DO BEGIN
			ii	= tmp.id(cut(j))
			ss	= tmp.snap(cut(j))

			pid	= p_TFRun_getpid(settings, ii, ss)

			;; FORTRAN ROUTINE
			larr(1)	= N_ELEMENTS(pid)

			darr(0)	= 0.	;; initialize

			void	= CALL_EXTERNAL(ftr_name, 'get_merit', $
				larr, darr, pid0, pid)
			meritdum(j) = darr(0)
			PRINT, i, meritdum(j), ii, ss
		ENDFOR

		merit(i)	= MAX(meritdum)
	ENDFOR

	maxmerit	= MAX(merit)
	IF maxmerit LT 0.1 THEN BEGIN
		PRINT, 'too low merit..?'
		PRINT, '	:', id0, ' / ',  snap0
		STOP
	ENDIF
	cut	= WHERE(merit EQ maxmerit)
	RETURN, blist(cut)
END

;;-----
;; Determine Branch By directly computing merits
;;-----

;;----- Determine which snapshot will be used for this galaxy
PRO p_TFRun_findbr_bymerit_selectsnap_scheck, evol, avg, std, cut, sfact

	v1	= 10.^(avg - sfact*std)
	v2	= 10.^(avg + sfact*std)
	cgDisplay, 800, 800
	cgPlot, evol.snapnum, evol.mass_tot, linestyle=0, thick=2, /ylog
	cgOplot, [evol(0).snapnum, evol(-1).snapnum], [v1, v1], linestyle=2, thick=2, color='red'
	cgOplot, [evol(0).snapnum, evol(-1).snapnum], [v2, v2], linestyle=2, thick=2, color='red'
	cgOplot, evol(cut(0)).snapnum, evol(cut(0)).mass_tot, psym=16, color='red'
END
FUNCTION p_TFRun_findbr_bymerit_selectsnap, settings, settings_corr, tree, quantity
	sanitycheck	= settings.p_TFRun_corr_masslog
	evol	= f_getevol(tree, tree.snap(-1), tree.id(-1), datalist=settings.column_list, $
		horg=settings.horg, dir=settings.dir_catalog)

	mass	= ALOG10(evol.mass_tot)
	snap	= evol.snapnum

	n_end 	= settings_corr.nlast < (N_ELEMENTS(mass)-1L)
	;;
	delM0	= 1d15
	FOR i=0L, n_end-1L DO BEGIN
		ind0	= i
		ind1 	= (i+settings_corr.nstep) < (N_ELEMENTS(mass)-1L)
		delM	= MAX(mass(ind0:ind1)) - MIN(mass(ind0:ind1))
		IF delM LT delm0 THEN BEGIN
			delm0 	= delM
			cut 	= i
		ENDIF
	ENDFOR
	;delM	= mass(1L:*) - mass(0L:-2)
	;delM	= delM(0L:n_end-2L)
	;cut	= WHERE(delM EQ MIN(delM)) + 1L
	;cut 	= cut(0)

	;;
	mass_avg	= MEAN(mass(0L:n_end))
	mass_std	= STDDEV(mass(0L:n_end))

	;cut 	= WHERE(ABS(mass-mass_avg) LT settings_corr.sfact * mass_std, ncut)
	;cut	= WHERE(mass GT mass_avg - settings.sfact * mass_std AND $
	;	mass LT mass_avg + settings.sfact * mass_std, ncut)
	;IF ncut EQ 0L THEN BEGIN
	;	PRINT, 'no snapshot exists for this mass range'
	;	STOP
	;ENDIF

	IF sanitycheck EQ 1L THEN BEGIN
		PRINT, 'Sanity Check whether this range is proper'
		p_TFRun_findbr_bymerit_selectsnap_scheck, evol, mass_avg, mass_std, cut, settings_corr.sfact
		STOP
	ENDIF

	;cut2	= WHERE(tree.snap(cut) - tree.snap(0) GT 20.)
	;cut	= cut(cut2(0))
	;cut 	= cut(0)

	;; OUTPUT
	snap0	= evol(cut).snapnum
	rd_info, info0, file=settings.dir_raw + 'output_' + $
		STRING(snap0,format='(I5.5)') + '/info_' + $
		STRING(snap0,format='(I5.5)') + '.txt'

	mass0	= evol(cut).mass_tot
	pos0	= [evol(cut).xc, evol(cut).yc, evol(cut).zc] / info0.unit_l * 3.086d21
	speed0	= SQRT(evol(cut).vxc^2 + evol(cut).vyc^2 + evol(cut).vzc^2) * 1.023
	speed0	= 500. * 1.023; set to 500km/s
	quantity	= {mass:mass0, pos:pos0, speed:speed0, time:info0.tGyr, unit_l:info0.unit_l}
	RETURN, cut
END
;;----- LINK GALAXY WITH NO TREE
PRO p_TFRun_findbr_bymerit_mergenotreegal, tree, id, snap
	length 	= N_ELEMENTS(tree.id)
	nbranch = N_ELEMENTS(tree.m_id)
	tree 	= {$
		ID:[id,tree.id], $
		SNAP:[snap,tree.snap], $
		STAT:tree.stat, $
		P_SNAP:[-1L, tree.p_snap], $
		P_ID:[-1L, tree.p_id], $
		P_MERIT:[-1.d, tree.p_merit], $
		M_ID:tree.m_id, $
		M_MERIT:tree.m_merit, $
		M_BID:tree.m_bid, $
		M_SNAP:tree.m_snap, $
		D_ID:[tree.id(0), tree.d_id], $
		D_SNAP:[tree.snap(0), tree.d_snap], $
		ENDIND:tree.endind+1L, $
		NUMPROG:tree.numprog}
END
;; FINDBR MAIN
FUNCTION p_TFRun_findbr_bymerit, settings, settings_corr, tree, complete_tree, tree_key

	nsearch	= settings_corr.nsearch		;; # of snapsts to search

	;;----- First Pick the snapshot where to stitch
	;mass0	= 0.
	ind	= p_TFRun_findbr_bymerit_selectsnap(settings, settings_corr, tree, quantity)

	;;----- GET Partile IDs AT THIS SNAPSHOT
	id0	= tree.id(ind)
	snap0	= tree.snap(ind)
	pid0	= p_TFRun_getpid(settings, id0, snap0)

	;;----- FIND GAL with merit scores

	;; Fortran routine settings
	ftr_name	= settings.dir_lib + 'sub_ftn/get_merit.so'
	larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= N_ELEMENTS(pid0)
		larr(2)	= settings.num_thread


	treedir	= settings.p_tfrun_treedir
	settings.p_tfrun_treedir	= 'prg'

	merit	= DBLARR(nsearch)
	snaplist= LONARR(nsearch)
	idlist	= LONARR(nsearch)

	snap0	= tree.snap(0)

	;; Find a galaxy with the highest merit at each snapshot
	FOR i=0L, nsearch-1L DO BEGIN
		IF settings.P_TFrun_corr_timelog EQ 1L THEN TIC
		;;----- FIND SNAPSHOT
		snap	= snap0
		FOR j=0L, settings_corr.nstep*(i+1L)-1L DO BEGIN
			snap	= p_tfrun_findnextsnap(settings, snap)
			IF snap EQ -1L THEN BREAK
		ENDFOR
		IF snap EQ -1L THEN CONTINUE

		;;----- READ GAL AT THIS SNAPSHOT
		gal	= f_rdgal(snap, -1L, column_list=['ID', 'Mass_tot', 'Xc', 'Yc', 'Zc'], $
			dir=settings.dir_catalog, $
			horg=settings.horg)

		;;---- READ INFO
		rd_info, info0, file=settings.dir_raw + '/output_' + $
			STRING(snap,format='(I5.5)') + '/info_' + $
			STRING(snap,format='(I5.5)') + '.txt'

		gal.xc	*= (3.086d21/info0.unit_l)
		gal.yc	*= (3.086d21/info0.unit_l)
		gal.zc	*= (3.086d21/info0.unit_l)

		;rr0	= quantity.speed *3.0* (quantity.time - info0.tgyr) * 3.086d21 / quantity.unit_l
		;;----- COMPUTE MERIT
		ngal	= N_ELEMENTS(gal)
		mer_dum	= DBLARR(ngal)

		;; FIND THE FIRST 10% GALAXIES AROUND based on the code unit

		d3d	= (gal.xc - quantity.pos(0))^2 + $
			(gal.yc - quantity.pos(1))^2 + $
			(gal.zc - quantity.pos(2))^2
		d3d_s	= d3d(SORT(d3d))
		;dcut	= d3d_s(50<(ngal-1L))
		dcut	= d3d_s( LONG(ngal/10) )

		FOR j=0L, ngal-1L DO BEGIN

			;; SKIP CRITERIA
			;;----- BY MASS?
			IF ABS(ALOG10(quantity.mass) - ALOG10(gal(j).Mass_tot)) GT 2. THEN CONTINUE

			;;----- BY TIME * SPEED
			;;d3d	= (gal(j).xc - quantity.pos(0))^2 + $
			;;	(gal(j).yc - quantity.pos(1))^2 + $
			;;	(gal(j).zc - quantity.pos(2))^2
			;;d3d	= SQRT(d3d)

			;;IF d3d / rr0 GT 5. THEN CONTINUE

			;;----- BY THE FIRST 100th DISTANCE
			IF d3d(j) GT dcut THEN CONTINUE

			;;----- SKIP THIS GALAXY IF HAS A FULL TREE 123123123123
			tind	= p_TFRun_corr_gettree(gal(j).id, snap, complete_tree, tree_key)
			IF tind GE 0L THEN $
				IF (*complete_tree(tind)).snap(-1) EQ $
				settings.p_TFrun_corr_snap_f THEN CONTINUE

			;;----- COMPUTE MERIT
			pid	= p_TFRun_getpid(settings, gal(j).id, snap)

			;; FORTRAN ROUTINE
			larr(1)	= N_ELEMENTS(pid)
			darr(0)	= 0.	;; initialize

			void	= CALL_EXTERNAL(ftr_name, 'get_merit', $
				larr, darr, pid0, pid)
			mer_dum(j) = darr(0)

			IF settings.p_TFRun_corr_log EQ 1L THEN $
				PRINT, snap, ' : ', gal(j).id, ' / ', ngal, ' / ', $
					gal(j).Mass_tot, ' ( ', mer_dum(j), ' )'
		ENDFOR

		;;----- DETERMINE GAL BY MERIT
		cut	= WHERE(mer_dum EQ MAX(mer_dum) AND mer_dum GT 0., ncut)
		IF ncut EQ 0L THEN BEGIN
			mer_val	= 0.
			id_val	= -1L
		ENDIF ELSE IF ncut GE 2L THEN BEGIN
			mer_val = mer_dum(cut(0))
			id_val	= gal(cut(0)).id
		ENDIF ELSE BEGIN
			mer_val	= mer_dum(cut)
			id_val	= gal(cut).id
		ENDELSE
		merit(i)	= mer_val
		snaplist(i)	= snap
		idlist(i)	= id_val

		;; If this merit is high enough, end this loop
		IF merit(i) GT settings_corr.meritU THEN BREAK

		IF settings.P_TFrun_corr_timelog EQ 1L THEN BEGIN
			TOC, elapsed_time=elt
			PRINT, 'time : ', i, ' / ', nsearch
			PRINT, '	', snap0, ' -> ', snap
			PRINT, '	', elt, ' [s]'
			PRINT, '	', merit(i)
		ENDIF
	ENDFOR

	settings.p_tfrun_treedir	= treedir

	ind	= REVERSE(SORT(merit))
	merit	= merit(ind)
	id	= idlist(ind)
	snap	= snaplist(ind)

	IF merit(0)/(merit(1)+1e-32) LT 2. THEN BEGIN	;; IF THE PRIMARY PROGENITOR HAS NOT ENOUGH MERIT SCORE
		cut1	= WHERE(MERIT GT MERIT(0)/2.)
		cut2 	= WHERE(MERIT LT MERIT(0)/2., ncut2)

		merit2	= merit(cut1)
		id2 	= id(cut1)
		snap2 	= snap(cut1)

		sortind = REVERSE(SORT(snap2))
		merit2 	= merit2(sortind)
		id2 	= id2(sortind)
		snap2	= snap2(sortind)
		IF ncut2 GE 1L THEN BEGIN
			merit 	= [merit2, merit(cut2)]
			id 		= [id2, id(cut2)]
			snap 	= [snap2, snap(cut2)]
		ENDIF
	ENDIF

	FOR i=0L, N_ELEMENTS(ind)-1L DO BEGIN
		keyval 	= snap(i) + tree_key(0)*id(i)
		keyval	= tree_key(keyval)
		IF keyval LT 0L THEN BEGIN
			IF merit(i) GT settings_corr.meritL THEN BEGIN
				p_TFRun_findbr_bymerit_mergenotreegal, tree, id(i), snap(i)
				PRINT, '%123123-----'
				PRINT, '	CONNECTION DONE BY DIRECTLY COMPUTING MERIT SCORES (to a galaxy w/o tree)'
				PRINT, '	ID = ' + STRING(tree.id(1), format='(I6.6)') + ' ---> ' + $
					' ID = ' + STRING(id(i), format='(I6.6)')
				PRINT, '	SS = ' + STRING(tree.snap(1), format='(I4.4)') + '      ' + $
					' SS = ' + STRING(snap(i), format='(I4.4)')
				PRINT, '	MERIT SCORE = ', merit(i)
				PRINT, '%123123-----'

				tree_key(keyval)	= tree_key(tree.snap(1) + tree.id(1)*tree_key(0))
				RETURN, -2
			ENDIF
			CONTINUE
		ENDIF

		IF merit(i) LT settings_corr.meritL THEN BEGIN
			PRINT, '%123123-----'
			PRINT, '	STOP MERGING DUE TO TOO LOW MERIT'
			PRINT, '		merit = ', merit(i)
			PRINT, '%123123-----'
			RETURN, -1
		ENDIF ELSE BEGIN
			PRINT, '%123123-----'
			PRINT, '	CONNECTION DONE BY DIRECTLY COMPUTING MERIT SCORES'
			PRINT, '	ID = ' + STRING(tree.id(0), format='(I6.6)') + ' ---> ' + $
				' ID = ' + STRING(id(i), format='(I6.6)')
			PRINT, '	SS = ' + STRING(tree.snap(0), format='(I4.4)') + '      ' + $
				' SS = ' + STRING(snap(i), format='(I4.4)')
			PRINT, '	MERIT SCORE = ', merit(i)
			PRINT, '%123123-----'
			RETURN, keyval
		ENDELSE
	ENDFOR
	RETURN, -1



	;FOR j=0L, nsearch-1L DO BEGIN
	;	FOR i=0L, N_ELEMENTS(complete_tree)-1L DO BEGIN
	;		tmp	= *complete_tree(i)
	;		cut	= WHERE(tmp.snap EQ snap(j), ncut)
	;		IF ncut EQ 0L THEN CONTINUE
	;		IF tmp.id(cut) EQ id(j) THEN BEGIN
	;			PRINT, '%123123-----'
	;			PRINT, '	CONNECTION DONE BY DIRECTLY COMPUTING MERIT SCORES'
	;			PRINT, '	ID = ' + STRING(tree.id(0), format='(I6.6)') + ' ---> ' + $
	;				' ID = ' + STRING(id(j), format='(I6.6)')
	;			PRINT, '	SS = ' + STRING(tree.snap(0), format='(I4.4)') + '      ' + $
	;				' SS = ' + STRING(snap(j), format='(I4.4)')
	;			PRINT, '	MERIT SCORE = ', merit(j)
	;			PRINT, '%123123-----'
	;			RETURN, i
	;		ENDIF
	;	ENDFOR
	;ENDFOR
	;RETURN, -1L
END

;;----- LINK
FUNCTION p_TFRun_corr_link, settings, tree, complete_tree, tree_key, bid, ind, remove_ind

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

	keyval	= tree_key(0)
	tree_key(WHERE(tree_key EQ ind)) = bid
	tree_key(0)	= keyval

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

	evol	= f_getevol(tree, tree.snap(-1), tree.id(-1), datalist=settings.column_list, $
		horg=settings.horg, dir=settings.dir_catalog)

	cut	= WHERE(evol.snapnum EQ n0)

	n_pix	= 1000L
	img	= BYTARR(n_pix,n_pix)
	;img	= REPLICATE({img:img},5)

	xr	= DBLARR(2,5)
	yr	= DBLARR(2,5)
	zr	= DBLARR(2,5)
	pos	= DBLARR(4,5)
		pos(*,0)	= [0., 0.5, 1./3, 1.]
		pos(*,1)	= [1./3, 0.5, 2./3, 1.]
		pos(*,2)	= [2./3, 0.5, 1., 1.]
		pos(*,3)	= [0., 0., 1./3, 0.5]
		pos(*,4)	= [1./3, 0., 2./3, 0.5]

	ang	= FINDGEN(100)/99*!pi*2.
	cgDisplay, 600, 400
	FOR i=0, 4L DO BEGIN
		IF cut-2L+i LT 0L THEN CONTINUE
		img	= draw_gal($
			evol(cut-2L+i).snapnum, evol(cut-2L+i).id, $
			num_thread=settings.num_thread, dir_raw=settings.dir_raw, $
			dir_catalog=settings.dir_catalog, /raw, boxrange=20., $
			n_pix=n_pix, min=1e1, max=1e5, proj='xy')
		xr(*,i)	= [-1., 1.]*20. + evol(cut-2L+i).xc
		yr(*,i)	= [-1., 1.]*20. + evol(cut-2L+i).yc
		zr(*,i)	= [-1., 1.]*20. + evol(cut-2L+i).zc
		cgImage, img, position=pos(*,i), /noerase
		cgPlot, 0, 0, /noerase, position=pos(*,i), xrange=xr(*,i), $
			yrange=yr(*,i), xstyle=4, ystyle=4
		cgOplot, COS(ang)*evol(cut-2L+i).r_halfmass + evol(cut-2L+i).xc, $
			SIN(ang)*evol(cut-2L+i).r_halfmass + evol(cut-2L+i).yc, $
		       linestyle=2, color='red', thick=2

	        gal0	= f_rdgal(evol(cut-2L+i).snapnum, -1L, column_list=settings.column_list, $
			dir=settings.dir_catalog, horg=settings.horg)
		ind	= js_bound(gal0.xc, gal0.yc, gal0.zc, $
			xr=xr(*,i), yr=yr(*,i), zr=zr(*,i))
		cgOplot, gal0(ind).xc, gal0(ind).yc, psym=9, color='blue', thick=1, symsize=2.0

	ENDFOR


	;cgImage, img(0).img, position=[0., 0.5, 1./3, 1.], /noerase
	;cgImage, img(1).img, position=[1./3, 0.5, 2./3, 1.], /noerase
	;cgImage, img(2).img, position=[2./3, 0.5, 1., 1.], /noerase
	;cgImage, img(3).img, position=[0., 0., 1./3, 0.5], /noerase
	;cgImage, img(4).img, position=[1./3, 0., 2./3, 0.5], /noerase

	;;----- MASS EVOL
	cgPlot, 0, 0, /nodata, /noerase, position=[0.7, 0.1, 0.95, 0.48], $
		xrange=[settings.P_TFRun_corr_snap_i, settings.P_TFRun_corr_snap_f], $
		yrange=[1e6, 1e12], /ylog
	cgOplot, evol.snapnum, evol.mass_tot, linestyle=0, thick=2
	cgOplot, evol((cut-2L)>0:cut+2L).snapnum, evol((cut-2L)>0:cut+2L).mass_tot, $
		linestyle=0, thick=2, color='red'


END

PRO p_TFRun_corr, settings, complete_tree, tree_key

	;;----- Settings for Tree correction
	settings_corr	= {$
		nsearch:settings.P_TFrun_corr_msearch_nsearch, $		;; # of snapshots to search further
		nstep:settings.P_TFrun_corr_msearch_nstep, $			;; Snapshot interval for each search
		meritU:settings.P_TFrun_corr_msearch_meritU, $			;; Merit cut for Seacrhing
		meritL:settings.P_TFrun_corr_msearch_meritL, $			;; Merit cut for Searching
		sfact:1.d, $		;; Sigma range when finding the starting snapshot
		nlast:settings.P_TFrun_corr_msearch_nlast, $		;; # of snapshots when computing mean/std mass to find starting snapshot
		a:'a'}

	snap0	= settings.p_TFRun_corr_snap_f

	remove_ind	= LONARR(N_ELEMENTS(complete_tree))-1L
	;;----- LOAD GAL & BRANCH
	gal	= f_rdgal(snap0, -1L, column_list=settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg)
	;gal	= f_rdgalquick(snap0, dir=settings.dir_catalog, horg=settings.horg)
	bid	= p_TFRun_corr_getbr(complete_tree, tree_key, gal, snap0)
	corr_idlist	= LONARR(N_ELEMENTS(gal))-1L

	;;----- CONNECT
	
	FOR i=0L, N_ELEMENTS(gal)-1L DO BEGIN
		IF i LT settings.p_TFrun_corr_nn * 10L THEN BEGIN
			CONTINUE
		ENDIF ELSE IF i EQ settings.p_TFrun_corr_nn * 10L THEN BEGIN
			IF settings.p_TFrun_corr_nn NE 0L THEN BEGIN
				RESTORE, settings.dir_tree + 'ctree_' + STRING(i,format='(I4.4)') + '.sav'
			ENDIF
		ENDIF ELSE IF i EQ settings.p_TFrun_corr_nn * 10L + 10L THEN BEGIN
			SAVE, filename=settings.dir_tree + 'ctree_' + STRING(i,format='(I4.4)') + '.sav', $
			complete_tree, corr_idlist, tree_key
			STOP
		ENDIF

;IF gal(i).ID NE 133L THEN CONTINUE ;;%456456

		;IF gal(i).ID LE 232L THEN CONTINUE
		IF bid(i) LT 0L THEN CONTINUE
		tree	= *complete_tree(bid(i))
		tree0	= tree
		IF tree.snap(0) LE settings.P_TFrun_corr_snap_i THEN CONTINUE

		PRINT, '%123123	-----'
		PRINT, '	Tree Seacrhing for galaxy #ID = ', gal(i).ID, tree.snap(0)

		ind	= bid(i)

		;;---- SEARCH LINKED TREE
		REPEAT BEGIN
			;;----- BY DIRECTLY COMPARING MERIT SCORES
			n0	= tree.snap(0)

			blist	= p_TFRun_findbr_bymerit(settings, settings_corr, tree, complete_tree, tree_key)

			;n0	= tree.snap(0)
			;id0	= tree.id(0)

			;blist1	= p_TFrun_corr_findbr_bymerge(settings, tree, complete_tree, ind)
			;blist2	= p_TFrun_corr_findbr_byidmbp(settings, tree, complete_tree)

			;blist	= [blist1, blist2]
			;IF MAX(blist) LT 0L THEN BEGIN
			;	n1	= n0
			;	STOP
			;ENDIF ELSE BEGIN
			;	cut	= WHERE(blist GE 0L)
			;	blist	= blist(cut)
			;	cut	= SORT(blist)
			;	blist	= blist(cut)
			;	blist	= blist(UNIQ(blist))

			;	ind	= p_TFRun_corr_detbr(settings, tree, complete_tree, blist)
			;ENDELSE

			IF blist EQ -1L THEN BEGIN
				n1	= n0
				;STOP
			ENDIF ELSE IF blist EQ -2L THEN BEGIN
				n1 	= tree.snap(0)
			ENDIF ELSE BEGIN
				tree	= p_TFRun_corr_link(settings, tree, complete_tree, tree_key, ind, blist, remove_ind)


				IF settings.P_TFRun_corr_log EQ 1L THEN $
					P_TFRun_corr_sanitycheck, settings, tree, n0
				n1	= tree.snap(0)
			ENDELSE
		ENDREP UNTIL tree.snap(0) LE settings.P_TFrun_corr_snap_i OR n0 EQ n1

			;g	= f_getevol(tree, 1026L, gal(i).ID, datalist=['Mass_tot'], dir='/storage5/NewHorizon/VELOCIraptor/', horg=settings.horg)
			;IF settings.horg EQ 'h' THEN dir_horg = STRTRIM('halo',2)
			;IF settings.horg EQ 'g' THEN dir_horg = STRTRIM('galaxy',2)
			;cgPS_open, '/storage6/jinsu/var/Paper4_Group/catalog/' + dir_horg + '/l1/gal_' + STRING(gal(i).ID, format='(I4.4)') + '.eps', /encapsulated
			;cgDisplay, 800, 800
			;!p.font = -1 & !p.charsize = 1.5 & !p.charthick = 4.0
			;cgPlot, g.snapnum, g.mass_tot, /ylog, linestyle=0, position=[0.18, 0.18, 0.95, 0.95], xrange=[0., 1200.], $
			;	xtitle='Snap #', ytitle=textoidl('Stellar Mass [M' + sunsymbol() + ']')
			;cgOplot, [110, 110], [1e6, 1e13], linestyle=2, color='red', thick=3	;; a~0.2
			;cgOplot, [335, 335], [1e6, 1e13], linestyle=2, color='red', thick=3	;; a~0.4
			;cut_merit	= WHERE(tree.m_merit GT 0.001 , ncut_merit)
			;IF ncut_merit GE 1L THEN BEGIN
			;	FOR j=0L, ncut_merit-1L DO BEGIN
			;		ss 	= tree.m_snap(cut_merit(j))
			;		mm 	= g(WHERE(g.snapnum EQ ss)).mass_tot
			;		cgOplot, ss, mm, psym=9, color='red', symsize=1.5
			;	ENDFOR
			;ENDIF
			;cgPS_close
		complete_tree(bid(i))	= PTR_NEW(tree, /no_copy)
		corr_idlist(i)	= gal(i).ID
		;STOP	;456456
		IF tree_key(0) NE 10000L THEN STOP
	ENDFOR
	cut	= WHERE(corr_idlist GE 0L, ncut)
	IF ncut GE 1L THEN corr_idlist = corr_idlist(cut)

	SAVE, filename=settings.dir_tree + 'ctree.sav', complete_tree, corr_idlist, tree_key
	;; REMOVE IND
END

;;-----
;; SAVE as hdf5
;;-----
PRO p_TFRun_save_writehdf5, tmp, file

		fid	= h5f_create(file)
		;;----- WRITE PROPERTY

		;; IDs
		SIMPLE_WRITE_HDF5, tmp.id, 'ID', fid

		;; SNAPs
		SIMPLE_WRITE_HDF5, tmp.snap, 'Snap', fid

		;; STAT
		SIMPLE_WRITE_HDF5, tmp.stat, 'Stat', fid

		;; Merged branch
		IF tmp.numprog EQ 1L THEN BEGIN
			SIMPLE_WRITE_HDF5, [-1L], 'M_ID', fid
			SIMPLE_WRITE_HDF5, [-1.0d], 'M_Merit', fid
			SIMPLE_WRITE_HDF5, [-1L], 'M_BID', fid
			SIMPLE_WRITE_HDF5, [-1L], 'M_Snap', fid
		ENDIF ELSE BEGIN
			SIMPLE_WRITE_HDF5, tmp.m_id, 'M_ID', fid
			SIMPLE_WRITE_HDF5, tmp.m_merit, 'M_Merit', fid
			SIMPLE_WRITE_HDF5, tmp.m_bid, 'M_BID', fid
			SIMPLE_WRITE_HDF5, tmp.m_snap, 'M_Snap', fid
		ENDELSE

		;; ENDDING INDEX
		SIMPLE_WRITE_HDF5, tmp.endind, 'Endind', fid

		;; # of Progenitors
		SIMPLE_WRITE_HDF5, tmp.numprog, 'NumProg', fid

		h5f_close, fid
END

PRO p_TFRun_save, settings, complete_tree

	;;-----
	;; Default settings
	;;-----
	snap0	= 1026L

	;;-----
	;; Make a Directory
	;;-----
	dir	= settings.dir_catalog + 'VR_Tree/'
	dir_s	= dir + 'snap_' + STRING(snap0,format='(I4.4)') + '/'
	dir_a	= dir + 'all/'
	SPAWN, 'mkdir ' + STRTRIM(dir_s,2)
	SPAWN, 'mkdir ' + STRTRIM(dir_a,2)

	;;-----
	;; SAVE ALL BRANCHES
	;;-----
	n_tree	= N_ELEMENTS(complete_tree)
	FOR i=0L, n_tree-1L DO BEGIN
		tmp	= *complete_tree(i)
		IF tmp.stat EQ 'removed' THEN CONTINUE
		file	= dir_a + 'TREE_' + STRING(i,format='(I6.6)') + '.hdf5'

		;;----- CREATE A HDF5 File
		p_TFRun_save_writehdf5, tmp, file

		PRINT, i
	ENDFOR

	;;-----
	;; LOAD GAL
	;;-----
	gal	= f_rdgal(snap0, -1L, column_list=settings.column_list, dir=settings.dir_catalog, $
		horg=settings.horg)
	bid	= p_TFRun_corr_getbr(complete_tree, gal, snap0)

	;;-----
	;; GET TREE AND SAVE IT
	;;-----

	FOR i=0L, N_ELEMENTS(gal)-1L DO BEGIN
		IF bid(i) LT 0L THEN CONTINUE
		tmp	= *complete_tree(bid(i))
		FOR j=0L, N_ELEMENTS(complete_tree)-1L DO BEGIN
			tmp	= *complete_tree(j)
			cut	= WHERE(tmp.snap EQ snap0)
			IF MAX(cut) LT 0L THEN CONTINUE
			IF tmp.id(cut) EQ gal(i).ID THEN BREAK
		ENDFOR

		file	= dir_s + 'TREE_' + STRING(gal(i).ID,format='(I6.6)') + '.hdf5'

		;;----- WRITE PROPERTY
		p_TFRun_save_writehdf5, tmp, file

	ENDFOR

END


;;-----
;; Main Part
;;-----
;;----- REALLOCATE
PRO p_tfrun_reallocate_t, tree, gind, evoldum, maxgind
	maxind	= N_ELEMENTS(tree) + maxgind
	;; tree
	tree2	= REPLICATE(tree(0), maxind)
	tree2(0L:gind)	= tree(0L:gind)
	tree	= tree2

	;; evoldum
	evoldum2	= {ID:LONARR(maxind)-1L, snap:LONARR(maxind)-1L, merit:DBLARR(maxind)-1.d}
	evoldum2.ID(0L:gind)	= evoldum.ID(0L:gind)
	evoldum2.snap(0L:gind)	= evoldum.snap(0L:gind)
	evoldum2.merit(0L:gind)	= evoldum.merit(0L:gind)
	evoldum	= evoldum2
END
PRO p_tfrun_reallocate_ct, complete_tree, n_comp, maxgind
	maxind	= N_ELEMENTS(complete_tree) + maxgind
	;; complete_tree
	complete_tree2	= PTRARR(maxind)
	complete_tree2(0L:n_comp-1L)	= complete_tree(0L:n_comp-1L)
	complete_tree	= complete_tree2
END

;;----- GEN KEY
FUNCTION p_TFRun_makebr_genkey, settings, tree

	MAX_snap	= 200L
	MAX_ID		= 10000L

	genkey_redo:
	tree_key 	= LONARR(MAX_snap + settings.P_TFrun_bidkey*MAX_ID) - 1L

	n_tree 		= N_ELEMENTS(tree)
	FOR i=0L, n_tree-1L DO BEGIN
		tmp	 	= *tree(i)
		s 		= tmp.snap
		id 		= tmp.id
		ind 	= s + settings.P_TFrun_bidkey*id

		IF MAX(id) GT MAX_ID THEN BEGIN
			MAX_ID 		= MAX(id) + 1L
			GOTO, genkey_redo
		ENDIF
		IF MAX(s) GT MAX_snap THEN BEGIN
			MAX_snap 	= MAX(s) + 1L
			GOTO, genkey_redo
		ENDIF

		IF MAX(s) GT settings.P_TFrun_bidkey THEN BEGIN
			power 	= LOGN(ALOG10(MAX(s))) + 1.d
			settings.P_TFrun_bidkey = LONG(10.d^power)
			GOTO, genkey_redo
		ENDIF


		tree_key(ind) 	= i
	ENDFOR
	RETURN, tree_key
END

;;-----
;; CNAME
;;-----
PRO p_tfrun_makebr_cname, settings
	dir	= settings.dir_tree
        flist   = dir + '/tree.snaplist.txt'
        slist   = LONARR(FILE_LINES(flist))
        OPENR, 10, flist
        FOR i=0L, FILE_LINES(flist)-1L DO BEGIN
                str     = ' '
                READF, 10, str
                ind     = STRPOS(str,'snap_')
                str     = STRMID(str,ind+5,4)
                slist(i)= LONG(str)
        ENDFOR
        CLOSE, 10

        tfile   = FILE_SEARCH(dir + '/tree.snapshot*.tree')
        IF N_ELEMENTS(tfile) NE N_ELEMENTS(slist) THEN BEGIN
                PRINT, 'wrong number of tree results'
                STOP
        ENDIF

        tnumber = LONARR(N_ELEMENTS(tfile))
        FOR i=0L, N_ELEMENTS(tfile)-1L DO BEGIN
                str     = tfile(i)
                str     = STRSPLIT(str, '_' ,/extract)
                str     = str(1)
                str     = STRSPLIT(str, '.', /extract)
                str     = str(0)
                tnumber(i)      = LONG(str)
        ENDFOR
        cut     = SORT(tnumber)
        tfile   = tfile(cut)

        FOR i=0L, N_ELEMENTS(tfile)-1L DO BEGIN
                orgname = tfile(i)
                newname = 'tree.snapshot_' + STRING(slist(i),format='(I4.4)') + 'VELOCIraptor'
                IF orgname EQ newname THEN CONTINUE
                tmp     = 'mv ' + $
                        orgname + ' ' + dir + '/' + newname
                SPAWN, tmp
        ENDFOR

        FOR i=0L, N_ELEMENTS(tfile)-1L DO BEGIN
                orgname = 'tree.snapshot_' + STRING(slist(i),format='(I4.4)') + 'VELOCIraptor'
                newname = orgname + '.tree'
                tmp     = 'mv ' + dir + '/' + orgname + ' ' + dir + '/' + newname
                SPAWN, tmp
        ENDFOR
END

PRO p_tfrun, settings

IF settings.p_tfrun_makebr EQ 1L THEN BEGIN
	;;-----
	;; Basic settings
	;;-----
	treeset	= p_tfrun_set(settings)
	maxgind	= 10000L

	;;-----
	;; Allocate Memory
	;;-----
	dumarr	= LONARR(ABS(treeset.N1-treeset.N0)+1L)-1L
	dumarr2	= LONARR(5000L) - 1L
	dumstr	= {ID:dumarr, snap:dumarr, $
		p_snap:dumarr, p_ID:dumarr, p_merit:DOUBLE(dumarr), $
		d_snap:dumarr, d_ID:dumarr, endind:-1L, $
		m_ID:dumarr2, m_snap:dumarr2, m_merit:DOUBLE(dumarr2), m_BID:dumarr2, $
		numprog:1L}
	tree	= REPLICATE(dumstr, maxgind)
	complete_tree	= PTRARR(maxgind)

	evoldum	= {ID:LONARR(maxgind)-1L, snap:LONARR(maxgind)-1L, merit:DBLARR(maxgind)-1.d}

	;;-----
	;; CHANGE TREE FILE NAME
	;;-----
	p_tfrun_makebr_cname, settings

	;;-----
	;; GO FORWARD
	;;-----

	gind	= -1L
	n_comp	= 0L
	treelog	= {n_new:0L, n_link:0L, n_link2:0L, n_link3:0L, n_broken:0L, n_all:0L}
	FOR i=treeset.N0, treeset.N1, treeset.DN DO BEGIN
		;;----- REALLOCATE
		IF N_ELEMENTS(tree) - gind LT 2000L THEN $
			p_tfrun_reallocate_t, tree, gind, evoldum, maxgind
		IF N_ELEMENTS(complete_tree) - n_comp LT 2000L THEN $
			p_tfrun_reallocate_ct, complete_tree, n_comp, maxgind

		IF i LT settings.p_TFrun_corr_nn * 10L THEN BEGIN
			CONTINUE
		ENDIF ELSE IF i EQ settings.p_TFrun_corr_nn * 10L THEN BEGIN
			IF i NE treeset.N0 THEN BEGIN
				RESTORE, settings.dir_tree + 'tree_' + STRING(i,format='(I4.4)') + '.sav'
			ENDIF
		ENDIF ELSE IF i EQ settings.p_TFrun_corr_nn * 10L + 10L THEN BEGIN
			SAVE, filename=settings.dir_tree + $
				'tree_' + STRING(i,format='(I4.4)') + '.sav', $
				treelog, tree, complete_tree, n_comp, gind, evoldum
			STOP
		ENDIF

		;;-----
		;; SNAPSHOT CHECK
		;;-----
		dumfname	= settings.dir_catalog
		IF settings.horg EQ 'h' THEN dumfname += 'Halo/'
		IF settings.horg EQ 'g' THEN dumfname += 'Galaxy/'
		dumfname += 'snap_' + STRING(i,format='(I4.4)')

		IF STRLEN(FILE_SEARCH(dumfname)) LE 5L THEN CONTINUE

		snap_curr	= i
		snap_next	= p_tfrun_findnextsnap(settings,snap_curr)

		;;-----
		;; LAST SNAPSHOT
		;;-----
		IF i EQ treeset.N1 OR snap_next EQ -1L THEN BEGIN
			g_curr	= f_rdgal(snap_curr, -1L, column_list=['ID', 'npart'],$
				dir=settings.dir_catalog, horg=settings.horg)
			p_tfrun_lastsnap, settings, complete_tree, n_comp, $
				tree, evoldum, gind, g_curr, snap_curr

			FOR j=0L, gind DO BEGIN
				IF N_ELEMENTS(complete_tree) - n_comp LT 2000L THEN $
					p_tfrun_reallocate_ct, complete_tree, n_comp, maxgind

				IF ABS(tree(j).snap(0)-snap_curr) GE 10L THEN $
					p_tfrun_finishbranch, settings, tree, complete_tree, n_comp, j, 'main'
			ENDFOR

			complete_tree	= complete_tree(0L:n_comp-1L)
			BREAK
		ENDIF
		;;-----
		;; LOAD NEEDED DATA
		;;-----
		treefname	= settings.dir_tree + 'tree.snapshot_' + $
			STRING(snap_curr,format='(I4.4)') + 'VELOCIraptor.tree'
		t_curr	= p_tfrun_rdhdf5(treefname, treeset)
		g_curr	= f_rdgal(snap_curr, -1L, column_list=['ID', 'npart'], $
			dir=settings.dir_catalog, horg=settings.horg)

		g_next	= f_rdgal(snap_next, -1L, column_list=['ID', 'npart'], $
			dir=settings.dir_catalog, horg=settings.horg)

		IF t_curr.nlink GE 2L THEN BEGIN
			FOR i2=0L, t_curr.nlink-2L DO BEGIN
				snap_next	= p_tfrun_findnextsnap(settings, snap_next)
				IF snap_next EQ -1L THEN CONTINUE
				g_dum	= f_rdgal(snap_next, -1L, column_list=['ID', 'npart'], $
					dir=settings.dir_catalog, horg=settings.horg)
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
	ENDFOR

	;; GENERATE KEY
	tree_key	= p_TFRun_makebr_genkey(settings, complete_tree)
	tree_key(0) = settings.P_TFrun_bidkey
	SAVE, filename=settings.dir_tree + 'tree.sav', complete_tree, tree_key
	PRINT, 'Done ^-^'

	STOP
ENDIF

IF settings.p_tfrun_corr EQ 1L THEN BEGIN
	RESTORE, settings.dir_tree + 'tree.sav'

	p_TFRun_corr, settings, complete_tree, tree_key

	;PRINT, 'Mergered branch mutual link'
	;PRINT, 'finish branch modulate m_ID'
ENDIF

IF settings.p_tfrun_save EQ 1L THEN BEGIN
	RESTORE, settings.dir_tree + 'ctree.sav'

	p_TFRun_save, settings, complete_tree
ENDIF
END
