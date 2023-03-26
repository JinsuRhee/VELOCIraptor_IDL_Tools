;;-----
;; INIT
;;-----

FUNCTION p_ctree_init, settings, tree_set

	;; Snap list
	file 	= settings.dir_tree + 'tree.snapshot_*VELOCIraptor.tree'
	file 	= FILE_SEARCH(file)
	slist 	= LONARR(N_ELEMENTS(file))
	FOR i=0L, N_ELEMENTS(file)-1L DO BEGIN
		tmp 	= file(i)
		tmp 	= (STRSPLIT(tmp, '/', /extract))[-1]
		i0 	= STRPOS(tmp, '_')
		i1 	= STRPOS(tmp, 'VELO')
		tmp0 	= STRMID(tmp, i0+1L, i1-i0-1L)
		slist(i)= LONG(tmp0)
	ENDFOR

	tree_set 	= CREATE_STRUCT(tree_set, 'slist', slist);, 'unit_l', unit_l, 'unit_t', unit_t, 'aexp', aexp, 'lbt', lbt)

	;; Get Info
	aexp	= DBLARR(MAX(slist)+1L)
	unit_l	= aexp
	unit_t	= aexp
	age	= aexp
	t_conf	= aexp

	oM	= 0.d
	oL	= 0.d
	H0	= 0.d
	FOR i=0L, N_ELEMENTS(slist)-1L DO BEGIN
		fname	= tree_set.dir_raw + 'output_' + STRING(slist(i),format='(I5.5)') + '/info_' + STRING(slist(i),format='(I5.5)') + '.txt'
		OPENR, 10, fname
		dum = ' '
		FOR j=1L, 8L DO READF, 10, dum

		READF, 10, dum
		dum2	= STRSPLIT(dum, '=', /extract)
		t_conf(slist(i))	= DOUBLE(dum2(1))

		READF, 10, dum
		dum2	= STRSPLIT(dum, '=', /extract)
		aexp(slist(i)) = DOUBLE(dum2(1))

		IF i EQ 0L THEN BEGIN
			READF, 10, dum
			dum2	= STRSPLIT(dum, '=', /extract)
			H0	= dum2(1)

			READF, 10, dum
			dum2	= STRSPLIT(dum, '=', /extract)
			oM	= dum2(1)

			READF, 10, dum
			dum2	= STRSPLIT(dum, '=', /extract)
			oL	= dum2(1)

			FOR j=1, 2 DO READF, 10, dum
		ENDIF ELSE BEGIN
			FOR j=1, 5L DO READF, 10, dum
		ENDELSE

		READF, 10, dum
		dum2	= STRSPLIT(dum, '=', /extract)
		unit_l(slist(i)) = DOUBLE(dum2(1))
		
		READF, 10, dum

		READF, 10, dum
		dum2	= STRSPLIT(dum, '=', /extract)
		unit_t(slist(i)) = DOUBLE(dum2(1))
		CLOSE, 10
	ENDFOR

	cut	= WHERE(aexp GT 0.)
	age(cut)	= get_time(slist, aexp(cut), h0=h0, om=om, ol=ol)
	tree_set	= CREATE_STRUCT(tree_set, 'aexp', aexp, 'unit_l', unit_l, 'unit_t', unit_t, 'age', age)

	RETURN, tree_set
END

FUNCTION p_ctree_getweight, pid

	nn 	= N_ELEMENTS(pid)
	weight	= DINDGEN(nn) + 1.d
	weight	= REVERSE(weight) / nn
	weight	/= (0.5772156649d + ALOG(nn*1.d))
	RETURN, weight
END
;;-----
;; ALLOCATE
;;-----
FUNCTION p_ctree_allocate, tree_set, gal, complete_tree, tree_key
	tmp 	= {ID0:0L, $		;; ID at the final snapshot
		ID:0L, snap:0L, $ 	;; End of id and snap at the current tree
		;IDc:0L, snapc:0L, $	;; check point to compute merit
		pos:DBLARR(3), vel:DBLARR(3), $	;; position & velocity
		stat:'T', $		;; T (Tree exists), C (To be connected), B (Broken)
		detstat:-1L, $		;; -1 (End is not specified yet) 1 (specified)
		p_list:PTR_NEW(0.d), $ 	;; Particle list at the ending point
		n_ptcl:-1L, $		;; # of p_list
		list:REPLICATE({merit:0.d, id:-1L, snap:-1L}, tree_set.n_search), $
		list_n:0L, $
								;; Meirt list
		etc:'etc'}
		
	tmp 	= REPLICATE(tmp, N_ELEMENTS(gal))

	tmp.ID0 	= gal.id
	FOR i=0L, N_ELEMENTS(gal)-1L DO BEGIN
		tree 	= f_gettree(gal(i).snapnum, gal(i).id, complete_tree, tree_key)
		IF TYPENAME(tree) NE 'POINTER' THEN BEGIN
			tmp(i).snap = gal(i).snapnum
			tmp(i).id 	= gal(i).ID

			;tmp(i).snapc= gal(i).snapnum
			;tmp(i).idc 	= gal(i).id
		ENDIF ELSE BEGIN
			tmp(i).snap = (*tree).snap(0)
			tmp(i).id  	= (*tree).id(0)

			;tmp(i).snapc= (*tree).snap(0)
			;tmp(i).idc 	= (*tree).id(0)
		ENDELSE
	ENDFOR
	RETURN, tmp
END

;;-----
;; Collect Particle ID on the brach that exists for a long time
;;-----
FUNCTION p_ctree_collectpidonbranch, tree_set, slist, idlist

	pid 	= f_rdpid(slist(0), idlist(0), horg=tree_set.horg, $
		dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)
	pweight	= p_ctree_getweight(pid)

	nn 		= N_ELEMENTS(pid)
	pid 	= [pid, LON64ARR( (10LL*nn*tree_set.n_step_bw) < 500000000LL)]
	pweight = [pweight, DBLARR( (10LL*nn*tree_set.n_step_bw) < 500000000LL)]

	ind0	= nn-1L

	loop_n 	= 1L
	loop_ind= tree_set.n_step_dn
	REPEAT BEGIN
		IF loop_ind GE N_ELEMENTS(slist) THEN BREAK

		pid0	= f_rdpid(slist(loop_ind), idlist(loop_ind), horg=tree_set.horg, $
			dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)
		pweight0= p_ctree_getweight(pid0)

		ind1 	= ind0 + N_ELEMENTS(pid0)-1L

		IF ind1 GE N_ELEMENTS(pid) THEN pid = [pid, LON64ARR((ind1-ind0)*10L)]
		IF ind1 GE N_ELEMENTS(pweight) THEN pweight = [pweight, DBLARR((ind1-ind0)*10L)]

		pid(ind0:ind1)	= pid0
		pweight(ind0:ind1)	= pweight0
		ind0 	= ind1 + 1L

		loop_n ++
		loop_ind 	+= tree_set.n_step_dn
	ENDREP UNTIL loop_n GE tree_set.n_step_bw OR loop_ind GE N_ELEMENTS(slist)

	pid 	= pid(0L:ind0-1L)
	pweight = pweight(0L:ind0-1L)

	sortind	= SORT(pid)
	pid 	= pid(sortind)
	pweight = pweight(sortind)

	uind 	= UNIQ(pid)
	pid 	= pid(uind)
	pweight = pweight(uind)

	numid	= uind - [-1L, uind(0L:-2L)]

	;denu 	= 1.d
	n_step_bw0	= tree_set.n_step_bw
	REPEAT BEGIN
		ucut	= WHERE(numid GE n_step_bw0, uncut);tree_set.n_step_bw/denu, uncut)
		;denu 	+= 1.d
		n_step_bw0 --
	ENDREP UNTIL DOUBLE(uncut) / DOUBLE(N_ELEMENTS(pid)) GT 0.25d

	pid 	= pid(ucut)
	pweight = pweight(ucut)
	RETURN, {pid:pid, weight:pweight, n_con:n_step_bw0+1L}
END

;;-----
;; SEARCH GALAXY LIST of which merit is computed
;;-----
;PRO p_ctree_statcheck, tree_set, data, snap0
;	delN 	= data.snap0 - snap0
;
;	cut 	= WHERE(delN LE 0, ncut)
;	IF ncut GE 1L THEN data(cut).stat = 'T'	;; Have Tree
;
;	cut 	= WHERE(delN GT 0 AND delN LE tree_set.n_search, ncut)
;	IF ncut GE 1L THEN data(cut).stat = 'C' ;; To be connected
;
;	cut 	= WHERE(delN GT tree_set.n_search, ncut)
;	IF ncut GE 1L THEN data(cut).stat = 'B' ;; Broken Tree
;END

;;-----
;; COMPUTE MERIT
;;-----
;PRO p_ctree_com_merit, tree_set, i, ctree_data, glist, pid0
;
;	nn	= N_ELEMENTS(glist)
;	FOR i=0L, nn-1L DO BEGIN
;		ind0	= glist(i)
;
;		FOR j=0L, tree_set.n_merit-1L DO BEGIN
;			IF ctree_data(ind0).p_slist(j) EQ 0L THEN CONTINUE
;			pid 	= *(ctree_data(ind0).p_pidlist(j))
;
;			;; merge all pid and use a hash table one time
;
;		ENDFOR
;
;	ENDFOR
;END



;;-----
;; Particle list from galaxies to be connedted
;;-----
;FUNCTION p_ctree_plistgal, tree_set, data, c_snap, p_snap
;
;	cut 	= WHERE(data.stat EQ 'C' AND data.n_ptcl LT 0L, ncut)
;
;	FOR i=0L, ncut-1L DO BEGIN
;		ind 	= cut(i)
;
;		s0 	= data(ind).snap
;		i0 	= data(ind).id
;
;		key 	= s0*tree_set.tree_key + i0
;
;		sind 	= WHERE(p_snap.snap EQ s0, ns)
;		IF ns EQ 0L THEN BEGIN
;			PRINT, '?'
;			STOP
;		ENDIF
;
;		ext_ind 	= WHERE((*p_snap.gid) EQ key, nptcl)
;		data(ind).p_list 	= PTR_NEW((*p_snap.ptcl)(ext_ind))
;		data(ind).n_ptcl 	= nptcl
;	ENDFOR
;
;	;; INPUT PTCL
;	cut 	= WHERE(data.stat EQ 'C', ncut)
;	n_ptcl 	= LONG(TOTAL(data(cut).n_ptcl))
;	pid 	= LON64ARR(n_ptcl)
;	gid 	= LONARR(n_ptcl)
;
;	i0 	= 0L
;	FOR i=0L, ncut-1L DO BEGIN
;		ind 	= cut(i)
;
;		pid(i0:i0+data(ind).n_ptcl-1L) 	= (*(data(ind).p_list)).id
;
;		key 	= data(ind).id0 + tree_set.tree_key * data(ind).snap0
;		gid(i0:i0+data(ind).n_ptcl-1L) 	= key
;		i0 	+= data(ind).n_ptcl
;	ENDFOR
;	RETURN, {pid:pid, gid:gid}
;
;
;END

;;-----
;; Particle list from galaxies at this snapshot
;;-----
;FUNCTION p_ctree_plistsnap, tree_set, data, c_snap
;
;	;;-----
;	;; 123123 Select proper gal
;	;;-----
;
;	gal0 	= f_rdgal(c_snap, -1, horg=tree_set.horg, dir=tree_set.dir, column_list=['ID', 'npart'])
;
;	n_ptcl 	= LONG(TOTAL(gal0.npart))
;
;	pid 	= LON64ARR(n_ptcl)
;	gid 	= LONARR(n_ptcl)
;
;	i0 	= 0L
;	FOR i=0L, N_ELEMENTS(gal0)-1L DO BEGIN
;		;ptcl0 	= f_rdptcl(c_snap, gal0(i).id, horg=tree_set.horg, dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, num_thread=tree_set.num_thread, /p_id, /silent)
;		pid0 	 = f_rdpid(c_snap, gal0(i).id, horg=tree_set.horg, dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)
;		cut0 	= WHERE(pid0 GT -1e10)
;		pid(i0:i0+gal0(i).npart-1L) 	= pid0(cut0)
;		gid(i0:i0+gal0(i).npart-1L) 	= gal0(i).ID + tree_set.tree_key * c_snap
;		i0 	+= gal0(i).npart
;	ENDFOR
;
;	RETURN, {pid:pid, gid:gid}
;END

;;-----
;; Particle list input
;;-----
;PRO p_ctree_plistsnap_input, p_snap, pid_s, c_snap
;
;	n_search 	= N_ELEMENTS(p_snap.snap)
;	FOR i=0L, n_search-1L DO BEGIN
;		IF p_snap.snap(i) LT 0L THEN BEGIN
;			p_snap.snap(i) 	= c_snap
;			p_snap.ptcl(i)	= PTR_NEW(pid_s.pid)
;			p_snap.gid(i) 	= PTR_NEW(pid_s.gid)
;			BREAK
;		ENDIF ELSE IF i EQ n_search-1L THEN BEGIN
;			p_snap.snap(1L:-1L) 	= p_snap.snap(0L:-2L)
;			p_snap.ptcl(1L:-1L) 	= p_snap.ptcl(0L:-2L)
;			p_snap.gid(1L:-1L) 		= p_snap.ptcl(0L:-2L)
;
;			p_snap.snap(i) 	= c_snap
;			p_snap.ptcl(i)	= PTR_NEW(pid_s.pid)
;			p_snap.gid(i) 	= PTR_NEW(pid_s.gid)
;		ENDIF ELSE BEGIN
;			PRINT, ' ? '
;			STOP
;		ENDELSE
;	ENDFOR
;END

;;-----
;; Collect pID
;;-----
PRO p_ctree_collectpid, tree_set, data, pid, complete_tree, tree_key

	cut	= WHERE(data.stat EQ 'C' AND data.n_ptcl LT 0L, ncut)
	IF ncut GE 1L THEN BEGIN
		FOR i=0L, ncut-1 DO BEGIN
			ind	= cut(i)

			kval 	= data(ind).snap + tree_key(0)*data(ind).id
			tind 	= tree_key(kval)

			IF tind LT 0L THEN BEGIN
				pid0	= f_rdpid(data(ind).snap, data(ind).id, horg=tree_set.horg, $
					dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)
				pweight0= p_ctree_getweight(pid0)
			ENDIF ELSE BEGIN
				tree 	= *complete_tree(tind)
				t_cut	= WHERE(tree.snap GE data(ind).snap, t_nn)
				IF t_nn EQ 0L THEN STOP ;; weird

				t_slist	= tree.snap(t_cut)
				t_idlist= tree.id(t_cut)

				cpid 	= p_ctree_collectpidonbranch(tree_set, t_slist, t_idlist)

				pid0	= cpid.pid
				pweight0= cpid.weight
				;pid0	= p_ctree_collectpidonbranch(tree_set, t_slist, t_idlist)
			ENDELSE

			;pid0	= f_rdpid(data(ind).snapc, data(ind).idc, horg=tree_set.horg, $
			;	dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)

			data(ind).n_ptcl	= N_ELEMENTS(pid0)
			data(ind).p_list	= PTR_NEW(pid0, /no_copy)

			gal0	= f_rdgal(data(ind).snap, data(ind).id, horg=tree_set.horg, $
				dir=tree_set.dir, column_list=['Xc', 'Yc', 'Zc', 'VXc', 'VYc', 'VZc'])

			data(ind).pos	= [gal0.xc, gal0.yc, gal0.zc]
			data(ind).vel	= [gal0.vxc, gal0.vyc, gal0.vzc]
		ENDFOR
	ENDIF

	;;
	cut	= WHERE(data.n_ptcl GE 0L, ncut)
	IF ncut EQ 0L THEN BEGIN
		PRINT, '?'
		STOP
	ENDIF

	nptcl	= TOTAL(data(cut).n_ptcl)

	dum	= {pid:LONG64(1), gid:0L}
	pid	= REPLICATE(dum, nptcl)

	i0	= 0L
	FOR i=0L, ncut-1L DO BEGIN
		ind	= cut(i)

		i1	= i0+data(ind).n_ptcl-1L
		IF i1 GE N_ELEMENTS(pid) THEN BEGIN
			pid	= [pid, REPLICATE(pid(0),i1-i0+1L)]
		ENDIF
		pid(i0:i1).pid	= *data(ind).p_list
		pid(i0:i1).gid	= data(ind).id0

		i0	= i1+1L
	ENDFOR
	pid	= pid(0L:i1)
END
;;-----
;; Determine ending point
;;-----
PRO p_ctree_detend, tree_set, data, complete_tree, tree_key

	cut 	= WHERE(data.stat EQ 'C', ncut)

	IF ncut GE 1L THEN BEGIN
		FOR i=0L, ncut-1L DO BEGIN
			ind	= cut(i)
			tkey	= data(ind).snap + data(ind).id*tree_key(0)
			tind	= tree_key(tkey)

			;IF tind GE 1L THEN BEGIN
			;	tdum	= *complete_tree(tind)
			;	cut2	= WHERE(tdum.p_merit GT 0.25, nc)
			;	IF nc GE 1L THEN BEGIN
			;		cut2	= cut2(0)
			;		data(ind).snapc = tdum.snap(cut2)
			;		data(ind).idc	= tdum.id(cut2)
			;	ENDIF
			;ENDIF

		ENDFOR
		data(cut).detstat	= 1L
	ENDIF
END
;;-----
;; READ SNAPshot particles
;;-----
FUNCTION p_ctree_readsnap, tree_set, data, snap0

	;;-----
	;; FIRST FIND COMOVING VOLUME
	;;-----

	cut	= WHERE(data.stat EQ 'C', ncut)
	cen	= DBLARR(ncut,3)
	rad	= DBLARR(ncut)
	FOR i=0L, ncut-1L DO BEGIN
		ind	= cut(i)
		pos	= data(ind).pos
		vel	= data(ind).vel

		unit_l	= tree_set.unit_l(data(ind).snap)
		cen(i,*)= pos*3.086d21/unit_l

		speed	= NORM(vel) / 3.086d16  * (365.d * 86400d * 1e9) 	;; [kpc/Gyr]
		rad(i)	= speed * tree_set.rfact * ABS(tree_set.age(snap0) - tree_set.age(data(ind).snap)) * 3.086d21 / unit_l

	ENDFOR

	gal	= f_rdgal(snap0, -1L, horg=tree_set.horg, dir=tree_set.dir, column_list=['ID', 'npart', 'Xc', 'Yc', 'Zc'])

	nptcl	= TOTAL(gal.npart)
	dum 	= {pid:LONG64(0), gid:-1L}
	pid	= REPLICATE(dum, nptcl)

	i0	= 0L
	checkind	= LONARR(N_ELEMENTS(gal))-1L
	FOR i=0L, N_ELEMENTS(gal)-1L DO BEGIN

		;;----- Distance check
		pos	= [gal(i).xc, gal(i).yc, gal(i).zc] * 3.086d21 / tree_set.unit_l(snap0)
		d3d	= js_d3d(cen(*,0), cen(*,1), cen(*,2), pos) / rad

		IF MIN(d3d) GT 1. THEN CONTINUE
		checkind(i)	= 1L

		pid0	= f_rdpid(snap0, gal(i).id, horg=tree_set.horg, dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)

		npart0	= N_ELEMENTS(pid0)
		i1	= i0+npart0-1L
		IF i1 GE N_ELEMENTS(pid) THEN pid = [pid, REPLICATE(dum,npart0)]

		pid(i0:i1).pid	= pid0
		pid(i0:i1).gid	= gal(i).id
		i0	= i1 + 1L
	ENDFOR
	cut	= WHERE(pid.gid GT 0L, ncut)
	IF ncut EQ 0L THEN BEGIN
		PRINT, 'Why no gal?' ; may incrase rfact?
		STOP
	ENDIF
	pid	= pid(cut)

	;;-----
	;; Make Hash
	;;-----
	;; split index
	num_thread	= 1L

	dN	= N_ELEMENTS(pid)/num_thread
	indarr	= LONARR(num_thread, 2L)
	FOR i=0L, num_thread-1L DO BEGIN
		indarr(i,0)	= dN*i
		indarr(i,1)	= dN*(i+1L) - 1L
	ENDFOR
	indarr(-1,1)	= N_ELEMENTS(pid)-1L

	;; hash allocate
	dN	= MAX(indarr(*,1)-indarr(*,0))+1L
	hash		= LONARR(dN,num_thread) - 1L
	hash_next	= LONARR(dN,num_thread) - 1L

	;; fortran
	ftr_name        = tree_set.dir_root + 'src/sub/sub_ftn/get_hash.so'
	 	larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= N_ELEMENTS(pid)
		larr(1)	= dN

		larr(10)= num_thread

	void	= CALL_EXTERNAL(ftr_name, 'get_hash', $
		larr, darr, pid.pid, indarr, hash, hash_next)


	cut	= WHERE(checkind GE 0L)

	RETURN, {pid:pid, n_ptcl:gal(cut).npart, gid:gal(cut).id, hash:hash, hash_next:hash_next, dn:dn}

END
;;-----
;; TREE classification tree
;;-----
PRO p_ctree_classify, tree_set, data, snap0, number

	number	= {T:0L, C:0L, B:0L}

	ind0	= (WHERE(tree_set.slist EQ snap0))[0]
	FOR i=0L, N_ELEMENTS(data)-1L DO BEGIN
		;; Tree Exists
		IF data(i).snap LE snap0 THEN BEGIN
			data(i).stat	= 'T'
			number.T	++
			CONTINUe
		ENDIF

		ind	= (WHERE(tree_set.slist EQ data(i).snap))[0]

		;; For altered Tree
		IF data(i).list(0).snap GT 0L THEN BEGIN
			ind1 	= (WHERE(tree_set.slist EQ data(i).list(0).snap))[0]
		ENDIF ELSE BEGIN
			ind1 	= ind0 + tree_set.n_search*2L
		ENDELSE
		


		IF ind - ind0 LE tree_set.n_search OR ind1 - ind0 LT tree_set.n_search THEN BEGIN
			data(i).stat 	= 'C'
			number.C	++
		ENDIF ELSE BEGIN
			data(i).stat	= 'B'
			number.B	++
		ENDELSE
	ENDFOR

	;; TREE Exists
	;cut	= WHERE(data.snap LE snap0, ncut)
	;IF ncut GE 1L THEN data(cut).stat = 'T'
	;number.T	= ncut

	;;; Tree to be connected
	;cut	= WHERE(data.snap GT snap0 AND data.snap LE snap0+tree_set.n_search, ncut)
	;IF ncut GE 1L THEN data(cut).stat = 'C'
	;number.C	= ncut

	;;; Broken Tree
	;cut	= WHERE(data.snap GT snap0+tree_set.n_search, ncut)
	;IF ncut GE 1L THEN data(cut).stat = 'B'
	;number.B	= ncut
	PRINT, ''
	PRINT, '			TREE CLASSIFICATION'
	PRINT, '				With tree       = ', number.T
	PRINT, '				To be connected = ', number.C
	PRINT, '				Broken          = ', number.B
	PRINT, ''
END
;;-----
;; MERIT CAL
;;-----
PRO p_ctree_commerit, tree_set, data, pid, pid0, snap0

	pid_g	= pid.pid
	gid_g	= pid.gid
	
	pid_s	= pid0.pid.pid
	gid_s	= pid0.pid.gid

	hash		= pid0.hash
	hash_next	= pid0.hash_next

	;; ALLOCATE (ID = IND in fortran)
	merit	= DBLARR(MAX(gid_g)+1L, MAX(gid_s)+1L)
	npart_g	= LONARR(MAX(gid_g)+1L)
	npart_s	= LONARR(MAX(gid_s)+1L)

	;; PTCL NUM INPUT
	cut	= WHERE(data.stat EQ 'C', ncut)
	npart_g(data(cut).ID0-1L)	= data(cut).n_ptcl
	npart_s(pid0.gid-1L)		= pid0.n_ptcl

	match_id	= LONARR(N_ELEMENTS(cut)) - 1L
	match_merit	= DBLARR(N_ELEMENTS(cut)) - 1.d

	ftr_name        = tree_set.dir_root + 'src/sub/sub_ftn/get_merit2.so'
	 	larr = LONARR(20) & darr = DBLARR(20)
		larr(0)	= N_ELEMENTS(pid_g)
		larr(1)	= N_ELEMENTS(pid_s)
		larr(2)	= N_ELEMENTS(npart_g)
		larr(3)	= N_ELEMENTS(npart_s)
		larr(4) = pid0.dn
		larr(5)	= N_ELEMENTS(hash(0,*))
		larr(6) = N_ELEMENTS(cut)

		larr(10)= 1L;tree_set.num_thread

	void	= CALL_EXTERNAL(ftr_name, 'get_merit2', $
		larr, darr, pid_g, gid_g, pid_s, gid_s, $
			hash, hash_next, $
			npart_g, npart_s, merit, $
			match_id, match_merit)

	FOR i=0L, ncut-1L DO BEGIN
		n0	= data(cut(i)).list_n
		data(cut(i)).list(n0).merit	= match_merit(i)
		data(cut(i)).list(n0).id	= match_id(i)
		data(cut(i)).list(n0).snap	= snap0
		data(cut(i)).list_n ++
	ENDFOR

	;cut1	= WHERE(npart_g GT 0L)
	;cut2	= WHERE(npart_s GT 0L)

	;FOR i=0L, N_ELEMENTS(cut1)-1L DO BEGIN
	;	id	= cut1(i)
	;	mdum	= merit(id,*)
	;	cut	= WHERE(mdum EQ MAX(mdum))
	;	IF MAX(mdum) Eq 0. THEN STOP
	;	PRINT, id+1, ' to ', cut(0)+1, ' merit = ', MAX(mdum)
	;	PRINT, ' 	', match_id(i), ' ', match_merit(i)
	;ENDFOR
	;STOP

	;	;TEST
	;	;ID dumping
	;;FOR i=0L, 
	;;FROM SELECTING nearby galaxies
	;STOP
END
;;-----
;; MAKE NEW BRANCH
;;-----
PRO p_ctree_makenewbr, snap0, id0, ctree, tkey

	tmp	= {ID:[id0], SNAP:[snap0], STAT:'main', P_SNAP:[-1L], P_ID:[-1L], P_MERIT:[-1.d], $
		M_ID:[-1L], M_MERIT:[-1.d], M_BID:[-1L], M_SNAP:[-1L], $
		D_SNAP:[-1L], D_ID:[-1L], ENDIND:0L, NUMPROG:0L}
	tmp_ptr	= PTR_NEW(tmp, /no_copy)
	ctree	= [ctree, tmp_ptr]
	tind	= N_ELEMENTS(ctree)-1L
	tkey(snap0 + tkey(0)*id0)	= tind
END
;;-----
;; EXPAND BRANCH
;;-----
PRO p_ctree_expandbr, data, ind, complete_tree, tree_key, idc, snapc, meritc
	kval	= data(ind).snap + tree_key(0)*data(ind).id
	tind	= tree_key(kval)
	IF tind LT 0L THEN BEGIN
		p_ctree_makenewbr, data(ind).snap, data(ind).id, complete_tree, tree_key
		tind 	= tree_key(data(ind).snap + tree_key(0)*data(ind).id)
	ENDIF
	tmp_tree= *complete_tree(tind)

	IF N_ELEMENTS(tmp_tree.id) EQ 1L THEN BEGIN
		p_snap	= [-1L, snapc]
		p_id	= [-1L, idc]
		p_merit	= [-1.d, meritc]
	ENDIF ELSE BEGIN
		p_snap	= [-1L, snapc, tmp_tree.p_snap(1L:*)]
		p_id	= [-1L, idc , tmp_tree.p_id(1L:*)]
		p_merit	= [-1.d, meritc, tmp_tree.p_merit(1L:*)]
	ENDELSE

	tmp	= {$
		ID	: [idc, tmp_tree.id], $
		SNAP	: [snapc, tmp_tree.snap], $
		STAT	: 'main', $
		P_SNAP	: p_snap, $
		P_ID	: p_id, $
		P_MERIT	: p_merit, $
		M_ID	: tmp_tree.m_id, $
		M_MERit	: tmp_tree.m_merit, $
		M_BID	: tmp_tree.m_bid, $
		M_SNAP 	: tmp_tree.m_snap, $
		D_SNAP	: [tmp_tree.snap(0), tmp_tree.d_snap], $
		D_ID	: [tmp_tree.id(0), tmp_tree.d_id], $
		ENDIND	: tmp_tree.endind + 1L, $
		NUMPROG	: tmp_tree.numprog}

	kval_new	= snapc + tree_key(0)*idc
	IF tree_key(kval_new) GE 0L THEN BEGIN
		PRINT, 'tree corrupted'
		STOP
	ENDIF
	tree_key(kval_new)	= tind
	PTR_FREE, complete_tree(tind)
	complete_tree(tind)	= PTR_NEW(tmp, /no_copy)
END
;;-----
;; BRANCH COMPARE
;;-----
FUNCTION p_ctree_brcompare, tree_set, s0, id0, slist, idlist
	pid0	= f_rdpid(s0, id0, horg=tree_set.horg, dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)
	pweight0= p_ctree_getweight(pid0)
	;pid1	= f_rdpid(s1, id1, horg=tree_set.horg, dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)

	IF N_ELEMENTS(slist) EQ 1L THEN BEGIN
		pid1	= f_rdpid(slist(0), idlist(0), horg=tree_set.horg, $
			 dir_catalog=tree_set.dir, dir_raw=tree_set.dir_raw, /silent)
		pweight1= p_ctree_getweight(pid1)
		factor	= 1.d / tree_set.n_step_bw
	ENDIF ELSE BEGIN
		cpid 	= p_ctree_collectpidonbranch(tree_set, slist, idlist)
		pid1 	= cpid.pid
		pweight1= cpid.weight
		factor	= cpid.n_con * 1.d / tree_set.n_step_bw
		;pid1 	= p_ctree_collectpidonbranch(tree_set, slist, idlist)
	ENDELSE
	;; factor is to be 1 if all pid1 exist in a galaxy during n_step_bw

	larr = LONARR(20) & darr = DBLARR(20)
	ftr_name	= tree_set.dir_root + '/src/sub/sub_ftn/get_merit.so'
		larr(0)	= N_ELEMENTS(pid0)
		larr(1)	= N_ELEMENTS(pid1)
		larr(2)	= tree_set.num_thread
		larr(3)	= 1L

	void	= CALL_EXTERNAL(ftr_name, 'get_merit', $
		larr, darr, pid0, pid1, pweight0, pweight1)


	RETURN, darr(0)*factor

END
;;-----
;; LINK BRANCH
;;-----
PRO p_ctree_linkbr, tree_set, data, ind, complete_tree, tree_key, idc, snapc, meritc, c_snap

	kval	= data(ind).snap + tree_key(0)*data(ind).id
	tind	= tree_key(kval)
	tmp_tree= *complete_tree(tind)
	IF tind LT 0L THEN BEGIN
		p_ctree_makenewbr, data(ind).snap, data(ind).id, complete_tree, tree_key
		kval	= data(ind).snap + tree_key(0)*data(ind).id
		tind	= tree_key(kval)
		tmp_tree= *complete_tree(tind)
	ENDIF

	kval2	= snapc + tree_key(0)*idc
	tind2	= tree_key(kval2)
	tmp_tree_toc	= *complete_tree(tind2)

	IF tmp_tree_toc.stat EQ 'MERGED' THEN BEGIN
		PRINT, 'BRANCH STOLEN'
		STOP
	ENDIF

	oldgalind	= -1L
	IF tmp_tree_toc.snap(-1) EQ tmp_tree.snap(-1) THEN BEGIN
		brorg_cut 		= WHERE(tmp_tree.snap GT snapc + tree_set.n_step_dn, nbr_org);[0]
		brcompare_cut	= WHERE(tmp_tree_toc.snap GT snapc + tree_set.n_step_dn, nbr_com);[0]
		IF nbr_org EQ 0L THEN BEGIN
			brorg_cut	= WHERE(tmp_tree.snap GT snapc, nbr_org)
		ENDIF

		IF nbr_com EQ 0L THEN BEGIN
			brcompare_cut	= WHERE(tmp_tree_toc.snap GT snapc, nbr_com)
		ENDIF

		merit_com	= p_ctree_brcompare(tree_set, snapc, idc, tmp_tree_toc.snap(brcompare_cut), tmp_tree_toc.id(brcompare_cut))
		merit_org 	= p_ctree_brcompare(tree_set, snapc, idc, tmp_tree.snap(brorg_cut), tmp_tree.ID(brorg_cut))

		IF merit_com GT merit_org THEN BEGIN	;; Existing tree is better
			data(ind).stat = 'B'
			RETURN
		ENDIF
		oldgalind	= WHERE(data.id EQ tmp_tree_toc.id(0) AND data.snap EQ tmp_tree_toc.snap(0), nold)
		IF nold NE 1L THEN STOP
	ENDIF

	;; Leave Old tree
	cut_toc	= WHERE(tmp_tree_toc.snap GT snapc, nnn)
	IF nnn GE 1L THEN BEGIN
		cut_toc_merge	= WHERE(tmp_tree_toc.m_snap GT snapc, nmerge)
		IF nmerge GE 1L THEN BEGIN
			mid	= tmp_tree_toc.m_id(cut_toc_merge)
			msnap	= tmp_tree_toc.m_snap(cut_toc_merge)
			mbid	= tmp_tree_toc.m_bid(cut_toc_merge)
			mmerit	= tmp_tree_toc.m_merit(cut_toc_merge)
		ENDIF ELSE BEGIN
			mid	= -1L
			msnap	= -1L
			mbid	= -1L
			mmerit	= -1L
		ENDELSE

		IF nnn EQ 0L THEN STOP
		old_tree	= {$
			ID		: tmp_tree_toc.id(cut_toc), $
			SNAP		: tmp_tree_toc.snap(cut_toc), $
			STAT		: 'main', $
			P_SNAP		: tmp_tree_toc.p_snap(cut_toc), $
			P_ID		: tmp_tree_toc.p_id(cut_toc), $
			P_MERIT		: tmp_tree_toc.p_merit(cut_toc), $
			M_ID		: mid, $
			M_MERIT		: mmerit, $
			M_BID		: mbid, $
			M_SNAP		: msnap, $
			D_SNAP		: tmp_tree_toc.d_snap(cut_toc), $
			D_ID		: tmp_tree_toc.d_id(cut_toc), $
			ENDIND		: nnn-1L, $
			NUMPROG		: nmerge}
	ENDIF ELSE BEGIN
		old_tree	= -1L
	ENDELSE

	cut_org	= WHERE(tmp_tree.snap GE data(ind).snap, norg)
	IF norg EQ 0L THEN STOP
	
	cut_toc	= WHERE(tmp_tree_toc.snap LE snapc, ntoc)
	IF ntoc EQ 0L THEN STOP

	;; LINK
	m_id	= [tmp_tree_toc.m_id, 		tmp_tree.m_id]
	m_merit	= [tmp_tree_toc.m_merit,	tmp_tree.m_merit]
	m_bid	= [tmp_tree_toc.m_bid, 		tmp_tree.m_bid]
	m_snap	= [tmp_tree_toc.m_snap,		tmp_tree.m_snap]
	mcut	= WHERE(m_id GE 0L, nm)
	IF nm EQ 0L THEN BEGIN
		m_id	= [-1L]
		m_merit	= [-1.0d]
		m_bid	= [-1L]
		m_snap	= [-1L]
	ENDIF ELSE BEGIN
		m_id	= m_id(mcut)
		m_merit	= m_merit(mcut)
		m_bid	= m_bid(mcut)
		m_snap	= m_snap(mcut)
		;; what if m_snap contains snapshots that are not in the tree due to pruning
	ENDELSE

	IF N_ELEMENTS(cut_org) EQ 1L THEN BEGIN
		p_snap	= [tmp_tree_toc.P_SNAP(cut_toc), snapc]
		p_id	= [tmp_tree_toc.P_ID(cut_toc), idc]
		p_merit	= [tmp_tree_toc.p_merit(cut_toc), meritc]
	ENDIF ELSE BEGIN
		p_snap	= [tmp_tree_toc.P_SNAP(cut_toc), snapc, tmp_tree.p_snap(cut_org(1L:*))]
		p_id	= [tmp_tree_toc.P_ID(cut_toc), idc, tmp_tree.p_id(cut_org(1L:*))]
		p_merit	= [tmp_tree_toc.p_merit(cut_toc), meritc, tmp_tree.p_merit(cut_org(1L:*))]
	ENDELSE

	IF N_ELEMENTS(cut_toc) EQ 1L THEN BEGIN
		d_snap	= [-1L, tmp_tree.snap(cut_org(0)), tmp_tree.d_snap(cut_org)]
		d_id	= [-1L, tmp_tree.id(cut_org(0)), tmp_tree.d_id(cut_org)]
	ENDIF ELSE BEGIN
		d_snap	= [tmp_tree_toc.d_snap(cut_toc(0L:-2L)), tmp_tree.snap(cut_org(0)), tmp_tree.d_snap(cut_org)]
		d_id	=  [tmp_tree_toc.d_id(cut_toc(0L:-2L)), tmp_tree.id(cut_org(0)), tmp_tree.d_id(cut_org)]
	ENDELSE

	tmp	= {$
		ID	: [tmp_tree_toc.id(cut_toc)	, tmp_tree.id(cut_org)], $
		SNAP	: [tmp_tree_toc.snap(cut_toc)	, tmp_tree.snap(cut_org)], $
		STAT	: 'main', $
		P_SNAP	: p_snap, $
		P_ID	: p_id ,$
		P_MERIT	: p_merit, $
		M_ID	: m_id, $
		M_MERIT	: m_merit, $
		M_BID	: m_bid, $
		M_SNAP	: m_snap, $
		D_SNAP	: d_snap, $
		D_ID	: d_id, $
		ENDIND	: N_ELEMENTS(cut_toc) + N_ELEMENTS(cut_org) - 1L, $
		NUMPROG	: tmp_tree_toc.numprog + tmp_tree.numprog}

	;; initialize
	key_vals_org	= tmp_tree.snap + tree_key(0)*tmp_tree.id
	tree_key(key_vals_org)	= -1L

	key_vals_new	= tmp_tree_toc.snap + tree_key(0)*tmp_tree_toc.id
	tree_key(key_vals_new)	= -1L

	key_vals	= tmp.snap + tree_key(0)*tmp.id
	tree_key(key_vals)	= tind
	PTR_FREE, complete_tree(tind)
	complete_tree(tind)	= PTR_NEW(tmp, /no_copy)

	IF TYPENAME(old_tree) NE 'LONG' THEN BEGIN
		key_vals_old	= old_tree.snap + old_tree.id*tree_key(0)
		tree_key(key_vals_old)	= tind2
		PTR_FREE, complete_tree(tind2)

		IF oldgalind GE 0L THEN BEGIN  
			p_ctree_free, data, oldgalind, old_tree.snap(0), old_tree.id(0), c_snap
		ENDIF

		complete_tree(tind2)	= PTR_NEW(old_tree, /no_copy)
	ENDIF
	;complete_tree(tind2)	= PTR_NEW(tmp, /no_copy)

END
;;-----
;; LINK
;;-----
PRO p_ctree_link, tree_set, data, number, c_snap, complete_tree, tree_key

	number	= {n_link:0L, n_broken:0L}

	snap_int_cut 	= MIN([c_snap - tree_set.snap_i, tree_set.n_search])
	cut	= WHERE(data.stat EQ 'C' AND data.list_n GE snap_int_cut, ncut)
	IF ncut EQ 0L THEN RETURN

	FOR i=0L, ncut-1L DO BEGIN
		ind	= cut(i)
		mlist	= data(ind).list.merit
		idlist	= data(ind).list.id
		slist	= data(ind).list.snap
		IF MAX(mlist) LT tree_set.merit_limit THEN BEGIN
			data(ind).stat = 'B'
			p_ctree_free, data, ind, -1L, -1L, c_snap
			CONTINUE
		ENDIF
		cut2	= (WHERE(mlist EQ MAX(mlist)))[0]

		id_tolink	= idlist(cut2)
		snap_tolink	= slist(cut2)
		merit_tolink	= MAX(mlist)
		;; Check Whether there is tree existed
		kval	= snap_tolink + tree_key(0)*id_tolink
		tind	= tree_key(kval)

		IF tind EQ -1L THEN BEGIN	;; notree
			p_ctree_expandbr, data, ind, complete_tree, tree_key, id_tolink, snap_tolink, merit_tolink
			IF data(ind).stat EQ 'B' THEN BEGIN
				p_ctree_free, data, ind, -1L, -1L, c_snap
			ENDIF ELSE BEGIN
				p_ctree_free, data, ind, snap_tolink, id_tolink, c_snap
			ENDELSE
		ENDIF ELSE BEGIN		;; Tree exist
			p_ctree_linkbr, tree_set, data, ind, complete_tree, tree_key, id_tolink, snap_tolink, merit_tolink, c_snap
			IF data(ind).stat EQ 'B' THEN BEGIN
				p_ctree_free, data, ind, -1L, -1L, c_snap
			ENDIF ELSE BEGIN
				tkey	= snap_tolink + tree_key(0)*id_tolink
				tind2	= tree_key(kval)
				tdum	= *complete_tree(tind2)
				p_ctree_free, data, ind, tdum.snap(0), tdum.id(0), c_snap
			ENDELSE
		ENDELSE
		;a = f_gettree(830L, 19L, complete_tree, tree_key)
		;a	= *a
		;PRINT, ind, a.snap(0)
		;IF ind EQ 13L THEN STOP
	ENDFOR
END
;;-----
;; FREE MEMORY
;;-----
PRO p_ctree_free, data, ind, s_end, id_end, c_snap

	data(ind).detstat	= -1L
	PTR_FREE, data(ind).p_list
	data(ind).n_ptcl	= -1L
	IF s_end LT 0L THEN BEGIN
		data(ind).list.merit	= -1.d
		data(ind).list.id	= -1L
		data(ind).list.snap	= -1L
		data(ind).list_n	= 0L
	ENDIF ELSE IF s_end GT c_snap THEN BEGIN
		data(ind).id		= id_end
		data(ind).snap		= s_end
		cut 	= WHERE(data(ind).list.snap LT s_end AND data(ind).list.snap GT 0L, ncut)
		IF ncut EQ 0L THEN BEGIN
			data(ind).list_n	= 0L
			data(ind).list.merit 	= -1.d
			data(ind).list.snap 	= -1L
			data(ind).list.id 		= -1L
		ENDIF ELSE BEGIN
			data(ind).list(0L:ncut-1L).merit 	= data(ind).list(cut).merit
			data(ind).list(0L:ncut-1L).id 		= data(ind).list(cut).id
			data(ind).list(0L:ncut-1L).snap 	= data(ind).list(cut).snap
			data(ind).list_n 					= ncut

			data(ind).list(ncut:-1L).merit 		= -1.d
			data(ind).list(ncut:-1L).id 		= -1L
			data(ind).list(ncut:-1L).snap 		= -1L
		ENDELSE
	ENDIF ELSE IF s_end LE c_snap AND s_end GT 0L THEN BEGIN
		data(ind).id		= id_end
		data(ind).snap		= s_end
		data(ind).list.merit= -1.d
		data(ind).list.id	= -1L
		data(ind).list.snap	= -1L
		data(ind).list_n	= 0L
	ENDIF
END
;;-----
;; MAIN
;;-----
PRO p_ctree, settings

	RESTORE, settings.dir_tree + 'tree.sav'

	;;-----
	;; Settings 
	;;-----

	tree_set	= {$
		num_thread:settings.num_thread, $
		snap_f:settings.p_ctree_snap(1), $
		snap_i:settings.p_ctree_snap(0), $
		horg:settings.horg, $
		tree_key:tree_key(0), $
		dir:settings.dir_catalog, $
		dir_raw:settings.dir_raw, $
		dir_root:settings.root_path, $
		rfact:settings.p_ctree_rfact, $
		merit_limit:settings.p_ctree_merit_limit, $
		n_step_bw:settings.p_ctree_n_step_n, $
		n_step_dn:settings.p_ctree_n_step_dn, $
		n_search:settings.p_ctree_n_search $
		}

	TIC & TOC

	;;-----
	;; Load Galaxies
	;;-----
	gal 	= f_rdgal(tree_set.snap_f, -1L, horg=tree_set.horg, column_list=['ID'], dir=settings.dir_catalog)
	
	;;-----
	;; ALLOCATE
	;;-----
	data 	= p_ctree_allocate(tree_set, gal, complete_tree, tree_key)

	;;-----
	;; GET SNAPLIST & Info
	;;-----
	tree_set 	= p_ctree_init(settings, tree_set)

	;;-----
	;; MAIN LOOP
	;;-----
	n_gal	= N_ELEMENTS(gal)
	n_tree 	= N_ELEMENTS(tree_set.slist)
	FOR i=n_tree-1L, 0L, -1L DO BEGIN

;IF tree_set.slist(i) GE 30L THEN CONTINUE
;IF tree_set.slist(i) EQ 29L THEN RESTORE, settings.dir_tree + 'ctree_0030.sav'

		c_snap 	= tree_set.slist(i)
		PRINT, '%123123-----'
		PRINT, ''
		PRINT, '		TREE CONNECTION AT SNAP = ' + STRING(c_snap,format='(I4.4)') + ' ( ' + STRING(N_ELEMENTS(data),format='(I4.4)') + ' gals )'

		;;-----
		;; LOAD GALAXIES WITH BROKEN TREES
		;;-----
		TIC
		p_ctree_classify, tree_set, data, c_snap, number
		TOC, elapsed_time=t_classify
		IF number.T EQ n_gal THEN BEGIN
			PRINT, '			SKIP due to all galaxies having trees'
			CONTINUE
		ENDIF

		;;-----
		;; Determine End point
		;;-----
		TIC
		p_ctree_detend, tree_set, data, complete_tree, tree_key
		TOC, elapsed_time=t_detend

		;;-----
		;; Collect galaxies for their merit to be computed
		;;-----
		TIC
		p_ctree_collectpid, tree_set, data, pid, complete_tree, tree_key
		TOC, elapsed_time=t_cpid

		;;-----
		;; Read particles at this snapshot
		;;-----
		TIC
		pid0	= p_ctree_readsnap(tree_set, data, c_snap)
		TOC, elapsed_time=t_rsnap

		;;-----
		;; Merit Calcultion
		;;-----
		TIC
		p_ctree_commerit, tree_set, data, pid, pid0, c_snap
		TOC, elapsed_time=t_merit

		;;-----
		;; LINK TREE
		;;-----
		TIC
		p_ctree_link, tree_set, data, number, c_snap, complete_tree, tree_key
		TOC, elapsed_time=t_link

		;;-----
		;; TODO
		;;-----

		; If tree is connected or renewed, set detstat to be -1
		;		free p_list and set n_ptcl to be negative
		;		free list and list_n

		PRINT, '		Time report [sec]'
		PRINT, '			Classify Galaxies :', t_classify
		PRINT, '			Det- Branch End   :', t_detend
		PRINT, '			Collect PID       :', t_cpid
		PRINT, '			Read Snap ptcls   :', t_rsnap
		PRINT, '			Compute Merits    :', t_merit
		PRINT, '			Link Branch	  :', t_link
		IF c_snap MOD 5L EQ 0L THEN BEGIN
			SAVE, FILENAME=settings.dir_tree + '/ctree_' + STRING(c_snap,format='(I4.4)') + '.sav', tree_set, data, c_snap, complete_tree, tree_key
		ENDIF
		IF c_snap LE tree_set.snap_i THEN BEGIN
			p_ctree_classify, tree_set, data, c_snap, number
			p_ctree_detend, tree_set, data, complete_tree, tree_key
			BREAK
		ENDIF
	ENDFOR

	SAVE, filename=settings.dir_tree + '/ctree.sav', complete_tree, tree_key
	SAVE, filename=settings.dir_tree + '/ctree_dat.sav', tree_set, data
	PRINT, 'Done ^-^'
	STOP
END
