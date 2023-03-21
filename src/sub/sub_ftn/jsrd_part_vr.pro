PRO jsrd_part_vr, part, dir2=dir2, snapnum=snapnum, swap=swap, icpu=icpu, ncpu=ncpu, num_thread=num_thread, $
	density=density, velocity=velocity, time=time, metal=metal, $
	silent=silent, domlist=domlist, l64ver=l64ver, newver=newver, $
	pointer=pointer, notime=notime, nometal=nometal

	;;-----
	;; GENERAL SETTINGS
	;;-----
	dir	= dir2
	IF ~KEYWORD_SET(domlist) THEN BEGIN
		domlist	= LINDGEN(ncpu) + icpu
	ENDIF ELSE BEGIN
		ncpu	= N_ELEMENTS(domlist)
	ENDELSE

	IF ~KEYWORD_SET(dir) THEN BEGIN
		PRINT, 'file location should be referred'
		IF ~KEYWORD_SET(silent) THEN DOC_LIBRARY, 'jsrd_part'
		RETURN
	ENDIF

	IF ~KEYWORD_SET(num_thread) THEN num_thread = 1L
	FINDPRO, 'jsrd_part_vr', dirlist=curr_dir, /noprint
	curr_dir	= curr_dir(0)

	IF ~KEYWORD_SET(snapnum) THEN BEGIN
		a	= STRPOS(dir, 'output_')
		snapnum	= STRMID(dir,a+7L, 5L)
		snapnum	= LONG(snapnum)
	ENDIF

	IF STRPOS(dir, 'output_') EQ -1L THEN BEGIN
		dir	= dir + 'output_' + STRING(snapnum,'(I5.5)') + '/'
	ENDIF

	file	= dir + '/part_' + STRING(snapnum,'(I5.5)') + '.out'
	;;-----
	;; READ HEADER
	;;-----
	ncpu_run = 0L & ndim = 0L & nstar = 0L & nsink = 0L & mstar = 0.0d
	fname	= file + STRING(domlist(0), format='(I5.5)')
	OPENR, 10, fname, /f77_unformatted, SWAP_ENDIAN=swap
	READU, 10, ncpu_run
	READU, 10, ndim
	READU, 10 & READU, 10
	READU, 10, nstar
	READU, 10 & READU, 10
	READU, 10, nsink
	CLOSE, 10

	IF ~KEYWORD_SET(silent) THEN BEGIN
		PRINT, 'nsnap	= ', snapnum
		PRINT, 'ncpu	= ', ncpu_run
		PRINT, 'ndim	= ', ndim
		IF nsink GT 0L THEN PRINT, 'nsink	= ', nsink
		IF nstar GT 0L THEN PRINT, 'nstar	= ', nstar
	ENDIF

	;;-----
	;; ALLOCATE MEMORY
	;;-----
	npart_tot	= 0L
	part_ind	= LONARR(ncpu)
		ftr_name	= curr_dir + '/jsrd_part_totnum.so'
			larr = LONARR(20) & darr = DBLARR(20)
			larr(0) = ncpu
			larr(2)	= num_thread
			larr(3) = STRLEN(file)

		void	= CALL_EXTERNAL(ftr_name, 'jsrd_part_totnum', $
			larr, darr, file, npart_tot, part_ind, domlist)

	;dblvar	= DBLARR(npart_tot,9)	;x, y, z, vx, vy, vz, mp, ap, zp
	;lonvar	= LONARR(npart_tot,3)	;fam, tag, domain
	xp = DBLARR(npart_tot,3) & vp = DBLARR(npart_tot,3)
	mp = DBLARR(npart_tot) & ap = DBLARR(npart_tot) & zp = DBLARR(npart_tot)
	fam = LONARR(npart_tot) & tag = LONARR(npart_tot) & domain = LONARR(npart_tot)
	idvar	= LON64ARR(npart_tot)	;ID

	;;-----
	;; READ PTCLS
	;;-----
		ftr_name	= curr_dir + '/jsrd_part.so'
			larr = lonarr(20) & darr = dblarr(20)
			larr(0) = ncpu
			;larr(1)	= icpu + ncpu - 1L
			larr(2) = num_thread
			larr(3) = STRLEN(file)
			larr(4) = npart_tot

			;skip time reading
			larr(16)	= 0L
			IF KEYWORD_SET(notime) THEN larr(16) = 20L

			;skip metal reading
			larr(17)	= 0L
			IF KEYWORD_SET(nometal) THEN larr(17) = 20L

			larr(18)= 0L
			IF KEYWORD_SET(newver) THEN larr(18) = 20L
			larr(19)= 0L
			IF KEYWORD_SET(l64ver) THEN larr(19)= 20L	

		void	= CALL_EXTERNAL(ftr_name, 'jsrd_part', $
			larr, darr, file, part_ind, xp, vp, mp, ap, zp, $
			fam, tag, domain, idvar, domlist)
			;larr, darr, file, part_ind, dblvar, lonvar, idvar, domlist)


	;;-----
	;; MERGE
	;;-----
	IF ~KEYWORD_SET(pointer) THEN BEGIN
		part	= {xp:xp, vp:vp, mp:mp, ap:ap, zp:zp, $
		id:idvar, family:fam, tag:tag, domain:domain}
	ENDIF ELSE BEGIN
		part	= {xp:PTR_NEW(xp,/no_copy), $
			vp:PTR_NEW(vp,/no_copy), $
			mp:PTR_NEW(mp,/no_copy), $
			ap:PTR_NEW(ap,/no_copy), $
			zp:PTR_NEW(zp,/no_copy), $
			id:PTR_NEW(idvar,/no_copy), $
			family:PTR_NEW(fam,/no_copy), $
			tag:PTR_NEW(tag,/no_copy), $
			domain:PTR_NEW(domain,/no_copy)}
	ENDELSE
	RETURN


END
