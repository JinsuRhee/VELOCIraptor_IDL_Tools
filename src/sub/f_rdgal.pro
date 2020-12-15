FUNCTION f_rdgal, settings, n_snap, Gprop, mrange=mrange, id=id, bound=bound, dir=dir

	IF ~KEYWORD_SET(dir) THEN $
		dir	= Settings.dir_save + $
		'VR_Galaxy/snap_' + string(n_snap,format='(I3.3)') + '/'

	flist	= file_search(dir + 'GAL_*')
	IF KEYWORD_SET(id) THEN flist = file_search(dir + 'GAL_' + STRING(id,format='(I6.6)') + '.hdf5')
	n_gal	= n_elements(flist)

	;;-----
	;; Mass Cut
	;;-----
	if keyword_set(mrange) then begin
		flist2	= file_search(dir + 'mlist.sav')
		if strlen(flist2) ge 5L then begin
			restore, dir + 'mlist.sav'
		endif else begin
			mdum	= dblarr(n_gal)
			for i=0L, n_gal - 1L do begin
				fid	= H5F_OPEN(flist(i))
				did	= H5D_OPEN(fid, '/G_Prop/G_Mass_tot')
				mdum(i)	= H5D_READ(did)
				H5D_close, did
				H5F_close, fid
			endfor
			IF ~KEYWORD_SET(id) THEN save, filename=dir + 'mlist.sav', mdum
		endelse
	endif else begin
		mrange	= fltarr(2)
		mrange(0) = -1.
		mrange(1) = 1e20
		mdum	= dblarr(n_gal) + 1.
	endelse

	;;-----
	;; Allocate Memory
	;;-----
	cut	= where(mdum ge mrange(0) and mdum le mrange(1))
	nn	= n_elements(cut)

	For ind=0L, n_elements(Gprop) - 1L DO BEGIN
		tmp	= Gprop(ind) + ' = dblarr(nn)'
		if Gprop(ind) eq 'ID' OR Gprop(ind) EQ 'ID_mbp' then $
		       tmp = Gprop(ind) + ' = lonarr(nn)'

		if Gprop(ind) eq 'ABmag' then $
		       tmp = Gprop(ind) + $
		       ' = dblarr(nn, n_elements(settings.flux_list), n_elements(settings.P_VRrun_mag_r))'

		IF Gprop(ind) eq 'SFR' then $
			tmp = Gprop(ind) + $
			' = dblarr(nn, n_elements(settings.P_VRrun_SFR_t))'

		void	= execute(tmp)
	EndFor

	;;-----
	;; Read Galaxies
	;;-----

	indG	= 0L
	For i=0L, n_gal - 1L Do Begin
		If(mdum(i) lt mrange(0)) Then Continue
		If(mdum(i) gt mrange(1)) Then Continue

		fid	= H5F_OPEN(flist(i))
		For j=0L, n_elements(Gprop) - 1L Do Begin
			did	= H5D_OPEN(fid, '/G_Prop/G_' + $
				strtrim(Gprop(j),2))

			tmp	= Gprop(j) + '(indG) = H5D_READ(did)'

			if Gprop(j) eq 'ABmag' then $
				tmp	= Gprop(j) + '(indG,*,*) = H5D_READ(did)'
			if Gprop(j) eq 'SFR' then $
				tmp	= Gprop(j) + '(indG,*) = H5D_READ(did)'

			void	= execute(tmp)
			H5D_close, did
		Endfor

		IF i EQ 0L THEN progs = LONARR(n_gal,10)
		did	= H5D_OPEN(fid, '/G_Prop/Progs')
		progs(i,*)	= H5D_READ(did)
		H5D_close, did

		did	= H5D_OPEN(fid, '/Domain_List')
		if i eq 0L then dlist = H5D_READ(did)
		if i eq 0L then n_mpi=n_elements(dlist)
		if i eq 0L then d_list=lonarr(n_gal,n_mpi)
		d_list(i,*) = H5D_READ(did)
		H5D_close, did

		IF i EQ 0L THEN rate = fltarr(n_gal)
		did	= H5D_OPEN(fid, '/rate')
		rate(i)	= H5D_READ(did)
		H5D_close, did

		IF i EQ 0L THEN BEGIN
			did	= H5D_open(fid, '/Flux_List')
			f_list	= H5D_READ(did)
			H5D_close, did

			did	= H5D_open(fid, '/Aexp')
			aexp	= H5D_READ(did)
			H5D_close, did
		ENDIF

		H5F_close, fid
		indG ++
	Endfor

	;;-----
	;; Create Structure
	;;-----

	For j=0L, n_elements(Gprop)-1L Do Begin
		if j eq 0L then $
			tmp	= 'GP = create_struct(Gprop(j), ' + Gprop(j) + ')'
		if j ge 1L then $
			tmp	= 'GP = create_struct(GP, Gprop(j), ' + Gprop(j) + ')'
		void	= execute(tmp)
	Endfor

	GP	= create_struct(GP, 'rate', rate, 'progs', progs, $
		'Aexp', mean(aexp), 'Domain_List', d_list, 'Flux_List', f_list, $
		'SFR_R', settings.P_VRrun_SFR_R, 'SFR_T', settings.P_VRrun_SFR_t, $
		'MAG_R', settings.P_VRrun_MAG_R)

	gal	= GP

	RETURN, gal

End
