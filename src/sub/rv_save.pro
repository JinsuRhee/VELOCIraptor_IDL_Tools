Pro rv_save, settings, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN

	;;-----
	;; Make a Directory?
	;;----
	IF settings.horg EQ 'g' THEN dum = 'Galaxy/VR_Galaxy/'
	IF settings.horg EQ 'h' THEN dum = 'Halo/VR_Halo/'

	fname	= settings.dir_catalog + dum + 'snap_' + STRING(n_snap,format='(I4.4)') + '.hdf5'

	;file_exist	= FILE_SEARCH(fname)
	;IF STRLEN(file_exist) LE 5L THEN SPAWN, 'mkdir ' + fname

	;;-----
	;; CALL POINTER FIRST
	;;-----
	rv_raw	= *data.rv_raw
	rv_id	= *data.rv_id
	rv_ptmatch 	= *data.rv_ptmatch
	rv_gprop	= *data.rv_gprop	

	;;-----
	;; Compute skip process first
	;;-----
	is_pextskip	= N_ELEMENTS(rv_id.b_ind)
	IF settings.horg EQ 'g' THEN is_ABMAGskip = N_ELEMENTS(rv_gprop.ABMAG)
	IF settings.horg EQ 'g' THEN is_SFRskip = N_ELEMENTS(rv_gprop.SFR)
	IF settings.horg EQ 'g' THEN is_SFRcorrskip = N_ELEMENTS(rv_gprop.SFR_clumpcorr)
	is_confmskip 	= N_ELEMENTS(rv_gprop.confrac_m)
	is_confnskip 	= N_ELEMENTS(rv_gprop.confrac_n)
	IF settings.horg EQ 'g' THEN is_clumpskip = N_ELEMENTS(rv_gprop.isclump)
	is_rateskip	= N_ELEMENTS(rv_ptmatch.rate)
	is_domskip	= N_ELEMENTS(rv_ptmatch.dom_list)
	;;	----- PTCLs
	;;		POS: in Kpc
	;;		VEL: in km/s
	;;		AGE: in conformal
	;;		SF: in scale factor
	;;		GYR: in Gyr
	;;		Mass: in Solar mass

	ngal	= N_ELEMENTS(rv_raw.id)
	
	;;-----
	;; Open HDF5
	;;-----
	fid	= h5f_create(fname)

	;;----- Write General Information
	simple_write_hdf5, settings.flux_list, 'Flux_List', 	fid

	simple_write_hdf5, settings.SFR_R, 'SFR_R',		fid
	simple_write_hdf5, settings.SFR_T, 'SFR_T', 		fid
	simple_write_hdf5, settings.MAG_R, 'MAG_R', 		fid
	simple_write_hdf5, settings.CONF_R, 'CONF_R', 			fid

	;;----- Write some bulk properties
	simple_write_hdf5, rv_raw.mass_tot, 'Mass_tot', fid
	simple_write_hdf5, rv_raw.mvir, 'Mvir', fid
	simple_write_hdf5, rv_raw.id, 'ID', fid

	for i=0L, ngal - 1L do begin

		ib = -1L & iu = -1L & ptcl_id = -1L
		IF is_pextskip GE 2 THEN BEGIN
			ib = rv_id.b_ind(i,*) & iu = rv_id.u_ind(i,*)
			ptcl_id	= [rv_id.p_id(ib(0):ib(1)), $
				rv_id.p_id(iu(0):iu(1))]

			cut	= WHERE(ptcl_id GT -922337203685477580LL, ncut)
			IF ncut NE rv_raw.npart(i) THEN BEGIN
				PRINT, 'WRONG PTCL ID ARRAY'
				STOP
			ENDIF
			ptcl_id	= ptcl_id(cut)
		ENDIF

		;;----- Create Groups for this galaxy
		idstr	= 'ID_' + STRING(rv_raw.id(i), format='(I6.6)')
		gpstr	= idstr + '/G_Prop'
		ppstr	= idstr + '/P_Prop'

		void	= h5g_create(fid, idstr) 
		void	= h5g_create(fid, gpstr)
		void	= h5g_create(fid, ppstr)

		;;----- Write Galaxy Properties
		FOR j=0L, N_ELEMENTS(settings.column_list)-1L DO BEGIN
			str	= 'tmp = [rv_raw.' + settings.column_list(j) + '(i)]'
			void	= EXECUTE(str)
			simple_write_hdf5, tmp, + gpstr + '/G_' + settings.column_list(j),	fid
		ENDFOR

		ABMag	= -1.
		IF settings.horg EQ 'g' THEN BEGIN
			IF is_ABMAGskip GE 2L THEN $
				ABMag = rv_gprop.ABMAG(i,*,*)
		ENDIF
		simple_write_hdf5, ABMag, gpstr + '/G_ABmag',	fid
	
		SFR = -1.
		IF settings.horg EQ 'g' THEN BEGIN
			IF is_SFRskip GE 2L THEN $
				SFR = rv_gprop.SFR(i,*)
		ENDIF
		simple_write_hdf5, SFR, gpstr + '/G_SFR',		fid

		SFR = -1.
		IF settings.horg EQ 'g' THEN BEGIN
			IF is_SFRcorrskip GE 2L THEN $
				SFR = rv_gprop.SFR_clumpcorr(i,*)
		ENDIF
		simple_write_hdf5, SFR, gpstr + '/G_SFR_clumpycorr',	fid

		CONFrac = -1.
		IF is_confmskip GE 2L THEN $
			Confrac = rv_gprop.CONFrac_m(i,*)
		simple_write_hdf5, CONfrac, gpstr + '/G_ConFrac_M',	fid

		CONFrac = -1.
		IF is_confnskip GE 2L THEN $
			Confrac = rv_gprop.CONFrac_n(i,*)
		simple_write_hdf5, CONfrac, gpstr + '/G_ConFrac_n',	fid
		
		;;----- Particle ID
		simple_write_hdf5, ptcl_id,	ppstr + '/P_ID',		fid

		;;-----Write Galaxy general properties
		isclump	= -1L
		IF settings.horg EQ 'g' THEN BEGIN
			IF is_clumpskip GE 2L THEN $
				isclump	= rv_gprop.isclump(i)
		ENDIF
		simple_write_hdf5, isclump, idstr + '/isclump',	fid


		rate = -1.
		IF is_rateskip GE 2L THEN $
			rate = rv_ptmatch.rate(i)
		simple_write_hdf5, rate, idstr + '/rate', fid

		simple_write_hdf5, rv_ptmatch.a_exp, idstr + '/Aexp', fid

		dom_list = -1L
		IF is_domskip GE 2L THEN $
			dom_list = rv_ptmatch.dom_list(i,*)
		simple_write_hdf5, dom_list, idstr + '/Domain_List', fid
	endfor
	h5f_close, fid
	;SPAWN, 'chmod 777 ' + STRTRIM(fname) + '/GAL_*.hdf5'
End

