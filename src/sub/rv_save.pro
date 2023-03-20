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
	;; Create HDF
	;;-----

	;;	----- PTCLs
	;;		POS: in Kpc
	;;		VEL: in km/s
	;;		AGE: in conformal
	;;		SF: in scale factor
	;;		GYR: in Gyr
	;;		Mass: in Solar mass

	ngal	= N_ELEMENTS((*data.rv_raw).id)
	
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
	simple_write_hdf5, (*data.rv_raw).mass_tot, 'Mass_tot', fid
	simple_write_hdf5, (*data.rv_raw).mvir, 'Mvir', fid
	simple_write_hdf5, (*data.rv_raw).id, 'ID', fid

	for i=0L, ngal - 1L do begin
		ib = -1L & iu = -1L & ptcl_id = -1L
		IF N_ELEMENTS((*data.rv_id).b_ind) GE 2 THEN BEGIN
			ib = (*data.rv_id).b_ind(i,*) & iu = (*data.rv_id).u_ind(i,*)
			ptcl_id	= [(*data.rv_id).p_id(ib(0):ib(1)), $
				(*data.rv_id).p_id(iu(0):iu(1))]

			cut	= WHERE(ptcl_id GT -922337203685477580LL, ncut)
			IF ncut NE (*data.rv_raw).npart(i) THEN BEGIN
				PRINT, 'WRONG PTCL ID ARRAY'
				STOP
			ENDIF
			ptcl_id	= ptcl_id(cut)
		ENDIF

		;;----- Create Groups for this galaxy
		idstr	= 'ID_' + STRING((*data.rv_raw).id(i), format='(I6.6)')
		gpstr	= idstr + '/G_Prop'
		ppstr	= idstr + '/P_Prop'

		void	= h5g_create(fid, idstr) 
		void	= h5g_create(fid, gpstr)
		void	= h5g_create(fid, ppstr)

		;----- Write Galaxy Properties
		for j=0L, N_ELEMENTS(settings.column_list)-1L do begin
			str	= 'tmp = [(*data.rv_raw).' + settings.column_list(j) + '(i)]'
			void	= execute(str)
			simple_write_hdf5, tmp, + gpstr + '/G_' + settings.column_list(j),	fid
		endfor

		ABMag	= -1.
		IF settings.horg EQ 'g' THEN BEGIN
			IF N_ELEMENTS((*data.rv_gprop).ABMAG) GE 2L THEN $
				ABMag = (*data.rv_gprop).ABMAG(i,*,*)
		ENDIF
		simple_write_hdf5, ABMag, gpstr + '/G_ABmag',	fid
	
		SFR = -1.
		IF settings.horg EQ 'g' THEN BEGIN
			IF N_ELEMENTS((*data.rv_gprop).SFR) GE 2L THEN $
				SFR = (*data.rv_gprop).SFR(i,*)
		ENDIF
		simple_write_hdf5, SFR, gpstr + '/G_SFR',		fid

		SFR = -1.
		IF settings.horg EQ 'g' THEN BEGIN
			IF N_ELEMENTS((*data.rv_gprop).SFR_clumpcorr) GE 2L THEN $
				SFR = (*data.rv_gprop).SFR_clumpcorr(i,*)
		ENDIF
		simple_write_hdf5, SFR, gpstr + '/G_SFR_clumpycorr',	fid

		CONFrac = -1.
		IF N_ELEMENTS((*data.rv_gprop).confrac_m) GE 2L THEN $
			Confrac = (*data.rv_gprop).CONFrac_m(i,*)
		simple_write_hdf5, CONfrac, gpstr + '/G_ConFrac_M',	fid

		CONFrac = -1.
		IF N_ELEMENTS((*data.rv_gprop).confrac_n) GE 2L THEN $
			Confrac = (*data.rv_gprop).CONFrac_n(i,*)
		simple_write_hdf5, CONfrac, gpstr + '/G_ConFrac_n',	fid

		;;----- Particle ID
		simple_write_hdf5, ptcl_id,	ppstr + '/P_ID',		fid

		;;-----Write Galaxy general properties
		isclump	= -1L
		IF settings.horg EQ 'g' THEN BEGIN
			IF N_ELEMENTS((*data.rv_gprop).isclump) GE 2L THEN $
				isclump	= (*data.rv_gprop).isclump(i)
		ENDIF
		simple_write_hdf5, isclump, idstr + '/isclump',	fid


		rate = -1.
		IF N_ELEMENTS((*data.rv_ptmatch).rate) GE 2L THEN $
			rate = (*data.rv_ptmatch).rate(i)
		simple_write_hdf5, rate, idstr + '/rate', fid

		simple_write_hdf5, (*data.rv_ptmatch).a_exp, idstr + '/Aexp', fid

		dom_list = -1L
		IF N_ELEMENTS((*data.rv_ptmatch).dom_list) GE 2L THEN $
			dom_list = (*data.rv_ptmatch).dom_list(i,*)
		simple_write_hdf5, dom_list, idstr + '/Domain_List', fid

	endfor
	h5f_close, fid
	;SPAWN, 'chmod 777 ' + STRTRIM(fname) + '/GAL_*.hdf5'
End

