Pro rv_save, settings, data, n_snap, run=run
;;-----
;; Check procedure set
;;-----
IF run EQ 0L THEN RETURN

	;;-----
	;; Make a Directory?
	;;----
	IF settings.horg EQ 'g' THEN dum = 'VR_Galaxy/'
	IF settings.horg EQ 'h' THEN dum = 'VR_Halo/'

	fname	= settings.dir_save + dum + 'snap_' + STRING(n_snap,format='(I4.4)')

	file_exist	= FILE_SEARCH(fname)
	IF STRLEN(file_exist) LE 5L THEN SPAWN, 'mkdir ' + fname

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
	for i=0L, ngal - 1L do begin
		IF (*data.rv_raw).mass_tot(i) LT 1e6 THEN CONTINUE
		ib = -1L & iu = -1L & ptcl_id = -1L
		IF N_ELEMENTS((*data.rv_id).b_ind) GE 2 THEN BEGIN
			ib = (*data.rv_id).b_ind(i,*) & iu = (*data.rv_id).u_ind(i,*)
			ptcl_id	= [(*data.rv_id).p_id(ib(0):ib(1)), $
				(*data.rv_id).p_id(iu(0):iu(1))]
		ENDIF


		;;----- Create a HDF5 file
		h5_name	= fname + '/GAL_' + $
			STRING((*data.rv_raw).id(i),format='(I6.6)') + '.hdf5'
		fid	= h5f_create(h5_name)

		;;----- Create Groups
		void	= h5g_create(fid, 'G_Prop')
		void	= h5g_create(fid, 'P_Prop')

		;----- Write Galaxy Properties
		for j=0L, N_ELEMENTS(settings.column_list)-1L do begin
			str	= 'tmp = [(*data.rv_raw).' + settings.column_list(j) + '(i)]'
			void	= execute(str)
			simple_write_hdf5, tmp, 'G_Prop/G_' + settings.column_list(j),	fid
		endfor

		ABMag	= -1.
		IF N_ELEMENTS((*data.rv_gprop).ABMAG) GE 2L THEN $
			ABMag = (*data.rv_gprop).ABMAG(i,*,*)
		simple_write_hdf5, ABMag, 'G_Prop/G_ABmag',	fid

		SFR = -1.
		IF N_ELEMENTS((*data.rv_gprop).SFR) GE 2L THEN $
			SFR = (*data.rv_gprop).SFR(i,*)
		simple_write_hdf5, SFR, 'G_Prop/G_SFR',		fid

		Progs = -1L
		IF N_ELEMENTS((*data.rv_tree).progs_merit) GE 2L THEN $
			Progs = (*data.rv_tree).progs_merit(i,*)
		simple_write_hdf5, progs, 'G_Prop/Progs', 		fid
		;;----- Wirte # of Ptcls
		;simple_write_hdf5, n_bdn, 'P_NumB', fid
		;simple_write_hdf5, n_ubd, 'P_NumU', fid

		;;----- Write Ptcl Properties
		;IF n_bdn + n_ubd ne data.npart(i) THEN stop

		simple_write_hdf5, ptcl_id,	'P_Prop/P_ID',		fid

		;;----- Write Other Information
		rate = -1.
		IF N_ELEMENTS((*data.rv_ptmatch).rate) GE 2L THEN $
			rate = (*data.rv_ptmatch).rate(i)
		simple_write_hdf5, rate, 'rate', fid

		simple_write_hdf5, (*data.rv_ptmatch).a_exp, 'Aexp', fid

		dom_list = -1L
		IF N_ELEMENTS((*data.rv_ptmatch).dom_list) GE 2L THEN $
			dom_list = (*data.rv_ptmatch).dom_list(i,*)
		simple_write_hdf5, dom_list, 'Domain_List', fid

		simple_write_hdf5, settings.flux_list, 'Flux_List', 	fid

		simple_write_hdf5, (*data.rv_gprop).SFR_R, 'SFR_R',		fid
		simple_write_hdf5, (*data.rv_gprop).SFR_T, 'SFR_T', 		fid
		simple_write_hdf5, (*data.rv_gprop).MAG_R, 'MAG_R', 		fid
		;;----- Close the HDF5 file
		h5f_close, fid

	endfor
	SPAWN, 'chmod 777 ' + STRTRIM(fname) + '/GAL_*.hdf5'
End

