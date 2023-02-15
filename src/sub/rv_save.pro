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

	fname	= settings.dir_catalog + dum + 'snap_' + STRING(n_snap,format='(I4.4)')

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
		;IF (*data.rv_raw).mass_tot(i) LT 1e6 THEN CONTINUE
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

IF settings.horg EQ 'g' THEN BEGIN
		ABMag	= -1.
		IF N_ELEMENTS((*data.rv_gprop).ABMAG) GE 2L THEN $
			ABMag = (*data.rv_gprop).ABMAG(i,*,*)
		simple_write_hdf5, ABMag, 'G_Prop/G_ABmag',	fid
	
		SFR = -1.
		IF N_ELEMENTS((*data.rv_gprop).SFR) GE 2L THEN $
			SFR = (*data.rv_gprop).SFR(i,*)
		simple_write_hdf5, SFR, 'G_Prop/G_SFR',		fid

		SFR = -1.
		IF N_ELEMENTS((*data.rv_gprop).SFR_clumpcorr) GE 2L THEN $
			SFR = (*data.rv_gprop).SFR_clumpcorr(i,*)
		simple_write_hdf5, SFR, 'G_Prop/G_SFR_clumpycorr',	fid
ENDIF
		CONFrac = -1.
		IF N_ELEMENTS((*data.rv_gprop).confrac) GE 2L THEN $
			Confrac = (*data.rv_gprop).CONFrac(i,*)
		simple_write_hdf5, CONfrac, 'G_Prop/G_ConFrac',	fid
		simple_write_hdf5, settings.CONF_R, 'CONF_R', 			fid
		;;----- Tree related
		;Progs = -1L
		;IF N_ELEMENTS((*data.rv_tree).prog_bymerit) GE 2L THEN $
		;	Progs = (*data.rv_tree).prog_bymerit(i,*)
		;simple_write_hdf5, progs, 'G_Prop/Prog_bymerit', 	fid

		;Progs = -1L
		;IF N_ELEMENTS((*data.rv_tree).prog_matsnap_merit) GE 2L THEN $
		;	Progs = (*data.rv_tree).prog_matsnap_merit(i,*)
		;simple_write_hdf5, progs, 'G_Prop/Prog_matchedsnapshot_merit', 	fid

		;Progs = -1L
		;IF N_ELEMENTS((*data.rv_tree).prog_bymass) GE 2L THEN $
		;	Progs = (*data.rv_tree).prog_bymass(i,*)
		;simple_write_hdf5, progs, 'G_Prop/Prog_bymass', 	fid

		;Progs = -1L
		;IF N_ELEMENTS((*data.rv_tree).prog_matsnap_mass) GE 2L THEN $
		;	Progs = (*data.rv_tree).prog_matsnap_mass(i,*)
		;simple_write_hdf5, progs, 'G_Prop/Prog_matchedsnapshot_mass', 	fid

		;Desc	= -1L
		;IF N_ELEMENTS((*data.rv_tree).desc_bymass) GE 2L THEN $
		;	Desc	= (*data.rv_tree).desc_bymass(i)
		;simple_write_hdf5, desc, 'G_Prop/Desc_bymass', fid
		;
		;Desc	= -1L
		;IF N_ELEMENTS((*data.rv_tree).desc_matsnap) GE 2L THEN $
		;	Desc	= (*data.rv_tree).desc_matsnap(i)
		;simple_write_hdf5, desc, 'G_Prop/Desc_matchedsnapshot', fid

		;Desc	= -1L
		;IF N_ELEMENTS((*data.rv_tree).desc_bymerit) GE 2L THEN $
		;	Desc	= (*data.rv_tree).desc_bymerit(i)
		;simple_write_hdf5, desc, 'G_Prop/Desc_bymerit', fid

		;;----- Wirte # of Ptcls
		;simple_write_hdf5, n_bdn, 'P_NumB', fid
		;simple_write_hdf5, n_ubd, 'P_NumU', fid

		;;----- Write Ptcl Properties
		;IF n_bdn + n_ubd ne data.npart(i) THEN stop

		simple_write_hdf5, ptcl_id,	'P_Prop/P_ID',		fid

		;;----- Write Other Information
IF settings.horg EQ 'g' THEN BEGIN
		simple_write_hdf5, settings.flux_list, 'Flux_List', 	fid

		simple_write_hdf5, (*data.rv_gprop).SFR_R, 'SFR_R',		fid
		simple_write_hdf5, (*data.rv_gprop).SFR_T, 'SFR_T', 		fid
		simple_write_hdf5, (*data.rv_gprop).MAG_R, 'MAG_R', 		fid
		;;-----Write clump data
		isclump	= -1L
		IF N_ELEMENTS((*data.rv_gprop).isclump) GE 2L THEN $
			isclump	= (*data.rv_gprop).isclump(i)
		simple_write_hdf5, isclump, 'isclump',	fid
ENDIF

		rate = -1.
		IF N_ELEMENTS((*data.rv_ptmatch).rate) GE 2L THEN $
			rate = (*data.rv_ptmatch).rate(i)
		simple_write_hdf5, rate, 'rate', fid

		simple_write_hdf5, (*data.rv_ptmatch).a_exp, 'Aexp', fid

		dom_list = -1L
		IF N_ELEMENTS((*data.rv_ptmatch).dom_list) GE 2L THEN $
			dom_list = (*data.rv_ptmatch).dom_list(i,*)
		simple_write_hdf5, dom_list, 'Domain_List', fid

		;;----- Close the HDF5 file
		h5f_close, fid

	endfor
	SPAWN, 'chmod 777 ' + STRTRIM(fname) + '/GAL_*.hdf5'
End

