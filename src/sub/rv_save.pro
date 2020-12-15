Pro rv_save, output, dir_save=dir_save, horg=horg, num_thread=num_thread, $
	n_snap=n_snap, column_list=column_list, flux_list=flux_list, skip=skip

	;;-----
	;; Skip Process
	;;-----
	IF skip EQ 1L THEN RETURN

	;;-----
	;; Make a Directory?
	;;----

	if horg eq 'g' then fname = dir_save + 'VR_Galaxy/' + 'snap_' + string(n_snap,format='(I3.3)')
	if horg eq 'h' then fname = dir_save + 'VR_Halo/' + 'snap_' + string(n_snap,format='(I3.3)')

	file_exist	= file_search(fname)
	if strlen(file_exist) le 5L then spawn, 'mkdir ' + fname

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

	ngal	= n_elements(output.id)
	for i=0L, ngal - 1L do begin
		if output.mass_tot(i) lt 1e6 then continue
		ib = -1L & iu = -1L & ptcl_id = -1L
		IF N_ELEMENTS(output.b_ind) GE 2 THEN BEGIN
			ib = output.b_ind(i,*) & iu = output.u_ind(i,*)
			ptcl_id	= [output.p_id(ib(0):ib(1)), output.p_id(iu(0):iu(1))]
		ENDIF
		;ptcl_pos= [output.p_pos(ib(0):ib(1),*), output.p_pos(iu(0):iu(1),*)]
		;ptcl_vel= [output.p_vel(ib(0):ib(1),*), output.p_vel(iu(0):iu(1),*)]
		;ptcl_age= [output.p_age(ib(0):ib(1)), output.p_age(iu(0):iu(1))]
		;ptcl_met= [output.p_met(ib(0):ib(1)), output.p_met(iu(0):iu(1))]
		;ptcl_mp	= [output.p_mass(ib(0):ib(1)), output.p_mass(iu(0):iu(1))]
		;ptcl_sf	= [output.sf(ib(0):ib(1)), output.sf(iu(0):iu(1))]
		;ptcl_gyr= [output.gyr(ib(0):ib(1)), output.gyr(iu(0):iu(1))]
		;ptcl_fl	= [output.flux(ib(0):ib(1),*), output.flux(iu(0):iu(1),*)]


		;;----- Create a HDF5 file
		h5_name	= fname + '/GAL_' + string(output.id(i),format='(I6.6)') + '.hdf5'
		fid	= h5f_create(h5_name)

		;;----- Create Groups
		void	= h5g_create(fid, 'G_Prop')
		void	= h5g_create(fid, 'P_Prop')

		;----- Write Galaxy Properties
		for j=0L, n_elements(column_list)-1L do begin
			str	= 'tmp = [output.' + column_list(j) + '(i)]'
			void	= execute(str)
			simple_write_hdf5, tmp, 'G_Prop/G_' + column_list(j),	fid
		endfor

		ABMag	= -1.
		IF N_ELEMENTS(output.ABMAG) GE 2L THEN ABMag = output.ABMAG(i,*,*)
		simple_write_hdf5, ABMag, 'G_Prop/G_ABmag',	fid

		SFR = -1.
		IF N_ELEMENTS(output.SFR) GE 2L THEN SFR = output.SFR(i,*)
		simple_write_hdf5, SFR, 'G_Prop/G_SFR',		fid

		Progs = -1L
		IF N_ELEMENTS(output.progs) GE 2L THEN Progs = output.progs(i,*)
		simple_write_hdf5, progs, 'G_Prop/Progs', 		fid
		;;----- Wirte # of Ptcls
		;simple_write_hdf5, n_bdn, 'P_NumB', fid
		;simple_write_hdf5, n_ubd, 'P_NumU', fid

		;;----- Write Ptcl Properties
		;if n_bdn + n_ubd ne data.npart(i) then stop

		simple_write_hdf5, ptcl_id,	'P_Prop/P_ID',		fid
		;simple_write_hdf5, ptcl_pos,	'P_Prop/P_Pos',		fid
		;simple_write_hdf5, ptcl_vel,	'P_Prop/P_Vel',		fid
		;simple_write_hdf5, ptcl_gyr,	'P_Prop/P_Age',		fid
                ;simple_write_hdf5, ptcl_sf,	'P_Prop/P_Sfact',	fid
		;simple_write_hdf5, ptcl_met,	'P_Prop/P_Metal',	fid
		;simple_write_hdf5, ptcl_mp,	'P_Prop/P_Mass',	fid
                ;simple_write_hdf5, ptcl_fl,	'P_Prop/P_Flux',	fid
                ;simple_write_hdf5, ib(1)-ib(0)+1, 'P_Prop/P_Nbdn',	fid
                ;simple_write_hdf5, iu(1)-iu(0)+1, 'P_Prop/P_Nubd',	fid

		;;----- Write Other Information
		rate = -1.
		IF N_ELEMENTS(output.rate) GE 2L THEN rate = output.rate(i)
		simple_write_hdf5, rate, 'rate', fid

		simple_write_hdf5, output.a_exp, 'Aexp', fid

		dom_list = -1L
		IF N_ELEMENTS(output.dom_list) GE 2L THEN dom_list = output.dom_list(i,*)
		simple_write_hdf5, dom_list, 'Domain_List', fid

		simple_write_hdf5, flux_list, 'Flux_List', 	fid

		simple_write_hdf5, output.SFR_R, 'SFR_R',		fid
		simple_write_hdf5, output.SFR_T, 'SFR_T', 		fid
		simple_write_hdf5, output.MAG_R, 'MAG_R', 		fid
		;;----- Close the HDF5 file
		h5f_close, fid

	endfor
	spawn, 'chmod 777 ' + STRTRIM(fname) + '/GAL_*.hdf5'
End

