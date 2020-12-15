PRO rv_ptmatch, output, output2, dir_snap=dir_snap, dir_raw=dir_raw, dir_lib=dir_lib, $
	horg=horg, num_thread=num_thread, n_snap=n_snap, longint=longint, skip=skip, yzics=yzics

	;;-----
	;; Skip Process
	;;-----
	IF skip EQ 1L THEN BEGIN
		output2	= {p_pos:-1, p_vel:-1, p_age:-1, p_met:-1, p_mass:-1, dom_list:-1, $
			rate:-1, a_exp:-1}
		RETURN
	ENDIF

	;;-----
	;; Read Simulation Info
	;;-----

	infoname	= dir_raw + 'output_' + string(n_snap,format='(I5.5)') + $
		'/info_' + string(n_snap,format='(I5.5)') + '.txt'
	rd_info, siminfo, file=infoname

	n_mpi	= n_elements(siminfo.hindex(*,0))

        xc      = dblarr(n_elements(output.id))
        yc      = dblarr(n_elements(output.id))
        zc      = dblarr(n_elements(output.id))
        rr      = dblarr(n_elements(output.id))
        for i=0L, n_elements(xc)-1L do xc(i) = output.xc(i)  * 3.086d21 / siminfo.unit_l
        for i=0L, n_elements(yc)-1L do yc(i) = output.yc(i)  * 3.086d21 / siminfo.unit_l
        for i=0L, n_elements(zc)-1L do zc(i) = output.zc(i)  * 3.086d21 / siminfo.unit_l
        for i=0L, n_elements(rr)-1L do rr(i) = output.r_halfmass(i)* 3.086d21 / siminfo.unit_l
	rr	= rr*0.0 + MEDIAN(rr)

	rate	= FLTARR(N_ELEMENTS(output.id))
	n_bdn   = output.b_ind(-1,-1)+1L
	n_ubd   = output.u_ind(-1,-1) - output.b_ind(-1,-1)
	n_part  = n_bdn + n_ubd

	pos_pt  = dblarr(n_part,3) - 1d8
	vel_pt  = pos_pt
	met_pt  = dblarr(n_part) - 1d8
	age_pt  = met_pt
	mp_pt   = met_pt

	dom_list	= lonarr(n_elements(xc),n_mpi) - 1L
	;;-----
	;; Matching Start
	;;-----
	n_nomatch	= N_ELEMENTS(WHERE(rate LT 0.8))
	match_cutval	= 0.9999
	N_itr = 0L & N_itrmax = 10L & dfact = 10.0

	dmp_mass	= 1./(4096.^3) * (siminfo.omega_m - siminfo.omega_b) / siminfo.omega_m
	IF KEYWORD_SET(yzics) THEN dmp_mass = 1./(2048.^3) * (siminfo.omega_m - siminfo.omega_b) / siminfo.omega_m
	REPEAT BEGIN
		cut	= WHERE(rate LT match_cutval)

		PRINT, "%123123 ----------"
		PRINT, "%     ", STRTRIM(N_itr+1,2) + ' th iterations'
		PRINT, "%     FOR", N_ELEMENTS(cut), " GALAXIES"
		PRINT, "%     USING ", dfact
		IF MAX(cut) LT 0L THEN N_itr = N_itrmax


		IF N_itr LT N_itrmax THEN BEGIN
			;;----- Allocate Mem
			xc2 = xc(cut) & yc2 = yc(cut) & zc2 = zc(cut) & rr2 = rr(cut)

			rate2	= rate(cut)
			ind_b2 = output.b_ind(cut,*) & ind_u2 = output.u_ind(cut,*)

			id_pt2	= lon64arr(1) + 0
			m0	= 0L
			FOR i2=0L, N_ELEMENTS(cut) - 1L DO BEGIN
				n0	= m0
				js_makearr, id_pt2, [output.p_id(ind_b2(i2,0):ind_b2(i2,1))], m0, $
				unitsize=1000000L, type='L64'
				ind_b2(i2,0) = n0 & ind_b2(i2,1) = m0 - 1L
			ENDFOR

			FOR i2=0L, N_ELEMENTS(cut) - 1L DO BEGIN
				n0	= m0
				js_makearr, id_pt2, [output.p_id(ind_u2(i2,0):ind_u2(i2,1))], m0, $
				unitsize=1000000L, type='L64'
				ind_u2(i2,0) = n0 & ind_u2(i2,1) = m0 - 1L
			ENDFOR
			id_pt2 = id_pt2(0L:m0-1L) & n_part2 = m0
			pos_pt2	= dblarr(n_part2,3) - 1.0d8
			vel_pt2 = dblarr(n_part2,3) - 1.0d8
			met_pt2 = dblarr(n_part2) - 1.0d8
			age_pt2 = met_pt2 & mp_pt2 = met_pt2


			;;----- Domain
			ftr_name	= dir_lib + 'sub_ftn/find_domain.so'
				dom_list2	= lonarr(n_elements(xc2),n_mpi) - 1L
				larr	= lonarr(20) & darr = dblarr(20)
				larr(0) = n_elements(xc2)
				larr(1) = n_mpi
				larr(2)	= num_thread

				IF dfact GE 100. THEN $
					darr(0) = dfact / (max(rr2) * siminfo.unit_l / 3.086d21)
					;; Search in Physical Radius

				darr(0) = dfact		;; Radius factor

			void	= call_external(ftr_name, 'find_domain', $
				xc2, yc2, zc2, rr2, siminfo.hindex, siminfo.levmax, $
				dom_list2, larr, darr)

			;;----- Matching
			ftr_name	= dir_lib + 'sub_ftn/rv_match.so'
			lset = lonarr(20) & dset = dblarr(20)
				lset(0) = n_elements(id_pt2)	;; # of particles in VR Data
				lset(1) = n_elements(xc2)	;; # of Gals
				lset(2) = num_thread
				lset(3) = 10L			;; # of Doamins in a set
				lset(4) = n_mpi
				lset(5) = n_snap
				lset(10) = strlen(dir_raw)
				IF yzics EQ 1L then lset(18) = 100 ;; For YZiCS ver
				IF longint EQ 1L then lset(19) = 100 ;; For logn int ID

				dset(0) = dmp_mass
			if horg eq 'g' then dset(1) = 1.
			if horg eq 'h' then dset(1) = -1.
	
			void = call_external(ftr_name, 'rv_match', $
				lset, dset, dir_raw, $
				id_pt2, ind_b2, ind_u2, pos_pt2, vel_pt2, $
				met_pt2, age_pt2, mp_pt2, rate2, $
				dom_list2)
		
			;;----- Extract
			FOR i2=0L, N_ELEMENTS(cut) - 1L DO BEGIN
				IF N_itr LT N_itrmax - 1L THEN $
					IF rate2(i2) LT match_cutval THEN CONTINUE

				rate(cut(i2)) = rate2(i2)

				pos_pt(output.b_ind(cut(i2),0):output.b_ind(cut(i2),1),*) = $
					pos_pt2(ind_b2(i2,0):ind_b2(i2,1),*)
				vel_pt(output.b_ind(cut(i2),0):output.b_ind(cut(i2),1),*) = $
					vel_pt2(ind_b2(i2,0):ind_b2(i2,1),*)
				met_pt(output.b_ind(cut(i2),0):output.b_ind(cut(i2),1)) = $
					met_pt2(ind_b2(i2,0):ind_b2(i2,1))
				age_pt(output.b_ind(cut(i2),0):output.b_ind(cut(i2),1)) = $
					age_pt2(ind_b2(i2,0):ind_b2(i2,1))
				mp_pt(output.b_ind(cut(i2),0):output.b_ind(cut(i2),1)) = $
					mp_pt2(ind_b2(i2,0):ind_b2(i2,1))

				pos_pt(output.u_ind(cut(i2),0):output.u_ind(cut(i2),1),*) = $
					pos_pt2(ind_u2(i2,0):ind_u2(i2,1),*)
				vel_pt(output.u_ind(cut(i2),0):output.u_ind(cut(i2),1),*) = $
					vel_pt2(ind_u2(i2,0):ind_u2(i2,1),*)
				met_pt(output.u_ind(cut(i2),0):output.u_ind(cut(i2),1)) = $
					met_pt2(ind_u2(i2,0):ind_u2(i2,1))
				age_pt(output.u_ind(cut(i2),0):output.u_ind(cut(i2),1)) = $
					age_pt2(ind_u2(i2,0):ind_u2(i2,1))
				mp_pt(output.u_ind(cut(i2),0):output.u_ind(cut(i2),1)) = $
					mp_pt2(ind_u2(i2,0):ind_u2(i2,1))

				dom_list(cut(i2),*) = dom_list2(i2,*)
			ENDFOR

			N_itr ++
			IF dfact LT 20.0 THEN BEGIN
				dfact = dfact + 40.0
			ENDIF ELSE BEGIN
				dfact = dfact + 50.0
			ENDELSE
		ENDIF

		PRINT, "%     Done. Left Galaxies are ", N_ELEMENTS(WHERE(rate LT match_cutval))
		PRINT, "%123123 ----------"
	ENDREP UNTIL N_itr GE N_itrmax

	pos_pt	= pos_pt * siminfo.unit_l / 3.086d21
	vel_pt	= vel_pt * siminfo.kms
	mp_pt	= mp_pt * siminfo.unit_m / 1.98892e33

	output2	= create_struct('p_pos', pos_pt, $
		'p_vel', vel_pt, 'p_age', age_pt, $
		'p_met', met_pt, 'p_mass', mp_pt, $
		'dom_list', dom_list, 'rate', rate, $
		'a_exp', siminfo.aexp)

	return
End
