PRO rv_readid, output, dir_snap=dir_snap, horg=horg, skip=skip

	;;-----
	;; SKIP PROCESS
	;;-----
	IF skip EQ 1 THEN BEGIN
		output = {p_id:-1, b_ind:-1, u_ind:-1}
		RETURN
	ENDIF

	;;-----
	;; I/O Settings
	;;-----

	if horg eq 'h' then pref = 'halo'
	if horg eq 'g' then pref = 'galaxy'

	dum_fname_pt	= dir_snap + strtrim(pref,2) + '.dat.catalog_groups.*'
	dum_fname_ptb	= dir_snap + strtrim(pref,2) + '.dat.catalog_particles.*'
	dum_fname_ptu	= dir_snap + strtrim(pref,2) + '.dat.catalog_particles.unbound.*'

	dum_fname	= file_search(dum_fname_pt)
	dum_fname_bdn	= file_search(dum_fname_ptb)
	dum_fname_ubd	= file_search(dum_fname_ptu)

	;;-----
	;; Read Ptcl ID Info
	;;-----

        str     = ' '
        openr, 10, dum_fname(0)
        readf, 10, str
        str2    = strsplit(str, ' ', /extract)
        n_mpi = long(str2(1))
        close, 10

        n_obj   = lonarr(n_mpi)
        for i=0L, n_mpi-1L do begin
                openr, 10, dum_fname(i)
                readf, 10, str & readf, 10, str
                str2    = strsplit(str, ' ', /extract)
                n_obj(i)= long(str2(0))
                if i eq 0L then n_tot = long(str2(1))
                close, 10
        endfor

        n_part  = lonarr(n_tot,4)
        i0      = 0L
        for i=0L, n_mpi-1L do begin
                if n_obj(i) eq 0L then continue
                openr, 10, dum_fname(i)
                readf, 10, str & readf, 10, str

                n_part(i0:i0 + n_obj(i)-1, 0) = i               ;; NUM MPI
                for j=0L, n_obj(i)-1L do begin
                        readf, 10, str
                        n_part(i0 + j,1) = long(str)            ;; # of ALL PTCLS
                endfor
                for j=0L, n_obj(i)-1L do begin
                        readf, 10, str
                        n_part(i0 + j,2) = long(str)            ;; BDN INDICES
                endfor
                for j=0L, n_obj(i)-1L do begin
                        readf, 10, str
                        n_part(i0 + j,3) = long(str)            ;; UBD INDICES
                endfor

                i0 = i0 + n_obj(i)
                close, 10
        endfor

	i0 = 0L & j0 = 0L & k0 = 0L
	l0 = 0L & m0 = 0L

        for i=0L, n_mpi-1L do begin
                openr, 10, dum_fname_bdn(i)
                openr, 11, dum_fname_ubd(i)
                readf, 10, str & readf, 10, str
                if i eq 0L then begin
                        str2    = strsplit(str, ' ', /extract)
                        n_bdn_tot= long(str2(1))
                endif
                readf, 11, str & readf, 11, str
                if i eq 0L then begin
                        str2    = strsplit(str, ' ', /extract)
                        n_ubd_tot= long(str2(1))
                endif

                if i eq 0L then begin
                        id_bdn  = lon64arr(n_bdn_tot + 1L)
                        id_ubd  = lon64arr(n_ubd_tot + 1L)
                        bdn_ind = lonarr(n_tot,2)
                        ubd_ind = lonarr(n_tot,2)
                endif

                for j=0L, n_obj(i)-1L do begin
                        if j ne n_obj(i) -1L then begin
                                n_tot = n_part(i0 + j,1)
                                n_bnd = n_part(i0 + j + 1,2) - n_part(i0 + j,2)
                                n_ubd = n_part(i0 + j + 1,3) - n_part(i0 + j,3)
                        endif else begin
                                n_tot = n_part(i0 + j,1)
                                n_bnd = file_lines(dum_fname_bdn(i)) - 2L - n_part(i0 + j,2)
                                n_ubd = file_lines(dum_fname_ubd(i)) - 2L - n_part(i0 + j,3)
                        endelse

                        if n_bnd ge 1L then begin
                                bdn_ind(i0 + j,0) = j0
				id_bdn_dum	= lon64arr(n_bnd)
                                for k=0L, n_bnd-1L do begin
                                        readf, 10, str
					id_bdn_dum(k) = long64(str)
                                endfor
				js_makearr, id_bdn, id_bdn_dum, j0, unitsize=100000L, type='L64'
                                bdn_ind(i0 + j,1) = j0 - 1L
                        endif else begin
                                bdn_ind(i0 + j,0) = j0
                                bdn_ind(i0 + j,1) = j0

				IF j0 EQ 0L THEN id_bdn(0) = -1L
				IF j0 GE 1L THEN js_makearr, id_bdn, [-1], j0, unitsize=100000L, type='L64'
                        endelse

                        if n_ubd ge 1L then begin
                                ubd_ind(i0 + j,0) = k0
				id_ubd_dum	= lon64arr(n_ubd)
                                for k=0L, n_ubd-1L do begin
                                        readf, 11, str
					id_ubd_dum(k) = LONG64(str)
                                endfor
				js_makearr, id_ubd, id_ubd_dum, k0, unitsize=100000L, type='L64'
                                ubd_ind(i0 + j,1) = k0 - 1L
                        endif else begin
                                ubd_ind(i0 + j,0) = k0
                                ubd_ind(i0 + j,1) = k0
				IF k0 EQ 0L THEN id_ubd(0) = -1L
				IF k0 GE 1L THEN js_makearr, id_ubd, [-1], k0, unitsize=100000L, type='L64'	;;;
                        endelse
                endfor
  
                close, 10 & close, 11
                i0 = i0 + n_obj(i)
        endfor
 
        id_bdn = id_bdn(0L:j0-1L) & id_ubd = id_ubd(0L:k0-1L)	
	output  = create_struct('p_id', [id_bdn, id_ubd])
	output  = create_struct(output, 'b_ind', bdn_ind, 'u_ind', ubd_ind + n_elements(id_bdn))

	RETURN

End
