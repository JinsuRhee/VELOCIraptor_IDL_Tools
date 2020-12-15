FUNCTION get_mag, xc, yc, zc, rr, bind, uind, pos, met, gyr, mass, $
	MAG_R=MAG_R, flux_list=flux_list, lib=lib, num_thread=num_thread

	;;-----
	;; Settings
	;;-----
	n_gal	= n_elements(xc)
	n_aper	= n_elements(MAG_R)
	n_part	= n_elements(pos(*,0))
	n_flux	= n_elements(flux_list)
	IF ~KEYWORD_SET(num_thread) THEN spawn, 'nproc --all', num_thread
	IF ~KEYWORD_SET(num_thread) THEN num_thread = long(num_thread)

	;;-----
	;; ALLOCATE MEMORY
	;;-----

	mag2	= dblarr(n_gal, n_flux, n_aper)

	;;-----
	;; CALCULATIONS
	;;-----
	ftr_name	= lib + 'sub_ftn/get_magnitude.so'
		larr = lonarr(20) & darr = dblarr(20)
		larr(0) = n_gal
		larr(1) = n_part
		larr(2) = num_thread

		darr(0)	= -1.0	;; APERTURE SIZE (Should be altered below)

	FOR i=0L, n_flux - 1L DO BEGIN
		TIC
		dummy	= dblarr(n_part) - 1.0d8
		dummy	= get_flux(mass, met, gyr, $
			lib=lib, band=flux_list(i), num_thread=num_thread)


		FOR j=0L, n_aper - 1L DO BEGIN
			mag_dum	= dblarr(n_gal)
			darr(0)	= MAG_R(j)

			void	= call_external(ftr_name, 'get_magnitude', $
				rr, xc, yc, zc, bind, uind, pos, dummy, $
				mag_dum, larr, darr)

			mag2(*,i,j) = mag_dum
		ENDFOR
		TOC, /VERBOSE
	ENDFOR

	RETURN, mag2
END
