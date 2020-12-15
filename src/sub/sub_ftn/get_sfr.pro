FUNCTION get_sfr, $
	xc, yc, zc, rr, bind, uind, pos, mass, gyr, $
	SFR_T=SFR_T, SFR_R=SFR_R, lib=lib, num_thread=num_thread

	;;-----
	;; Settings
	;;-----
	n_gal	= n_elements(xc)
	n_sfr	= n_elements(SFR_T)
	n_part	= n_elements(pos(*,0))

	IF ~KEYWORD_SET(num_thread) THEN spawn, 'nproc --all', num_thread
	IF ~KEYWORD_SET(num_thread) THEN num_thread = long(num_thread)

	;;-----
	;; Allocate Memory
	;;-----
	SFR	= dblarr(n_gal, n_sfr)

	;;-----
	;; Calculate SFRs
	;;-----
	ftr_name	= lib + 'sub_ftn/prop_sfr.so'
		larr = lonarr(20) & darr = dblarr(20)
		larr(0) = n_gal
		larr(1) = n_part
		larr(2) = num_thread

		darr(0)	= 1.0		;; Aperture size in Rhalf mass
		darr(10)= 0.1d		;; SFR Time scale in Gyr

	FOR i=0L, n_sfr - 1L DO BEGIN
		darr(0) = double(SFR_R(i))
		darr(1)	= double(SFR_t(i))

		dummy	= dblarr(n_gal)

		void	= call_external(ftr_name, 'prop_sfr', $
			xc, yc, zc, rr, bind, uind, pos, mass, gyr, $
			dummy, larr, darr)

		SFR(*,i)= dummy
	ENDFOR

	RETURN, SFR

END
