PRO p_mkcatalog, settings

	;;-----
	;; HEADER
	;;-----
	H_SFR1	= '	'
	FOR i=0L, N_ELEMENTS(settings.sfr_r)-1L DO H_SFR1 += STRING(settings.sfr_r(i),format='(F4.1)') + ' '

	H_SFR2	= '	'
	FOR i=0L, N_ELEMENTS(settings.sfr_r)-1L DO H_SFR2 += STRING(settings.sfr_t(i),format='(F4.2)') + ' '

	F_SFR	= ' '
	FOR i=0L, N_ELEMENTS(settings.sfr_r)-1L DO F_SFR += ',F9.5,5X '

	H_MAG	= '	'
	FOR i=0L, N_ELEMENTS(settings.mag_r)-1L DO H_MAG += STRING(settings.mag_r(i),format='(F4.1)') + ' '
	F_MAG	= ' '
	FOR i=0L, N_ELEMENTS(settings.mag_r)-1L DO $
		FOR j=0L, N_ELEMENTS(settings.flux_list) DO F_MAG += ',F9.5,5X '

	HEADER	= 'AEXP'
	FOR i=0L, N_ELEMENTS(settings.column_list)-1L DO HEADER += ' ' + settings.column_list(i)
	FOR i=0L, N_ELEMENTS(settings.sfr_r)-1L DO HEADER += ' SFR' + STRING(i,format='(I2.2)')
	FOR i=0L, N_ELEMENTS(settings.flux_list)-1L DO $
		FOR j=0L, N_ELEMENTS(settings.mag_r)-1L DO $
			HEADER += ' ' + STRTRIM(settings.flux_list(i)) + STRING(j,format='(I1.1)')

	;;-----
	;; LOAD GALAXY
	;;-----
	gal	= f_rdgal(959L, [settings.column_list, 'SFR', 'ABmag'], $
		dir=settings.dir_catalog, horg='g', id0=-1L)
	n_gal	= N_ELEMENTS(gal.id)

	FOR i=0L, n_gal-1L DO BEGIN
		dum	= f_getevol(gal.id(i), 959L, [settings.column_list, 'SFR', 'ABmag'], $
			horg='g', dir=settings.dir_catalog, /tmerit)

		IF dum.tend LE 300L THEN CONTINUE

		fname	= '/storage5/NewHorizon/VELOCIraptor/VR_Galaxy/catalog/GAL_' + $
			STRING(gal.id(i),format='(I6.6)') + '.txt'
		OPENW, 10, fname

		;;-----
		;; WRITE HEADER
		;;-----
		PRINTF, 10, 'SFR0 - SFR14'
		PRINTF, 10, '	Aperture in Re'
	       	PRINTF, 10, H_SFR1

		PRINTF, 10, '	Time in Gyr'
		PRINTF, 10, H_SFR2
		PRINTF, 10, ' '

		PRINTF, 10, 'MAG0 - MAG2'
		PRINTF, 10, '	Aperture in Re'
		PRINTF, 10, H_MAG
		PRINTF, 10, ' '

		PRINTF, 10, HEADER

		FOR j=0L, dum.tend DO BEGIN
			PRINTF, 10, $
				dum.aexp(j), dum.id(j), dum.id_mbp(j), dum.numsubstruct(j), $
				dum.mvir(j), dum.mass_tot(j), dum.mass_fof(j), dum.mass_200mean(j), $
				dum.Mass_200crit(j), dum.Efrac(j), dum.rvir(j), dum.r_size(j), $
				dum.r_200mean(j), dum.r_200crit(j), dum.r_halfmass(j), $
				dum.r_halfmass_200mean(j), dum.r_halfmass_200crit(j), dum.rmax(j), $
				dum.xc(j),dum.yc(j),dum.zc(j),dum.vxc(j),dum.vyc(j),dum.vzc(j), $
				dum.lx(j),dum.ly(j),dum.lz(j),dum.sigV(j),dum.vmax(j),dum.npart(j), $
				dum.sfr(j,*),dum.abmag(j,0,*), dum.abmag(j,1,*), dum.abmag(j,2,*), $
				dum.abmag(j,3,*), dum.abmag(j,4,*), $
				format='(F6.4,4X, I6,4X, I10,4X, I2,4X,' + $
				'F14.0,4X, F14.0,4X, F14.0,4X, F14.0,4X,' + $
			       	'F14.0,4X, F6.4,4X, F11.5,4X, F11.5,4X,' + $
		       		'F11.5,4X, F11.5,4X, F11.5,4X,' + $
				'F11.5,4X, F11.5,4X, F11.5,4X,' + $
				'F9.3,4X, F9.3,4X, F9.3,4X, F9.4,4X, F9.4,4X, F9.4,4X,' + $
				'F20.0,4X, F20.0,4X, F20.0,4X, F9.5,4X, F9.5,4X, I8,4X' + $
				STRTRIM(F_SFR,2) + STRTRIM(F_MAG,2) + ')'
		ENDFOR
		CLOSE, 10

		PRINT, i, ' / ', n_gal
	ENDFOR



	;;STOP
	STOP
	gal	= f_rdgal(959L, [settings.column_list, 'SFR', 'ABmag'], dir=settings.dir_catalog, horg='g', id0=-1L)

	n_gal	= N_ELEMENTS(gal.id)

	dum	= f_getevol(gal.id(0), 959L, [settings.column_list, 'SFR', 'ABmag'], horg='g', dir=settings.dir_catalog, /tmerit)

	dumstr	= 'dumstr = CREATE_STRUCT('
	dumtagname	= TAG_NAMES(dum)
	FOR i=0L, N_TAGS(dum)-1L DO BEGIN
		IF i NE 0L THEN dumstr += ','
		IF N_ELEMENTS(dum.(i)) GE 10L THEN BEGIN
			IF SIZE(dum.(i),/n_dimension) EQ 1L THEN $
				dumstr += '"' + dumtagname(i) + '",dum.(' + STRTRIM(i,2) + ')(0L:650L)'
			IF SIZE(dum.(i),/n_dimension) EQ 2L THEN $
				dumstr += '"' + dumtagname(i) + '",dum.(' + STRTRIM(i,2) + ')(0L:650L,*)'
			IF SIZE(dum.(i),/n_dimension) EQ 3L THEN $
				dumstr += '"' + dumtagname(i) + '",dum.(' + STRTRIM(i,2) + ')(0L:650L,*,*)'
		ENDIF ELSE BEGIN
			dumstr += '"' + dumtagname(i) + '",dum.(' + STRTRIM(i,2) + ')'
		ENDELSE
	ENDFOR
	dumstr	+= ')'
	void	= EXECUTE(dumstr)

	tree	= REPLICATE(dumstr, n_gal)

	cut	= LONARR(n_gal) + 1L
	FOR i=0L, n_gal - 1L DO BEGIN
		dum	= f_getevol(gal.id(i), 959L, [settings.column_list, 'SFR', 'ABmag'], horg='g', dir=settings.dir_catalog, /tmerit)
		IF dum.tend LT 650L THEN BEGIN
			cut(i) = -1L
			CONTINUE
		ENDIF

		FOR j=0L, N_TAGS(dum)-1L DO BEGIN
			IF N_ELEMENTS(dum.(j)) GE 10L THEN BEGIN
				IF SIZE(dum.(j),/n_dimension) EQ 1L THEN $
					tree(i).(j)	= dum.(j)(0L:650L)
				IF SIZE(dum.(j),/n_dimension) EQ 2L THEN $
					tree(i).(j)	= dum.(j)(0L:650L,*)
				IF SIZE(dum.(j),/n_dimension) EQ 3L THEN $
					tree(i).(j)	= dum.(j)(0L:650L,*,*)
			ENDIF ELSE BEGIN
				tree(i).(j)	= dum.(j)
			ENDELSE
		ENDFOR

		PRINT, i, ' / ', n_gal
	ENDFOR

	STOP

END
