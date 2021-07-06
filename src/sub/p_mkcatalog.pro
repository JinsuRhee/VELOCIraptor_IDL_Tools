FUNCTION p_mkcatalog_getbr, settings, complete_tree, id

	bid	= LONARR(N_ELEMENTS(id)) - 1L
	n_tree	= N_ELEMENTS(complete_tree)

	FOR i=0L, n_tree-1L DO BEGIN
		tmp	= *complete_tree(i)
		IF tmp.snap(tmp.endind) NE 1026L THEN CONTINUE
		cut	= WHERE(id EQ tmp.id(tmp.endind),ncut)
		IF ncut EQ 0L THEN CONTINUE
		bid(cut)	= i
	ENDFOR
	RETURN, bid
END
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
	gal	= f_rdgal(1026L, [settings.column_list, 'SFR', 'ABmag'], $
		dir=settings.dir_catalog, horg='g', id0=-1L)
	n_gal	= N_ELEMENTS(gal.id)

	;;-----
	;; LOAD Tree
	;;-----
	RESTORE, settings.dir_tree + 'l1.sav'

	;;-----
	;; GET BRANCH
	;;-----
	bid	= p_mkcatalog_getbr(settings, complete_tree, gal.id)

	gal	= f_rdgal(761L, [settings.column_list], dir=settings.dir_catalog, $
		horg='g', id0=-1L)
	ind	= js_bound(gal.xc, gal.yc, gal.zc, $
		xr=[-1.,1.]*20. + gal(2).xc, $
		yr=[-1.,1.]*20. + gal(2).yc, $
		zr=[-1.,1.]*20. + gal(2).zc)

	FOR i=0L, N_ELEMENTS(complete_tree)-1L DO BEGIN
		a	= (*complete_tree(i))
		cut	= WHERE(a.snap EQ 761, ncut)
		IF ncut EQ 0L THEN CONTINUE
		cut	= WHERE(a.id(cut) EQ 719L, ncut)
		IF ncut GE 1L THEN BREAK
	ENDFOR
	STOP
	img	= draw_gal(3L, 761L, dir_raw=settings.dir_raw, dir_catalog=settings.dir_catalog, boxrange=20., min=1e4, max=1e8, /raw)
	cgDisplay, 800, 800
	cgImage, img
	STOP
	for i=0L, n_gal-1L DO BEGIN
		dum	= f_getevol(complete_tree(bid(i)), $
			gal.id(i), 1026L, [settings.column_list, 'SFR', 'ABmag'], $
			horg='g', dir=settings.dir_catalog, /tmerit)

		;IF dum.tend LE 300L THEN CONTINUE

		fname	= settings.dir_catalog + 'VR_Galaxy/catalog/GAL_' + $
			STRING(gal(i).ID,format='(I6.6)') + '.txt'
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

		FOR j=0L, N_ELEMENTS(dum)-1L DO BEGIN
			PRINTF, 10, $
				dum(j).aexp , dum(j).id , dum(j).id_mbp , dum(j).numsubstruct , $
				dum(j).mvir , dum(j).mass_tot , dum(j).mass_fof , dum(j).mass_200mean , $
				dum(j).Mass_200crit , dum(j).Efrac , dum(j).rvir , dum(j).r_size , $
				dum(j).r_200mean , dum(j).r_200crit , dum(j).r_halfmass , $
				dum(j).r_halfmass_200mean , dum(j).r_halfmass_200crit , dum(j).rmax , $
				dum(j).xc ,dum(j).yc ,dum(j).zc ,dum(j).vxc ,dum(j).vyc ,dum(j).vzc , $
				dum(j).lx ,dum(j).ly ,dum(j).lz ,dum(j).sigV ,dum(j).vmax ,dum(j).npart , $
				dum(j).sfr,dum(j).abmag, dum(j).abmag, dum(j).abmag, $
				dum(j).abmag, dum(j).abmag, $
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
		STOP
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
