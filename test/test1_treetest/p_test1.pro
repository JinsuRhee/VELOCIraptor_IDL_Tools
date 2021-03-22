
PRO p_test1_findgal, settings, ctree, gal, prop

	cut	= LONARR(N_ELEMENTS(ctree))-1L
	iddum	= cut
	FOR i=0L, N_ELEMENTS(cut)-1L DO BEGIN
		IF (*ctree(i)).snap((*ctree(i)).endind) EQ 959L THEN BEGIN
			cut(i)	= 1L
			iddum(i)= (*ctree(i)).ID((*ctree(i)).endind)
		ENDIF
	ENDFOR
	cut	= WHERE(cut GE 0L)
	ctree	= ctree(cut)
	id	= iddum(cut)

	prop	= {mass:DBLARR(N_ELEMENTS(id)), length:LONARR(N_ELEMENTS(id))}

	FOR i=0L, N_ELEMENTS(id)-1L DO BEGIN
		cut	= WHERE(gal.id EQ id(i), ncut)
		IF ncut EQ 0L THEN STOP
		prop.mass(i)	= gal.mass_tot(cut)
		prop.length(i)	= 959L - (*ctree(i)).snap(0)
	ENDFOR
END

PRO p_test1_tlfig_mkhist, mass, length, xx, yy, mr
	cut	= WHERE(mass GT mr(0) AND mass LE mr(1))
	yy	= HISTOGRAM(length(cut), min=5, max=160, binsize=5, location=xx)
END
PRO p_test1_tlfig, settings, gal, prop1, prop3
	;;-----
	;; Mass vs. Treelength
	;;-----
	!p.font=-1 & !p.charsize=3.0 & !p.charthick=3.0
	cgDisplay, 800, 800

	mr	= [1e5, 1e12]
	p_test1_tlfig_mkhist, prop1.mass, prop1.length, x_all1, y_all1, mr
	p_test1_tlfig_mkhist, prop3.mass, prop3.length, x_all3, y_all3, mr
	PRINT, N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), ' / ', $
		N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)) 

	mr	= [1e6, 1e7]
	p_test1_tlfig_mkhist, prop1.mass, prop1.length, x_a1, y_a1, mr
	p_test1_tlfig_mkhist, prop3.mass, prop3.length, x_a3, y_a3, mr
	PRINT, N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), ' / ', $
		N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)) 

	mr	= [1e7, 1e8]
	p_test1_tlfig_mkhist, prop1.mass, prop1.length, x_b1, y_b1, mr
	p_test1_tlfig_mkhist, prop3.mass, prop3.length, x_b3, y_b3, mr
	PRINT, N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), ' / ', $
		N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)) 

	mr	= [1e8, 1e9]
	p_test1_tlfig_mkhist, prop1.mass, prop1.length, x_c1, y_c1, mr
	p_test1_tlfig_mkhist, prop3.mass, prop3.length, x_c3, y_c3, mr
	PRINT, N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), ' / ', $
		N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)) 

	mr	= [1e9, 1e10]
	p_test1_tlfig_mkhist, prop1.mass, prop1.length, x_d1, y_d1, mr
	p_test1_tlfig_mkhist, prop3.mass, prop3.length, x_d3, y_d3, mr
	PRINT, N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), ' / ', $
		N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)) 

	mr	= [1e10, 1e12]
	p_test1_tlfig_mkhist, prop1.mass, prop1.length, x_e1, y_e1, mr
	p_test1_tlfig_mkhist, prop3.mass, prop3.length, x_e3, y_e3, mr
	PRINT, N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1))), ' / ', $
		N_ELEMENTS(WHERE(prop1.mass GT mr(0) AND prop1.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)), $
		N_ELEMENTS(WHERE(prop3.mass GT mr(0) AND prop3.mass LE mr(1)))*1. / $
		N_ELEMENTS(WHERE(gal.mass_tot GT mr(0) AND gal.mass_tot LE mr(1) AND gal.isclump LE 0L)) 

	cgPlot, x_all1, y_all1, psym=10, color='green'
	cgOplot, x_all3, y_all3, psym=10, color='blue'
	STOP
	cgPlot, x_a1, y_a1, psym=10, color='green'
	cgOplot, x_a3, y_a3, psym=10, color='blue'
	STOP
	cgPlot, x_b1, y_b1, psym=10, color='green'
	cgOplot, x_b3, y_b3, psym=10, color='blue'
	STOP
	cgPlot, x_c1, y_c1, psym=10, color='green'
	cgOplot, x_c3, y_c3, psym=10, color='blue'
	STOP
	cgPlot, x_d1, y_d1, psym=10, color='green'
	cgOplot, x_d3, y_d3, psym=10, color='blue'
	STOP
	cgPlot, x_e1, y_e1, psym=10, color='green'
	cgOplot, x_e3, y_e3, psym=10, color='blue'
	STOP
END

FUNCTION p_test1_2dmap_evol, settings, id, ctree

	;;-----
	;; Find Branch
	;;-----
	FOR i=0L, N_ELEMENTS(ctree)-1L DO BEGIN
		IF (*ctree(i)).ID((*ctree(i)).endind) EQ id THEN BREAK
		IF i EQ N_ELEMENTS(ctree)-1L THEN RETURN, {stat:-1L}
	ENDFOR
	ct	= *ctree(i)

	nn	= ct.endind+1L
	evol	= {x:DBLARR(nn), y:DBLARR(nn), z:DBLARR(nn), SFR:DBLARR(nn), mass:DBLARR(nn), stat:1L}
	FOR i=nn-1L, 0L, -1L DO BEGIN
		id0	= ct.id(i)
		snap	= ct.snap(i)
		galdum	= f_rdgal(snap, [settings.column_list, 'SFR'], id0=id0, dir=settings.dir_save, $
			horg='g')
		rd_info, info, file='/storage6/NewHorizon/output_' + $
			STRING(snap,format='(I5.5)') + '/info_' + STRING(snap,format='(I5.5)') + $
			'.txt'

		evol.x(i)	= galdum.xc(0)*3.086d21/info.unit_l
		evol.y(i)	= galdum.yc(0)*3.086d21/info.unit_l
		evol.z(i)	= galdum.zc(0)*3.086d21/info.unit_l
		evol.sfr(i)	= galdum.sfr(0,3)
		evol.mass(i)	= galdum.mass_tot(0)
	ENDFOR

	RETURN, evol
END

PRO p_test1_2dmap, settings, gal, ctree1, ctree3

	;;-----
	;; INFO
	;;-----
	rd_info, info, file='/storage6/NewHorizon/output_00959/info_00959.txt'
	;;-----
	;; Find Gal
	;;-----
	halo	= f_rdgal(959L, ['Xc', 'Yc', 'Zc', 'Mvir', 'Rvir'], id0=1L, dir=settings.dir_save, horg='h')
	
	dx	= (gal.xc - halo.xc(0))^2 + (gal.yc - halo.yc(0))^2 + (gal.zc - halo.zc(0))^2
	dx	= SQRT(dx)
	cut	= WHERE(dx LT 1.0*halo.rvir(0))

	;;-----
	;; Halo Figure
	;;-----
	halo.xc	*= 3.086d21/info.unit_l
	halo.yc *= 3.086d21/info.unit_l
	halo.rvir	*= 3.086d21/info.unit_l

	range	= [-1., 1.]*halo.rvir(0)*1.5
	cgDisplay, 800, 800
	cgPlot, 0, 0, /nodata, xstyle=4, ystyle=4, $
		xrange=range + halo.xc(0), yrange=range + halo.yc(0), $
		position=[0., 0., 1., 1.]

	ang	= FINDGEN(100)/99.*!pi*2.
	cgOplot, halo.xc(0) + halo.rvir(0)*COS(ang), halo.yc(0) + halo.rvir(0)*SIN(ang), $
		linestyle=0

	cgLoadct, 33
	FOR i=0L, N_ELEMENTS(cut)-1L DO BEGIN
		ind	= cut(i)
		;;-----
		;; Last Point
		;;-----
		xc	= gal.xc(ind)*3.086d21/info.unit_l
		yc	= gal.yc(ind)*3.086d21/info.unit_l

		;;-----
		;; DRAW TRAJECTORY
		;;-----
		id	= gal.id(ind)
		evol1	= p_test1_2dmap_evol(settings, id, ctree3)
		IF evol1.stat EQ -1L THEN CONTINUE

		;;-----
		color	= 255./N_ELEMENTS(cut)*i
		cgOplot, xc, yc, psym=16, symsize=1.5, color=BYTE(color)
		cgOplot, xc, yc, psym=16, symsize=1., color='WHITE'

		;;-----
		cgOplot, evol1.x, evol1.y, linestyle=0, color=BYTE(color)

	ENDFOR	
	STOP

END

PRO p_test1_drbranch, settings, ctree, gal

	;;-----
	;; Find Tree
	;;-----
	FOR i=0L, N_ELEMENTS(ctree)-1L DO BEGIN
		id0	= (*ctree(i)).ID((*ctree(i)).endind)
		nsnap	= (*ctree(i)).snap((*ctree(i)).endind)

		IF id0 NE gal.id(0) OR nsnap NE 959L THEN CONTINUE
		ind	= i
		BREAK
	ENDFOR
	nprog	= (*ctree(ind)).numprog

	;;-----
	;; DRAW
	;;-----
	cgDisplay, 800, 800
	cgPlot, 0, 0, /nodata, xrange=[-0.5, 0.5]*nprog, yrange=[800L, 959L], $
		position=[0., 0., 1., 1.], xstyle=4, ystyle=4
	cgLoadct, 33

	;;-----
	;; MAIN BRANCH
	;;-----
	FOR i=0, (*ctree(ind)).endind DO BEGIN
		id	= (*ctree(ind)).ID(i)
		snap	= (*ctree(ind)).snap(i)

		gal	= f_rdgal(snap, [settings.column_list], id0=id, dir=settings.dir_save, horg='g')
		color	= BYTSCL(ALOG10(gal.mass_tot(0)),min=6., max=11.)
		cgOplot, 0, snap, psym=16, symsize=2.0, color=color
	ENDFOR

	;;-----
	;; OTHER BRANCH
	FOR i=1L, nprog-1 DO BEGIN
		p_id	= (*ctree(ind)).m_id(i)
		p_snap	= (*ctree(ind)).m_snap(i)
		cut	= WHERE(p_id GE 0L)
		id	= p_id(cut)
		snap	= p_snap(cut)

		FOR j=0L, N_ELEMENTS(ctree)-1L DO BEGIN
			id0	= (*ctree(j)).ID((*ctree(j)).endind)
			nsnap	= (*ctree(j)).snap((*ctree(j)).endind)
			IF id0 NE id OR nsnap NE snap THEN CONTINUE
			ind2	= j
			BREAK
		ENDFOR

		FOR j=0L, (*ctree(ind2)).endind DO BEGIN
			id	= (*ctree(ind2)).ID(j)
			snap	= (*ctree(ind2)).snap(j)

			gal	= f_rdgal(snap, [settings.column_list], id0=id, $
				dir=settings.dir_save, horg='g')
			color	= BYTSCL(ALOG10(gal.mass_tot(0)), min=6., max=11.)

			nn	= LONG(i/2)+1L
			IF i MOD 2L EQ 1L THEN nn = -nn
			cgOplot, nn, snap, psym=16, symsize=2.0, color=color
		ENDFOR
		cgOplot, [nn, 0], [snap, snap], linestyle=0, color=color, thick=1
	ENDFOR


	STOP
END

PRO p_test1_findoddgal, settings, gal, ctree1, ctree3, ind, range

	cut	= WHERE(gal.mass_tot GT range(0) AND gal.mass_tot LE range(1))
	FOR i=0L, N_ELEMENTS(cut)-1L DO BEGIN
		in	= cut(i)
		id	= gal.id(in)

		FOR j=0L, N_ELEMENTS(ctree1)-1L DO BEGIN
			IF (*ctree1(j)).ID((*ctree1(j)).endind) EQ id AND $
				(*ctree1(j)).snap((*ctree1(j)).endind) EQ 959L AND $
				(*ctree1(j)).snap((*ctree1(j)).endind) - 800L GE 150L THEN BEGIN

				id0	= (*ctree1(j)).id(0)
				snap0	= (*ctree1(j)).snap(0)
				FOR k=0L, N_ELEMENTS(ctree3)-1L DO BEGIN
					IF (*ctree3(k)).ID(0) EQ id0 AND $
						(*ctree3(k)).snap(0) EQ snap0 THEN BEGIN;AND $
						;(*ctree3(k)).snap((*ctree3(k)).endind) - 800L LE 20L THEN BEGIN
						;STOP
						PRINT, (*ctree3(k)).snap((*ctree3(k)).endind) - 800
					ENDIF
				ENDFOR
			ENDIF
		ENDFOR
	ENDFOR
	
END
PRO p_test1, settings

	RESTORE, '/storage6/jinsu/var/Paper3*/ctree1.sav'
	RESTORE, '/storage6/jinsu/var/Paper3*/ctree3.sav'
	;;-----
	;; LOAD Galaxies
	;;-----
	gal	= f_rdgal(959L, [settings.column_list, 'SFR'], id0=-1L, dir=settings.dir_save, horg='g')
	;p_test1_findgal, settings, ctree1, gal, prop1
	;p_test1_findgal, settings, ctree3, gal, prop3

	;;-----
	;; Treelength Figure
	;;-----
	;p_test1_tlfig, settings, gal, prop1, prop3

	;;-----
	;; 2D MAP
	;;-----
	;p_test1_2dmap, settings, gal, ctree1, ctree3

	;;-----
	;; Branch Of the most massive galaxy
	;;-----
	p_test1_drbranch, settings, ctree1, gal
	STOP
	;;-----
	;; Odd branch
	;;-----
	mr	= [1e10, 1e12]
	p_test1_findoddgal, settings, gal, ctree1, ctree3, ind, mr

	STOP
END
