
FUNCTION p_test1_getevol, id0, snap, ctree

	FOR i=0L, N_ELEMENTS(ctree)-1L DO BEGIN
		cut	= WHERE((*ctree(i)).snap EQ snap, ncut)
		IF ncut EQ 0L THEN CONTINUE

		IF (*ctree(i)).ID(cut) EQ id0 THEN BEGIN
			ct	= *ctree(i)
			RETURN, ct
		ENDIF
	ENDFOR
	ct	= (*ctree(0))
	ct.numprog	= -1L
	RETURN, ct
END
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

PRO p_test1_lencomp, settings, ctree1, ctree3, tag

	gal	= f_rdgal(800L, [settings.column_list, 'SFR'], id0=-1L, dir=settings.dir_save, horg='g')

	len1	= DBLARR(N_ELEMENTS(gal.id))
	len3	= len1
	mass	= len1
	tag	= STRARR(N_ELEMENTS(gal.id))
	btype	= LONARR(N_ELEMENTS(gal.id),2)
	FOR i=0L, N_ELEMENTS(gal.id)-1L DO BEGIN
		id0	= GAL.id(i)
		mass(i)	= GAL.mass_tot(i)

		dum1	= p_test1_getevol(id0, 800L, ctree1)
		dum3	= p_test1_getevol(id0, 800L, ctree3)

		IF dum1.numprog GE 0L THEN $
			len1(i)	= dum1.snap(dum1.endind) - 800L

		IF dum3.numprog GE 0L THEN $
			len3(i)	= dum3.snap(dum3.endind) - 800L

		btype(i,*)	= 1L
		IF dum1.stat EQ 'sub' THEN btype(i,0) = -1L
		IF dum3.stat EQ 'sub' THEN btype(i,1) = -1L

		IF len1(i) EQ len3(i) THEN BEGIN
			tag(i)	= 'normal'
		ENDIF ELSE IF len1(i) GT len3(i) THEN BEGIN
			IF len1(i) EQ 159L THEN BEGIN
				tag(i)	= '1a'
			ENDIF ELSE IF len3(i) EQ 0L THEN BEGIN
				tag(i)	= '1c'
			ENDIF ELSE BEGIN
				tag(i)	= '1b'
			ENDELSE
		ENDIF ELSE BEGIN
			IF len3(i) EQ 159L THEN BEGIN
				tag(i)	= '2a'
			ENDIF ELSE IF len1(i) EQ 0L THEN BEGIN
				tag(i)	= '2c'
			ENDIF ELSE BEGIN
				tag(i)	= '2b'
			ENDELSE
		ENDELSE
	ENDFOR

END

;;-----
;; Visual Map
;;-----
PRO p_test1_vmap, settings, ctree1, ctree3, tag
	;;-----
	;; Find Galaxy
	;;-----
	cut2	= WHERE(tag EQ '2a')
	;cut	= cut(1)

	;;-----
	;; 2D Map
	;;-----
	n_pix	= 1000L

FOR ii=1L, N_ELEMENTS(cut2)-1L DO BEGIN
	cut	= cut2(ii)
	gal	= f_rdgal(800L, ['ID', 'Mass_tot'], id0=-1L, dir=settings.dir_save, horg='g')
	id0	= gal.ID(cut)

	dum1	= p_test1_getevol(id0, 800L, ctree1)
	dum3	= p_test1_getevol(id0, 800L, ctree3)

	dir	= '/storage6/jinsu/var/Paper3*/branchtest/'
	iname2	= dir + 'GAL_' + STRING(id0,format='(I3.3)') + '_' + $
		STRING(dum1.snap(dum1.endind) - 800L, format='(I3.3)') + '_' + $
		STRING(dum3.snap(dum3.endind) - 800L, format='(I3.3)')
	xc1	= DBLARR(200L, 2)
	xc3	= DBLARR(200L, 2)
	dl	= 200.
	FOR i=0L, N_ELEMENTS(dum1.ID)-1L DO BEGIN
		id0	= dum1.ID(i)
		snap	= dum1.snap(i)

		rd_info, info, file='/storage6/NewHorizon/output_' + $
			STRING(snap,format='(I5.5)') + '/info_' + $
			STRING(snap,format='(I5.5)') + '.txt'


		cut	= WHERE(dum3.snap EQ snap, ncut)
		iname	= iname2 + '_' + STRING(i,format='(I3.3)') + '.png'
		IF ncut EQ 0L THEN CONTINUE
		id1	= dum3.id(cut)

		gal1	= f_rdgal(snap, [settings.column_list], id0=id0, dir=settings.dir_save, horg='g')
		gal3	= f_rdgal(snap, [settings.column_list], id0=id1, dir=settings.dir_save, horg='g')

		ptcl	= f_rdptcl(id0, snap, num_thread=40L, dir_raw=settings.dir_raw, $
			dir_catalog=settings.dir_catalog + '../', /p_pos, /p_mass, /raw, $
			boxrange=dl)

		xr	= [-1.,1.]*dl + gal1.xc(0)
		yr	= [-1.,1.]*dl + gal1.yc(0)
		bw	= [(xr(1)-xr(0))/n_pix, (yr(1)-yr(0))/n_pix]*0.5
		denmap	= js_kde($
			xx=ptcl.xp(*,0), yy=ptcl.xp(*,1), xrange=xr, yrange=yr, $
			n_pix=n_pix, mode=-1L, kernel=1L, bandwidth=bw, weight=ptcl.mp)

		xc1(i,*)= [gal1.xc(0), gal1.yc(0)]*3.086d21/info.unit_l
		xc3(i,*)= [gal3.xc(0), gal3.yc(0)]*3.086d21/info.unit_l
		image	= BYTSCL(ALOG10(denmap.z + 1.0d), min=4., max=11.)

		SET_PLOT, 'Z'
		DEVICE, decomposed=0
		;DEVICE, SET_RESOLUTION=[1000, 1000]
		LOADCT, 0
		;cgPS_open, dir + iname, /encapsulated
		;cgDisplay, 800, 800
		cgPlot, 0, 0, /nodata, xstyle=4, ystyle=4, position=[0., 0., 1., 1.], $
			background='black', axiscolor='white', xrange=xr, yrange=yr
		cgImage, image, /noerase, position=[0., 0., 1., 1.]
		cgOplot, xc1(0:i,0)*info.unit_l/3.086d21, xc1(0:i,1)*info.unit_l/3.086d21, $
			linestyle=0, color='red'
	        cgOplot, xc3(0:i,0)*info.unit_l/3.086d21, xc3(0:i,1)*info.unit_l/3.086d21, $
			linestyle=0, color='blue'	
		cgOplot, gal1.xc(0), gal1.yc(0), psym=16, color='red', symsize=1.0
		cgOplot, gal3.xc(0), gal3.yc(0), psym=9, color='blue', symsize=2.0
		cap	= TVRD()
		DEVICE, /close
		SET_PLOT, 'X'
		WRITE_PNG, iname, cap

		STOP
		;cgPS_close
	ENDFOR
ENDFOR
END

PRO p_test1, settings

	RESTORE, '/storage6/jinsu/var/Paper3*/ctree1.sav'
	RESTORE, '/storage6/jinsu/var/Paper3*/ctree3.sav'
	;;-----
	;; LOAD Galaxies
	;;-----
	;gal	= f_rdgal(959L, [settings.column_list, 'SFR'], id0=-1L, dir=settings.dir_save, horg='g')
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
	;p_test1_drbranch, settings, ctree1, gal

	;;-----
	;; Odd branch
	;;-----
	p_test1_lencomp, settings, ctree1, ctree3, tag
	p_test1_vmap, settings, ctree1, ctree3, tag
	STOP
END
