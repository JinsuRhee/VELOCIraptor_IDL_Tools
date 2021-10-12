PRO p_test2, settings, modrun

	id0	= 11L
	ttype = 'l1'
	;RESTORE, '/storage6/jinsu/var/NHCatalog/corrtest.sav'
	RESTORE, '/storage5/NewHorizon/VELOCIraptor/Galaxy/tree/' + STRTRIM(ttype, 2) +'/ctree_0360.sav'
	;RESTORE, '~/tmp.sav'
	tree	= f_gettree(1026L, id0, complete_tree, tree_key)
	gal	= f_getevol(tree, 1026L, id0, ['Xc', 'Yc', 'Zc', 'Mass_tot', 'ID', 'R_HalfMass', 'SFR'], horg='g', dir=settings.dir_catalog)
	;STOP
	bx 	= 50.
	;n0 	= 820L
	;n1 	= 758L
	cut0 	= WHERE(gal.snapnum GE MIN(gal.snapnum)+100L)
	cut1	= WHERE(gal.snapnum EQ MIN(gal.snapnum))
	IF MAX(cut0) EQ -1L THEN cut0 = N_ELEMENTS(gal)-1L
	SPAWN, 'mkdir ' + '/storage6/jinsu/var/NHCatalog/' + STRTRIM(ttype,2) + '/gal_' + $
		STRING(id0,format='(I4.4)')
	FOR i=cut0(0), cut1(0), -1L DO BEGIN
		IF modrun GE 0L THEN IF i MOD 4L NE modrun THEN CONTINUE
		id	= gal(i).id
		ss 	= gal(i).snapnum

		g2 	= f_rdgal(ss, -1L, header='~/Work_NHGroup/vrheader.txt', horg='g')
		d3d = js_d3d(g2.xc, g2.yc, g2.zc, [gal(i).xc, gal(i).yc, gal(i).zc])
		cut1 = WHERE(d3d LT bx AND g2.mass_tot GE 1e7, ncut)
		g2 	= g2(cut1)

		img 	= draw_gal(ss, id, vrheader='~/Work_NHGroup/vrheader.txt', num_thread=20L, /raw, $
			boxrange=bx, min=1e2, max=1e7, proj='xy', horg='g')
		img2	= draw_gal(ss, id, vrheader='~/Work_NHGroup/vrheader.txt', num_thread=20L, boxrange=bx, $
			min=1e2, max=1e7, horg='g')
		xr	= [-1., 1.]*bx + gal(i).Xc
		yr 	= [-1., 1.]*bx + gal(i).Yc

		iname 	= '/storage6/jinsu/var/NHCatalog/' + STRTRIM(ttype,2) + '/gal_' + $
			STRING(id0,format='(I4.4)') + '/corrtest_' + STRING(ss,format='(I4.4)') + '.eps'
		cgPS_open, iname, /encapsulated
		cgDisplay, 1200, 600
		cGImage, img, position=[0., 0., 0.5, 1.0]
		cgImage, img2, position=[0.5, 0., 1., 1.], /noerase
		cgplot, 0, 0, /nodata, /noerase, xrange=xr, yrange=yr, position=[0., 0., 0.5, 1.], $
			xstyle=4, ystyle=4


		ang 	= FINDGEN(100)/99.*!pi*2.
		cgOplot, COS(ang)*gal(i).r_halfmass + gal(i).xc, $
			SIN(ang)*gal(i).r_halfmass + gal(i).yc, linestyle=0, thick=2, color='red'

		IF ncut GE 1L THEN $
			FOR j=0L, ncut-1L DO $
				IF g2(j).ID NE gal(i).ID THEN $
					cgOplot, COS(ang)*g2(j).r_halfmass + g2(j).xc, $
					SIN(ang)*g2(j).R_Halfmass + g2(j).yc, linestyle=0, thick=2, color='dodger blue'


		PRINT, '%123123-----'
		PRINT, '	# SS = ', ss
		PRINT, '	# ID = ', id
		PRINT, '	# Mtot = ', ALOG10(gal(i).mass_tot)
		PRINT, '%123123-----'
		cgPS_close

		js_makepng, epsname=iname, density=500.
		
	ENDFOR
	STOP
END