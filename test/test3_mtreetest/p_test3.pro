PRO P_test3_treelengthcheck, settings, gal, ct_org, tk_org, ct_new, tk_new



	n_gal 	= N_ELEMENTS(gal)

	tl 	= LONARR(n_gal,2)
	FOR i=0L, n_gal-1L DO BEGIN
		t1 	= f_gettree(1026L, gal(i).ID, ct_org, tk_org)
		t2 	= f_gettree(1026L, gal(i).ID, ct_new, tk_new)

		IF TYPENAME(t1) NE 'POINTER' THEN BEGIN
			tl(i,0)	= 1L
		ENDIF ELSE BEGIN
			t1 	= *t1
			tl(i,0)	= t1.snap(-1) - t1.snap(0) + 1L
		ENDELSE

		IF TYPENAME(t2) NE 'POINTER' THEN BEGIN
			tl(i,1)	= 1L
		ENDIF ELSE BEGIN
			t2 	= *t2
			tl(i,1)	= t2.snap(-1) - t2.snap(0) + 1L
		ENDELSE
	ENDFOR

	ref 	= js_plot()
		ref.data(0).x	= PTR_NEW(tl(*,0))
		ref.data(0).y 	= PTR_NEW(tl(*,1))
		ref.data(0).psym= 16L
		ref.data(0).symsize = 1.2

		ref.data(1).x	= PTR_NEW([0L, 1e3])
		ref.data(1).y 	= PTR_NEW([0L, 1e3])
		ref.data(1).linestyle	= 2L
		ref.data(1).linethick 	= 2.

		ref.axis_x(0).range	= [1, 1e3]
		ref.axis_x(0).title	= 'Old Tree'
		ref.axis_y(0).range	= [1, 1e3]
		ref.axis_y(0).title	= 'New Tree'

	ref.draw	= 'show'
	void	= js_plot(ref=ref)
	STOP
END

PRO p_test3_atool, settings, gal, ct_org, tk_org, ct_new, tk_new, id0=id0

	n_gal 	= N_ELEMENTS(gal)

	FOR i=0L, n_gal-1L DO BEGIN
		IF gal(i).ID NE id0 THEN CONTINUE

		t1 	= f_gettree(1026L, gal(i).ID, ct_org, tk_org)
		t2 	= f_gettree(1026L, gal(i).ID, ct_new, tk_new)

		e1	= f_getevol(t1, 1026L, gal(i).ID, horg='g', header=settings.root_path + 'test/test3*/vrheader.txt')
		e2	= f_getevol(t2, 1026L, gal(i).ID, horg='g', header=settings.root_path + 'test/test3*/vrheader.txt')


		slist 	= [e1.snapnum, e2.snapnum]
		slist 	= slist(SORT(slist))
		slist	= slist(UNIQ(slist))

		m_low 	= LONG(ALOG10(MIN([MIN(e1.mass_tot), MIN(e2.mass_tot)])))
		m_max 	= LONG(ALOG10(MAX([MAX(e1.mass_tot), MAX(e2.mass_tot)]))) + 1.d
		i0 	= N_ELEMENTS(slist)-1L
		br 	= 25.
		dN 	= 40L
		REPEAT BEGIN

			cgDisplay, 1200, 800
			cgPlot, 0, 0, /nodata, /noerase, position=[0.05, 0.55, 0.05 + 0.8/3., 0.95], $
				xrange=[slist(0), slist(-1)], yrange=[m_low, m_max]
			cgOplot, e1.snapnum, ALOG10(e1.mass_tot), linestyle=0, thick=4
			cgOplot, e2.snapnum, ALOG10(e2.mass_tot), linestyle=0, thick=2, color='red'

			cgPlot, 0, 0, /nodata, /noerase, position=[0.05, 0.05, 0.05 + 0.8/3., 0.45], $
				xrange=[-1L, 1L]*dN+slist(i0), yrange=[m_low, m_max]
			cgOplot, e1.snapnum, ALOG10(e1.mass_tot), linestyle=0, thick=4
			cgOplot, e1.snapnum, ALOG10(e1.mass_tot), psym=16, symsize=1.2

			cgOplot, e2.snapnum, ALOG10(e2.mass_tot), linestyle=0, thick=2, color='red'
			cgOplot, e2.snapnum, ALOG10(e2.mass_tot), psym=16, symsize=0.8, color='red'
			cgOplot, [slist(i0), slist(i0)], [0., 15.], linestyle=2, thick=2


			LOADCT, 52
			;;
			ind0	= WHERE(e1.snapnum EQ slist(i0),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e1(ind0).snapnum, e1(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e1(ind0).mass_tot/1e8, max=e1(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[1./3, 0.5, 2./3, 1.]
			ENDIF

			ind0	= WHERE(e1.snapnum EQ slist(i0-1L),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e1(ind0).snapnum, e1(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e1(ind0).mass_tot/1e8, max=e1(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[2./3, 0.5, 1., 1.]
			ENDIF

			;;
			ind0	= WHERE(e2.snapnum EQ slist(i0),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e2(ind0).snapnum, e2(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e2(ind0).mass_tot/1e8, max=e2(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[1./3, 0., 2./3, 0.5]
			ENDIF

			ind0	= WHERE(e2.snapnum EQ slist(i0-1L),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e2(ind0).snapnum, e2(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e2(ind0).mass_tot/1e8, max=e2(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[2./3, 0., 1., 0.5]
			ENDIF


			i0 	= i0-1L
			STOP
		ENDREP UNTIL i0 EQ -1L
		STOP

	ENDFOR
END


PRO p_test3_drawevol, settings, gal, ct_org, tk_org, ct_new, tk_new

	n_gal 	= N_ELEMENTS(gal)

	FOR i=0L, n_gal-1L DO BEGIN
		t1 	= f_gettree(1026L, gal(i).ID, ct_org, tk_org)
		t2 	= f_gettree(1026L, gal(i).ID, ct_new, tk_new)
 		
 		;IF gal(i).ID LT 476L THEN CONTINUE
 		;IF gal(i).ID GE 2000L THEN CONTINUE
 		IF gal(i).ID NE 491L THEN CONTINUE

 		IF TYPENAME(t1) NE 'POINTER' THEN BEGIN
 			s1 = 1026L
 		ENDIF ELSE BEGIN
 			s1 = (*t1).snap(0)
 		ENDELSE

 		IF TYPENAME(t2) NE 'POINTER' THEN BEGIN
 			s2 = 1026L
 		ENDIF ELSE BEGIN
 			s2 = (*t2).snap(0)
 		ENDELSE

		;IF s2 LT s1 THEN CONTINUE

		e1	= f_getevol(t1, 1026L, gal(i).ID, horg='g', header=settings.root_path + 'test/test3*/vrheader.txt')
		e2	= f_getevol(t2, 1026L, gal(i).ID, horg='g', header=settings.root_path + 'test/test3*/vrheader.txt')


		slist 	= [e1.snapnum, e2.snapnum]
		slist 	= slist(SORT(slist))
		slist	= slist(UNIQ(slist))

		m_low 	= LONG(ALOG10(MIN([MIN(e1.mass_tot), MIN(e2.mass_tot)])))
		m_max 	= LONG(ALOG10(MAX([MAX(e1.mass_tot), MAX(e2.mass_tot)]))) + 1.d

		dir	= '/storage6/jinsu/treetest/evol/GAL_' + STRING(gal(i).ID,format='(I4.4)') + '/'
		js_mkdir, dir

		br 	= 25.
		dN 	= 40L

		FOR i0=0L, N_ELEMENTS(slist)-1L DO BEGIN
IF slist(i0) MOD 10L NE 6L THEN CONTINUE
;IF slist(i0) NE 1026L THEN CONTINUE
			iname	= dir + 'S_' + STRING(slist(i0),format='(I4.4)') + '.eps'

			cgPS_open, iname, /encapsulated
			!p.font = -1 & !p.charsize = 0.8 & !p.charthick = 2.0
			cgPlot, 0, 0, /nodata, /noerase, position=[0.05, 0.55, 0.05 + 0.8/3., 0.95], $
				xrange=[slist(0), slist(-1)], yrange=[m_low, m_max]
			cgOplot, e1.snapnum, ALOG10(e1.mass_tot), linestyle=0, thick=4
			cgOplot, e2.snapnum, ALOG10(e2.mass_tot), linestyle=0, thick=2, color='red'
		
			cgPlot, 0, 0, /nodata, /noerase, position=[0.05, 0.05, 0.05 + 0.8/3., 0.45], $
				xrange=[-1L, 1L]*dN+slist(i0), yrange=[m_low, m_max]
			cgOplot, e1.snapnum, ALOG10(e1.mass_tot), linestyle=0, thick=4
			cgOplot, e1.snapnum, ALOG10(e1.mass_tot), psym=16, symsize=1.2

			cgOplot, e2.snapnum, ALOG10(e2.mass_tot), linestyle=0, thick=2, color='red'
			cgOplot, e2.snapnum, ALOG10(e2.mass_tot), psym=16, symsize=0.8, color='red'
			cgOplot, [slist(i0), slist(i0)], [0., 15.], linestyle=2, thick=2

			LOADCT, 52
			ind0	= WHERE(e1.snapnum EQ slist(i0),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e1(ind0).snapnum, e1(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e1(ind0).mass_tot/1e8, max=e1(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[1./3, 0.5, 2./3, 1.]
			ENDIF

			ind0	= WHERE(e1.snapnum EQ slist(i0-1L),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e1(ind0).snapnum, e1(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e1(ind0).mass_tot/1e8, max=e1(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[2./3, 0.5, 1., 1.]
			ENDIF

			;;
			ind0	= WHERE(e2.snapnum EQ slist(i0),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e2(ind0).snapnum, e2(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e2(ind0).mass_tot/1e8, max=e2(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[1./3, 0., 2./3, 0.5]
			ENDIF

			ind0	= WHERE(e2.snapnum EQ slist(i0-1L),ncut)
			IF ncut GE 1L THEN BEGIN
				img 	= draw_gal(e2(ind0).snapnum, e2(ind0).ID, vrheader=settings.root_path + 'test/test3*/vrheader.txt', num_thread=10L, $
					/raw, boxrange=br, n_pix=1000L, min=e2(ind0).mass_tot/1e8, max=e2(ind0).mass_tot/1e4)
				cgImage, img, /noerase, position=[2./3, 0., 1., 0.5]
			ENDIF

			cgPS_close
		
		ENDFOR

	ENDFOR
END

PRO p_test3, settings

	RESTORE, '/storage5/NewHorizon/VELOCIraptor/Galaxy/tree/l1_test/ctree.sav'
	ct_org	= complete_tree
	tk_org 	= tree_key

	RESTORE, '/storage6/jinsu/treetest/ctree.sav'
	ct_new	= complete_tree
	tk_new 	= tree_key

	gal 	= f_rdgal(1026L, -1L, horg='g', header=settings.root_path + 'test/test3*/vrheader.txt')

	P_test3_treelengthcheck, settings, gal, ct_org, tk_org, ct_new, tk_new
	;P_test3_atool, settings, gal, ct_org, tk_org, ct_new, tk_new, id=934L

	;P_test3_drawevol, settings, gal, ct_org, tk_org, ct_new, tk_new
END
