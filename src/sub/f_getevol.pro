FUNCTION f_getevol, settings, id0, n0, horg=horg, abmag=abmag, sfr=sfr

	;;-----
	;; Settings
	;;-----

	IF ~KEYWORD_SET(horg) THEN horg = 'g'
	datalist	= settings.column_list
	IF KEYWORD_SET(abmag) THEN datalist = [datalist, 'ABmag']
	IF KEYWORD_SET(sfr) THEN datalist = [datalist, 'SFR']

	;;-----
	;; Get Snapshot list
	;;-----
	IF horg EQ 'h' THEN flist = settings.dir_save + '/VR_Halo/snap_*'
	IF horg EQ 'g' THEN flist = settings.dir_save + '/VR_Galaxy/snap_*'

	flist	= FILE_SEARCH(flist)
	nlist	= LONARR(N_ELEMENTS(flist)) - 1L
	FOR i=0L, N_ELEMENTS(flist)-1L DO BEGIN
		dum	= STRSPLIT(flist(i), '/', /extract)
		dum     = dum(-1)
		dum     = STRMID(dum,5L, 3L)
		nlist(i)= LONG(dum)
	ENDFOR

	CUT	= WHERE(nlist LE n0)
	flist	= flist(cut) & nlist = nlist(cut)

	;;-----
	;; Read one galaxy for making a data structure
	;;-----

	GAL	= f_rdgal(settings, n0, datalist, id=1L)

	;;-----
	;; Memory Allocate
	;;-----

	tree	= CREATE_STRUCT('snaplist', nlist)
	FOR i=0L, N_ELEMENTS(datalist) - 1L DO BEGIN
		str	= 'dumarr = GAL.' + STRTRIM(datalist(i),2)
		void	= EXECUTE(str)
		IF datalist(i) EQ 'ABmag' OR datalist(i) EQ 'SFR' THEN BEGIN
			;dumarr	= REFORM(dumarr,N_ELEMENTS(dumarr(0,*,0)),N_ELEMENTS(dumarr(0,0,*)))
			dumarr	= REBIN(dumarr,N_ELEMENTS(flist),N_ELEMENTS(dumarr(0,*,0)),N_ELEMENTS(dumarr(0,0,*)))*0 - 1
		ENDIF ELSE BEGIN
			dumarr	= REBIN(dumarr,N_ELEMENTS(flist))*0 - 1
		ENDELSE

		tree = CREATE_STRUCT(tree, datalist(i), dumarr)
	ENDFOR

	;;-----
	;; Input
	;;----

	FOR i=N_ELEMENTS(flist)-1L, 0L, -1L DO BEGIN
		IF id0 LT 0L THEN BREAK

		GAL	= f_rdgal(settings, nlist(i), datalist, id=id0)

		FOR j=0L, N_ELEMENTS(datalist) - 1L DO BEGIN
			IF datalist(j) EQ 'ABmag' THEN BEGIN
				str	= 'tree.' + STRTRIM(datalist(j),2) + '(' + STRTRIM(i,2) + ',*,*)' + $
					'= GAL.' + STRTRIM(datalist(j),2) + '(0,*,*)'
			ENDIF ELSE IF datalist(j) EQ 'SFR' THEN BEGIN
				str	= 'tree.' + STRTRIM(datalist(j),2) + '(' + STRTRIM(i,2) + ',*)' + $
					'= GAL.' + STRTRIM(datalist(j),2) + '(0,*)'
			ENDIF ELSE BEGIN
				str	= 'tree.' + STRTRIM(datalist(j),2) + '(' + STRTRIM(i,2) + ') = ' + $
					'GAL.' + STRTRIM(datalist(j),2) + '(0)'
			ENDELSE

			void	= EXECUTE(str)
		ENDFOR

		id0	= GAL.progs(0)
	ENDFOR

	RETURN, tree
END
