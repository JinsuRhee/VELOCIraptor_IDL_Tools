FUNCTION f_getfinaldes, settings, id, n0, n1, snaplist

	id0	= id
	FOR i=n0 + 1, n1 DO BEGIN
		GAL	= f_rdgal(settings, snaplist(i), ['ID'])
		cut	= WHERE(GAL.progs EQ id0)
		IF N_ELEMENTS(cut) GE 2L THEN BEGIN
			PRINT, 'Multiple descent !?'
			STOP
		ENDIF
		IF MAX(cut) LT 0L THEN BEGIN
			PRINT, 'No Descent exists'
			STOP
			RETURN, -1
		ENDIF
		ind	= ARRAY_indices(GAL.progs, cut)
		id0	= GAL.id(ind(0))
	ENDFOR

	RETURN, id0
END
