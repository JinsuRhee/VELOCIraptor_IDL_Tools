PRO P_VRrun, settings

	N1	= Settings.P_VRrun_snap(0)
	N2	= Settings.P_VRrun_snap(1)
	DN	= Settings.P_VRrun_snap(2)

	REDO:
	FOR i=N1, N2, DN DO BEGIN
		n_snap	= i

		PRINT, '	-----', i, ' is starting'
		dir	= settings.dir_catalog; + $
		IF settings.horg EQ 'h' THEN dir += 'Halo/'
		IF settings.horg EQ 'g' THEN dir += 'Galaxy/'
		dir	+= settings.dir_catalog_pre + $
			STRING(n_snap,format='(I4.4)') + $
			settings.dir_catalog_suf

		isdir	= STRLEN(FILE_SEARCH(dir))
		IF isdir LE 4L THEN CONTINUE

		file	= dir + '/PP_RUNNING'
		isfile	= STRLEN(FILE_SEARCH(file))
		IF isfile GE 4L THEN CONTINUE

		SPAWN, 'mkdir ' + dir + '/PP_RUNNING'
		read_vraptor, settings, i
		print, '      ----- ', i, ' / ', MAX([N1,N2])
	endfor

	STOP


			

End

