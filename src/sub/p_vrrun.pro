PRO P_VRrun, settings

	N1	= Settings.P_VRrun_snap(0)
	N2	= Settings.P_VRrun_snap(1)
	DN	= Settings.P_VRrun_snap(2)

	REDO:
	FOR i=N1, N2, DN DO BEGIN
		n_snap	= i
		read_vraptor, settings, i
		print, '      ----- ', i, ' / ', MAX([N1,N2])
	endfor

	STOP


			

End

