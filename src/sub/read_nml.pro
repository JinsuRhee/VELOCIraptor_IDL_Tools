Pro read_nml, settings, file=file

	settings = {dum:'dum'}
	
	OPENR, 10, file
	for i=0L, file_lines(file)-1L do begin
		v1	= STRING(' ')
		READF, 10, v1
		v1	= STRTRIM(v1,2)
		if STRLEN(v1) eq 0L then continue

		in	= STRPOS(v1, '#')
		if MAX(in) ge 0L then continue

		void	= EXECUTE(v1)	; define a variable

		tag_name= STRSPLIT(v1, '=', /extract)
		tag_name= tag_name(0)
		v2	= 'settings = CREATE_STRUCT(settings,"' + STRTRIM(tag_name,2) + '",' + $
			STRTRIM(tag_name,2) + ')'
		void	= EXECUTE(v2)
	endfor
	CLOSE, 10

	IF settings.simname EQ 'YZiCS2' THEN BEGIN
		dir_catalog = '/storage5/FORNAX/VELOCI_RAPTOR/' + STRTRIM(settings.cname,2) + '/Galaxy/'
		IF settings.P_VRrun_horg EQ 'h' THEN $
			dir_catalog = '/storage5/FORNAX/VELOCI_RAPTOR/' + STRTRIM(settings.cname,2) + '/Halo/'
		dir_raw	= '/storage5/FORNAX/KISTI_OUTPUT/' + STRTRIM(settings.cname,2) + '/snapshots/'
		dir_save= '/storage5/FORNAX/KISTI_OUTPUT/' + STRTRIM(settings.cname,2) + '/'
	ENDIF

	IF settings.simname EQ 'NH' THEN BEGIN
		dir_catalog	= '/storage5/NewHorizon/VELOCIraptor/Galaxy/'
		IF settings.P_VRrun_horg EQ 'h' THEN $
			dir_catalog = '/storage5/NewHorizon/VELOCIraptor/Halo/'

		dir_raw         = '/storage1/NewHorizon/snapshots/'
		dir_save	= '/storage5/NewHorizon/VELOCIraptor/VR_Galaxy/'
		IF settings.P_VRrun_horg EQ 'h' THEN $
			dir_save = '/storage5/NewHorizon/VELOCIraptor/VR_Halo/'
	ENDIF

	IF settings.simname EQ 'YZiCS' THEN BEGIN
		dir_catalog    = '/storage5/FORNAX/VELOCI_RAPTOR/YZiCS/c' + STRTRIM(settings.cname,2) + '/Galaxy/'
		IF settings.P_VRrun_horg EQ 'h' THEN $
			dir_catalog    = '/storage5/FORNAX/VELOCI_RAPTOR/YZiCS/c' + STRTRIM(settings.cname,2) + '/Halo/'
		dir_raw	= '/storage3/Clusters/' + STRTRIM(settings.cname,2) + '/snapshots/'
		dir_save= '/storage3/Clusters/Vraptor/c' + STRTRIM(settings.cname,2) + '/VR_Galaxy/'
		IF settings.P_VRrun_horg EQ 'h' THEN $
			dir_save = '/storage3/Clusters/Vraptor/c' + STRTRIM(settings.cname,2) + '/VR_Halo/'
	ENDIF

	settings	= CREATE_STRUCT(settings, 'dir_catalog', dir_catalog, $
		'dir_raw', dir_raw, 'dir_save', dir_save)
End
