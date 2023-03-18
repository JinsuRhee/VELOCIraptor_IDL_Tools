Pro main, $
	modrun

	IF ~KEYWORD_SET(modrun) THEN modrun = -1L
	
	;;-----
	;; Set the paths
	;;-----

	cd, '.', current=root_path
	root_path	= root_path + '/../'

	!path   = expand_path('+' + root_path + 'src/sub/') + ':' + !path
	!path   = expand_path('+' + root_path + 'test/') + ':' + !path

	;;-----
	;; Read the setting list
	;;-----
	;STOP
	settings = 0. & file_nml = root_path + 'settings.nml'
	read_nml, settings, file=file_nml
	settings = CREATE_STRUCT(settings, 'root_path', root_path)
	settings = CREATE_STRUCT(settings, 'dir_lib', root_path + 'src/sub/')

	;;-----
	;; Main Procedures
	;;-----
	IF settings.P_VRrun EQ 1L THEN P_VRrun, settings
	IF settings.P_TFrun EQ 1L THEN P_TFrun, settings
	IF settings.P_CTree EQ 1L THEN P_CTree, settings
	IF settings.P_MKtreedat EQ 1L THEN P_MKtree, settings
	IF settings.P_MKCatalog EQ 1L THEN P_MKcatalog, settings

	;;-----
	;; TESTs
	;;-----
	IF settings.P_test1 EQ 1L THEN P_test1, settings
	IF settings.P_test2 EQ 1L THEN P_test2, settings, modrun
	IF settings.P_test3 EQ 1L THEN P_test3, settings

	PRINT, 'Compute contamination fraction & merge stellar clump to the host galaxy'
	STOP

End
