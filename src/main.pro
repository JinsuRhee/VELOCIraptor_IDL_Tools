Pro main

	;;-----
	;; Set the paths
	;;-----

	cd, '.', current=root_path
	root_path	= root_path + '/../'

	!path   = expand_path('+' + root_path + 'src/sub/') + ':' + !path
	;!path   = expand_path('+' + root_path + 'test/') + ':' + !path

	;;-----
	;; Read the setting list
	;;-----

	settings = 0. & file_nml = root_path + 'settings.nml'
	read_nml, settings, file=file_nml
	settings = CREATE_STRUCT(settings, 'root_path', root_path)
	settings = CREATE_STRUCT(settings, 'dir_lib', root_path + 'src/sub/')

	;;-----
	;; Main Procedures
	;;-----
	P_VRrun, settings

	stop

End
