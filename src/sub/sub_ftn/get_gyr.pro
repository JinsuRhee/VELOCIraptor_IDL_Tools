;; Procedure that converts conformal time to scale factor and Gyr

FUNCTION get_gyr, t_conf, $
	dir_raw=dir_raw, dir_lib=dir_lib, simname=simname, $
	num_thread=num_thread, n_snap=n_snap, oM=oM, oL=oL, H0=H0

	;;-----
	;; Settings
	;;-----
	IF(~KEYWORD_SET(num_thread)) THEN SPAWN, 'nproc --all', num_thread
	IF(~KEYWORD_SET(num_thread)) THEN num_thread = long(num_thread)

	infoname	= dir_raw + 'output_' + string(n_snap,format='(I5.5)') + $
		'/info_' + string(n_snap,format='(I5.5)') + '.txt'
	rd_info, siminfo, file=infoname

	IF simname EQ 'NH' THEN BEGIN
		RESTORE, dir_lib + 'sub_ftn/conformal_table_NH.sav'
		RESTORE, dir_lib + 'sub_ftn/LBT_table_NH.sav'
	ENDIF ELSE IF simname EQ 'YZiCS' THEN BEGIN
		RESTORE, dir_lib + 'sub_ftn/conformal_table_YZiCS.sav'
		RESTORE, dir_lib + 'sub_ftn/LBT_table_YZiCS.sav'
	ENDIF ELSE IF simname EQ 'YZiCS2' THEN BEGIN
		RESTORE, dir_lib + 'sub_ftn/conformal_table_YZiCS2.sav'
		RESTORE, dir_lib + 'sub_ftn/LBT_table_YZiCS2.sav'
	ENDIF ELSE BEGIN
		IF ~KEYWORD_SET(oM) OR ~KEYWORD_SET(oL) OR ~KEYWORD_SET(H0) THEN BEGIN
			PRINT, 'No available cosmology chosen:'
			PRINT, '	Set keywords, oM=, oL=, H0='
			STOP
		ENDIF
		conformal, oM=oM, oL=oL, dir=dir_lib + 'sub_ftn/'
		lbt_table, oM=oM, oL=oL, H0=H0, dir=dir_lib + 'sub_ftn/'
	ENDELSE

	;;-----
	;; Allocate Memory
	;;-----

	n_mpi	= n_elements(siminfo.hindex(*,0))
	n_part	= n_elements(t_conf)

	t_res	= dblarr(n_part,2) - 1.0d8	;; [SFactor, GYR]
	v1 = dblarr(n_part) - 1.0d8 & v2 = dblarr(n_part) - 1.0d8
	;;-----
	;; Fortran Routine
	;;-----
	ftr_name	= dir_lib + 'sub_ftn/prop_time.so'
		larr = lonarr(20) & darr = dblarr(20)
		larr(0)	= n_part
		larr(1)	= num_thread

		darr(0)	= 1./siminfo.aexp - 1.0d

	void	= call_external(ftr_name, 'prop_time', $
		t_conf, v1, v2, conft, sfact, tmp_red, tmp_gyr, $
		larr, darr)

	t_res(*,0) = v1 & t_res(*,1) = v2
	return, t_res

END
