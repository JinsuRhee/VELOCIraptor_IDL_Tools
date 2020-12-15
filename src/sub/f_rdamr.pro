FUNCTION f_rdamr, settings, gid, n_snap=n_snap, yzics=yzics, rfact=rfact, $
	num_thread=num_thread

	;;-----
	;; Settings
	;;-----
	dir_lib = settings.dir_lib & dir_raw = settings.dir_raw & dir_save = settings.dir_save

	;;-----
	;; Domain & Center & Radius
	;;-----
	fname	= dir_save + 'VR_Galaxy/' + 'snap_' + $
		STRING(n_snap,format='(I3.3)') + '/GAL_' + $
		STRING(gid,format='(I6.6)') + '.hdf5'
	fid = H5F_OPEN(fname) & did = H5D_OPEN(fid, '/Domain_List')
	dom_list = H5D_READ(did) & H5D_CLOSE, did & H5F_CLOSE, fid

	fid     = H5F_OPEN(fname)
	did = H5D_OPEN(fid, '/G_Prop/G_Xc') & xc = H5D_READ(did) & H5D_CLOSE, did
	did = H5D_OPEN(fid, '/G_Prop/G_Yc') & yc = H5D_READ(did) & H5D_CLOSE, did
	did = H5D_OPEN(fid, '/G_Prop/G_Zc') & zc = H5D_READ(did) & H5D_CLOSE, did
	did = H5D_OPEN(fid, '/G_Prop/G_R_HalfMass') & Rsize = H5D_READ(did) & H5D_CLOSE, did
	H5F_CLOSE, fid

	dlist	= (WHERE(dom_list GE 0L)) + 1L
	;;-----
	;; Read Info File
	;;-----
	infoname        = dir_raw + 'output_' + STRING(n_snap,format='(I5.5)') + $
		'/info_' + STRING(n_snap,format='(I5.5)') + '.txt'
	rd_info, siminfo, file=infoname

	xc	= xc / siminfo.unit_l * 3.086d21
	yc	= yc / siminfo.unit_l * 3.086d21
	zc	= zc / siminfo.unit_l * 3.086d21
	rsize	= rsize / siminfo.unit_l * 3.086d21

	xr	= [-rsize, rsize] * rfact + mean(xc)
	yr	= [-rsize, rsize] * rfact + mean(yc)
	zr	= [-rsize, rsize] * rfact + mean(zc)

	;;-----
	;; Read AMR
	;;-----
	fname1	= dir_raw + 'output_' + STRING(n_snap,format='(I5.5)') + $
		'/amr_' + STRING(n_snap,format='(I5.5)') + '.out'
	fname2	= dir_raw + 'output_' + STRING(n_snap,format='(I5.5)') + $
		'/hydro_' + STRING(n_snap,format='(I5.5)') + '.out'

	dx = [0.0d] & x = [0.0d] & y = [0.0d] & z = [0.0d] & var = dblarr(1,8)
	n1 = 0L & n2 = 0L & n3 = 0L & n4 = 0L & n5 = 0L
	FOR i=0L, N_ELEMENTS(dlist)-1L DO BEGIN
		rd_amr, grid, file=fname1 + STRING(dlist(i),format='(I5.5)'), $
			icpu=dlist(i), /silent

		rd_hydro, hydro, file=fname2 + STRING(dlist(i),format='(I5.5)'), $
			icpu=dlist(i), /silent

		amr2cell, grid, hydro, cell, xr=xr, yr=yr, zr=zr
		js_makearr, dx, cell.dx, n1, unitsize=500000L, type='D'
		js_makearr, x, cell.x, n2, unitsize=500000L, type='D'
		js_makearr, y, cell.y, n3, unitsize=500000L, type='D'
		js_makearr, z, cell.z, n4, unitsize=500000L, type='D'
		js_makearr, var, cell.var, n5, unitsize=500000L, type='D'
	ENDFOR
	dx	= dx(0L:n1-1L)
	x	= x(0L:n2-1L)
	y	= y(0L:n3-1L)
	z	= z(0L:n4-1L)
	var	= var(0L:n5-1L,*)

	mass	= var(*,0) * dx^3 * siminfo.unit_m / 1.98892d+33
	nH	= var(*,0) * siminfo.unit_nH
	temp	= var(*,4) / var(*,0) * siminfo.unit_t2
	metal	= var(*,7)

	dx	= dx * siminfo.unit_l / 3.086d21
	x	= x * siminfo.unit_l / 3.086d21
	y	= y * siminfo.unit_l / 3.086d21
	z	= z * siminfo.unit_l / 3.086d21

	cell	= {dx:dx, x:x, y:y, z:z, density:nH, temperature:temp, metal:var(*,7), mass:mass}
	RETURN, cell
END
