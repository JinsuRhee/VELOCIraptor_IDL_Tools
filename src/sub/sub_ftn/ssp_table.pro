Pro ssp_table

	spawn, 'pwd', pwd
	;;-----
	;; Read Band
	;;----
	;;;; Sloan Bands
	fname = pwd + '/ssp/SLOAN_SDSS.u.dat'
	readcol, fname, lam_u, tr_u, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/SLOAN_SDSS.g.dat'
	readcol, fname, lam_g, tr_g, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/SLOAN_SDSS.r.dat'
	readcol, fname, lam_r, tr_r, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/SLOAN_SDSS.i.dat'
	readcol, fname, lam_i, tr_i, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/SLOAN_SDSS.z.dat'
	readcol, fname, lam_z, tr_z, format='D, D', numline=file_lines(fname), /silent

	;;;; GALEX bands
	fname = pwd + '/ssp/GALEX_GALEX.FUV.dat'
	readcol, fname, lam_GFUV, tr_GFUV, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/GALEX_GALEX.NUV.dat'
	readcol, fname, lam_GNUV, tr_GNUV, format='D, D', numline=file_lines(fname), /silent

	;;;; HST IR bands
	fname = pwd + '/ssp/HST_WFC3_IR.F105W.dat'
	readcol, fname, lam_HIR_F105, tr_HIR_F105, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/HST_WFC3_IR.F125W.dat'
	readcol, fname, lam_HIR_F125, tr_HIR_F125, format='D, D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/HST_WFC3_IR.F160W.dat'
	readcol, fname, lam_HIR_F160, tr_HIR_F160, format='D, D', numline=file_lines(fname), /silent

	;;Read SSP Age & Wavelength TABLE
	fname = pwd + '/ssp/ages_yybc.dat'
	readcol, fname, age, format='D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/lambda.dat'
	readcol, fname, lambda, format='D', numline=file_lines(fname), /silent


        metal_str       = ['0.0004', '0.0010', '0.0040', '0.0100', '0.0200', '0.0400']
        metal_val       = double(metal_str)


        flux    = replicate({f:dblarr(n_elements(lambda), n_elements(age))},n_elements(metal_str))
        for fi=0L, n_elements(metal_str)-1L do begin
                fname = pwd + '/ssp/bc03_yy_' + strtrim(metal_str(fi))
                readcol, fname, tmp_flux, format='D', numline=file_lines(fname)
                tmp_flux = reform(tmp_flux,n_elements(lambda), n_elements(age))
                flux(fi).f = tmp_flux
        endfor

	save, filename='ssp_table_sp.sav', age, lambda, flux, $
		lam_u, lam_g, lam_r, lam_i, lam_z, $
		TR_u, TR_g, TR_r, TR_i, TR_z, $
		lam_GFUV, lam_GNUV, TR_GFUV, TR_GNUV, $
		lam_HIR_F105, lam_HIR_F125, lam_HIR_F160, $
		TR_HIR_F105, TR_HIR_F125, TR_HIR_F160


	fname = pwd + '/ssp/bc03_chab/ages_yybc.dat'
	readcol, fname, age, format='D', numline=file_lines(fname), /silent

	fname = pwd + '/ssp/bc03_chab/lambda.dat'
	readcol, fname, lambda, format='D', numline=file_lines(fname), /silent

	metal_str	= ['0.0001', '0.0004', '0.004', '0.008', '0.02', '0.05']
	metal_val	= double(metal_str)
        flux    = replicate({f:dblarr(n_elements(lambda), n_elements(age))},n_elements(metal_str))
        for fi=0L, n_elements(metal_str)-1L do begin
                fname = pwd + '/ssp/bc03_chab/bc03_chab_' + string(metal_str(fi),format='(F6.4)')
                readcol, fname, tmp_flux, format='D', numline=file_lines(fname)
                tmp_flux = reform(tmp_flux,n_elements(lambda), n_elements(age))
                flux(fi).f = tmp_flux
        endfor

	save, filename='ssp_table_chab.sav', age, lambda, flux, $
		lam_u, lam_g, lam_r, lam_i, lam_z, $
		TR_u, TR_g, TR_r, TR_i, TR_z, $
		lam_GFUV, lam_GNUV, TR_GFUV, TR_GNUV, $
		lam_HIR_F105, lam_HIR_F125, lam_HIR_F160, $
		TR_HIR_F105, TR_HIR_F125, TR_HIR_F160
	print, '^-^'
	wait, 121231

end
