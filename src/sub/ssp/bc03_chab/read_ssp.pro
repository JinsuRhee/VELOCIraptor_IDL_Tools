Pro read_ssp

	fname = ['m22', 'm32', 'm42', 'm52', 'm62', 'm72']
	metal = ['0.0001', '0.0004', '0.004', '0.008', '0.02', '0.05']
	for fi=0L, 5L do begin
		age	= dblarr(221)
		lambda	= dblarr(6900)
		openr, 10, 'bc2003_hr_' + strtrim(fname(fi),2) + '_chab_ssp.ised_ASCII'
		ind = 0L
		for i=1, 37 do begin
			tmp	= ' ' 
			readf, 10, tmp
			value	= strsplit(tmp, ', ', /extract)

			if i eq 1 then value = value(1L:*)
			age(ind:ind+n_elements(value)-1) = value
			ind = ind + n_elements(value)
		endfor

		tmp = ' '
		for i=1, 5 do readf, 10, tmp

		ind = 0L
		for i=43, 679 do begin
			tmp	= ' '
			readf, 10, tmp
			value	= strsplit(tmp, ', ', /extract)

			if i eq 43 then value = value(1L:*)
			lambda(ind:ind+n_elements(value)-1) = value
			ind = ind + n_elements(value)
			
		endfor

		ssp	= dblarr(221, 6900)
		age_ind = 0L
		wav_ind	= 0L
		nn	= 1L
		for i=680, 338288 do begin
			tmp	= ' '
			readf, 10, tmp
			value	= strsplit(tmp, ', ', /extract)
			if wav_ind eq 0 and long(value(0)) ne 6900 then continue
			;if wav_ind eq 0L then print, i 
			if wav_ind eq 0 then value = value(1L:*)

			if wav_ind + n_elements(value) - 1L ge 6900 then begin
				value	= value(0L:6900-wav_ind-1)
				ssp(age_ind, wav_ind:wav_ind+n_elements(value)-1L) = value
				wav_ind	= (wav_ind + n_elements(value)) mod 6900
				if wav_ind eq 0L then age_ind ++
			endif else begin
				ssp(age_ind, wav_ind:wav_ind+n_elements(value)-1L) = value
				wav_ind	= (wav_ind + n_elements(value)) mod 6900
				if wav_ind eq 0L then age_ind ++
			endelse
			if age_ind eq 221L then break
		endfor
		close, 10

		if fi eq 0L then begin
			age	= age / 1e9
			openw, 11, 'ages_yybc.dat'
			for ll=0L, 220L do printf, 11, string(age(ll),format='(F14.11)')
			close, 11

			openw, 12, 'lambda.dat'
			for ll=0L, 6899 do printf, 12, string(lambda(ll),format='(F11.2)')
			close, 12
		endif

		openw, 13, 'bc03_chab_' + string(metal(fi),format='(F6.4)')
		for la=0L, 220L do begin
			for ll=0L, 6899 do printf, 13, ssp(la,ll)
		endfor
		close, 13
	endfor
	
End
