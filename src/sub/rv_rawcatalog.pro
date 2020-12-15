function rv_rawcatalog, dir_snap=dir_snap, horg=horg, column_list=column_list, silent=silent, skip=skip 

	;;-----
	;; Find # of columns first, tags, and types
	;;-----
	str	= ' '
	if horg eq 'h' then openr, 10, dir_snap + 'halo.dat.properties.0'
	if horg eq 'g' then openr, 10, dir_snap + 'galaxy.dat.properties.0'

	readf, 10, str & readf, 10, str & readf, 10, str
	dtag    = strsplit(str, ' ', /extract)
	n_column= n_elements(dtag)
	dtype   = replicate('D', n_column)
 	for i=0L, n_column-1L do begin
 	        dum     = strpos(dtag(i),'(')
 	        dtag(i) = strmid(dtag(i),0, dum)
 	        if dtag(i) eq 'ID' then dtype(i) = 'LL'
 	        if dtag(i) eq 'ID_mbp' then dtype(i) = 'LL'
 	        if dtag(i) eq 'ID_minpot' then dtype(i) = 'LL'
 	        if dtag(i) eq 'hostHaloID' then dtype(i) = 'LL'
 	        if dtag(i) eq 'numSubStruct' then dtype(i) = 'L'
 	        if dtag(i) eq 'npart' then dtype(i) = 'L'
 	        if dtag(i) eq 'Structuretype' then dtype(i) = 'L'
 	        if dtag(i) eq 'n_gas' then dtype(i) = 'L'
 	        if dtag(i) eq 'n_star' then dtype(i) = 'L'
 	        if dtag(i) eq 'n_bh' then dtype(i) = 'L'
 	endfor
 	close, 10

	;;-----
	;; Read Data w/ calling readcol
	;;-----

	if horg eq 'h' then dum_fname = file_search(dir_snap + 'halo.dat.properties.*')
	if horg eq 'g' then dum_fname = file_search(dir_snap + 'galaxy.dat.properties.*')

	dum_nn	= 0L
	for i=0L, n_elements(dum_fname)-1L do dum_nn = $
		dum_nn + file_lines(dum_fname(i)) - 3L

	;;;;----- Allocate Memory
	for i=0L, n_elements(column_list)-1L do begin
		tmp     = 'arr' + strtrim(i,2) + '='
		cut     = where(dtag eq column_list(i))

		if max(cut) lt 0L then print, '        ***** column_list has a wrong argument'
		if max(cut) lt 0L then stop

		if dtype(cut) eq 'LL' then tmp = tmp + 'lon64arr('
		if dtype(cut) eq 'L' then tmp = tmp + 'lonarr('
		if dtype(cut) eq 'D' then tmp = tmp + 'dblarr('
		if dtype(cut) eq 'F' then tmp = tmp + 'fltarr('
		tmp     = tmp + 'dum_nn)'
		void    = execute(tmp)

		if i eq 0L then tmp2 = 'output = create_struct(dtag(' + $
			strtrim(cut(0),2) + '), arr' + strtrim(i,2)
		if i ge 1L then tmp2 = tmp2 + ',dtag(' + strtrim(cut(0),2) + $
			'), arr' + strtrim(i,2)
	endfor
	tmp2    = tmp2 + ')'
	void    = execute(tmp2)

	;;;;----- Read
	n1 = 0L &  n2 = -1L
	for fi=0L, n_elements(dum_fname)-1L do begin
		fname2  = dum_fname(fi)

		if file_lines(fname2) eq 3L then continue
		dum_str = 'readcol, fname2, '
		for i=1L, n_column do dum_str = dum_str + 'v' + strtrim(i,2) + ', '
		dum_str = dum_str + 'format="'
		for i=0L, n_column-2L do dum_str = dum_str + strtrim(dtype(i),2) + ', '
		dum_str = dum_str + strtrim(dtype(n_column-1L),2) + '", '
		dum_str = dum_str + 'numline=file_lines(fname2), skipline=3'
		if keyword_set(silent) then dum_str = dum_str + ', /silent'
		void    = execute(dum_str)

		;;;;;;----- Adjust the line numbers
		n1	= n2 + 1L
		n2      = n2 + n_elements(v1)

		;;;;;;----- Extract the requested columns
		n_match	= 0L
		n_nan	= 0L
		for i=0L, n_column-1L do begin
			cut     = where(column_list eq dtag(i))
			if max(cut) lt 0L then continue
			n_match ++
			dum_str = 'output.' + strtrim(dtag(i),2) + $
				'(' + strtrim(n1,2) + ':' + strtrim(n2,2) + ') = ' + $
				'v' + strtrim(i+1,2)
			void    = execute(dum_str)
		endfor

		if n_elements(column_list) ne n_match then $
			print, '        ***** There is a wrong typed one in column_list'
	endfor

	return, output
End
