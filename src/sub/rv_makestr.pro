function rv_makestr, output2, output=output

	n_tag	= n_tags(output2)
	tagnames= tag_names(output2)
	if ~keyword_set(output) then begin
		for i=0L, n_tag - 1L do begin
			if i eq 0L then output = create_struct(tagnames(i), output2.(i))
			if i ge 1L then output = create_struct(output, tagnames(i), output2.(i))
		endfor
	endif else begin
		for i=0L, n_tag - 1L do begin
			output = create_struct(output, tagnames(i), output2.(i))
		endfor
	endelse

	return, output

End
