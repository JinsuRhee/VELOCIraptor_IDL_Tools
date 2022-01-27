;;-----
;; GENERATE TREE.dat
;;-----
PRO p_mktree, settings

	RESTORE, settings.dir_tree + 'ctree.sav'

	n_branch	= N_ELEMENTS(complete_tree)
	f_name	= settings.dir_tree + 'ctree.dat'

	OPENW, 10, f_name

	;;-----
	;; HEADER
	;;-----
	WRITEU, 10, LONG(n_branch)

	FOR i=0L, n_branch-1L DO BEGIN
		tmp	= *complete_tree(i)

		;; Num link
		n_link	= LONG(N_ELEMENTS(tmp.id))
		WRITEU, 10, n_link
		;; ID Link
		WRITEU, 10, tmp.ID
		;; Snap Link
		WRITEU, 10, tmp.snap

		;; Others?
	ENDFOR

	;;-----
	;; TREEKEY
	;;-----
	WRITEU, 10, LONG(N_ELEMENTS(tree_key))
	WRITEU, 10, tree_key
	CLOSE, 10

END
