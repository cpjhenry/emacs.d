	org-latex-classes '(
	("article" "\\documentclass{article}"
	 ("\\section{%s}" . "\\section*{%s}")
	 ("\\subsection{%s}" . "\\subsection*{%s}")
	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	 ("\\paragraph{%s}" . "\\paragraph*{%s}")
	 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	("report" "\\documentclass{report}"
	 ("\\part{%s}" . "\\part*{%s}")
	 ("\\chapter{%s}" . "\\chapter*{%s}")
	 ("\\section{%s}" . "\\section*{%s}")
	 ("\\subsection{%s}" . "\\subsection*{%s}")
	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	("book" "\\documentclass{book}"
	 ("\\part{%s}" . "\\part*{%s}")
	 ("\\chapter{%s}" . "\\chapter*{%s}")
	 ("\\section{%s}" . "\\section*{%s}")
	 ("\\subsection{%s}" . "\\subsection*{%s}")
	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

	org-latex-default-packages-alist '(
		("" "graphicx" t)
        	("" "grffile" t)
        	("" "longtable" nil)
        	("" "wrapfig" nil)
        	("" "rotating" nil)
        	("normalem" "ulem" t)
        	("" "amsmath" t)
        	("" "textcomp" t)
        	("" "amssymb" t)
        	("" "capt-of" nil)
        	("" "hyperref" nil))
