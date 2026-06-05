;;; hueflow_toc_autoload.lsp
;;; Add this file to AutoCAD Startup Suite once.
;;; It loads cad_toc_auto.lsp whenever a drawing session starts.

(vl-load-com)

(defun hueflow:toc-first-existing (paths / found p)
  (setq found nil)
  (foreach p paths
    (if (and (null found) p (findfile p))
      (setq found (findfile p))
    )
  )
  found
)

(defun hueflow:toc-add-trusted-path (path / dir trusted)
  (setq dir (vl-filename-directory path))
  (if dir
    (progn
      (setq trusted (getvar "TRUSTEDPATHS"))
      (if (not trusted) (setq trusted ""))
      (if (not (vl-string-search (strcase dir) (strcase trusted)))
        (vl-catch-all-apply
          'setvar
          (list "TRUSTEDPATHS" (strcat trusted (if (= trusted "") "" ";") dir))
        )
      )
    )
  )
)

(defun hueflow:toc-autoload (/ home path)
  (setq home (getenv "USERPROFILE"))
  (setq path
    (hueflow:toc-first-existing
      (append
        (list
          "cad_toc_auto.lsp"
          "C:\\Users\\kimyj\\Documents\\HueflowCADTools\\cad-toc-auto\\cad_toc_auto.lsp"
        )
        (if home
          (list (strcat home "\\Documents\\HueflowCADTools\\cad-toc-auto\\cad_toc_auto.lsp"))
          nil
        )
      )
    )
  )
  (if path
    (progn
      (hueflow:toc-add-trusted-path path)
      (load path)
    )
    (princ "\nHueflow TOC autoload: cad_toc_auto.lsp was not found. Put it in an AutoCAD support/trusted folder or Documents\\HueflowCADTools\\cad-toc-auto.")
  )
  (princ)
)

(hueflow:toc-autoload)
