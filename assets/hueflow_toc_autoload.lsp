;;; hueflow_toc_autoload.lsp
;;; Add this file to AutoCAD Startup Suite once.
;;; It loads cad_toc_auto.lsp whenever a drawing session starts.

(vl-load-com)

(defun hueflow:toc-autoload (/ path)
  (setq path (findfile "cad_toc_auto.lsp"))
  (if path
    (load path)
    (princ "\nHueflow TOC autoload: cad_toc_auto.lsp was not found. Put it in an AutoCAD support/trusted folder.")
  )
  (princ)
)

(hueflow:toc-autoload)
