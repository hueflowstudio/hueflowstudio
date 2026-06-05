;;; HF_AREA.lsp v6.0
;;; Hueflow Studio - area labels and summary table
;;; Command:
;;;   HFA  = select closed polylines once, then create labels and summary automatically
;;;   HFAT = select closed polylines, then pick summary table position manually

(vl-load-com)

(defun hfarea:old (name)
  (getvar name)
)

(defun hfarea:set (name val)
  (vl-catch-all-apply 'setvar (list name val))
)

(defun hfarea:restore (/)
  (if hfarea:*old-osmode* (hfarea:set "OSMODE" hfarea:*old-osmode*))
  (if hfarea:*old-cmdecho* (hfarea:set "CMDECHO" hfarea:*old-cmdecho*))
  (if hfarea:*old-layer* (hfarea:set "CLAYER" hfarea:*old-layer*))
  (if hfarea:*old-error* (setq *error* hfarea:*old-error*))
  (setq hfarea:*old-osmode* nil)
  (setq hfarea:*old-cmdecho* nil)
  (setq hfarea:*old-layer* nil)
  (setq hfarea:*old-error* nil)
  (princ)
)

(defun hfarea:error (msg)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
    (princ (strcat "\n[HFA] Error: " msg))
  )
  (hfarea:restore)
)

(defun hfarea:ensure-layer (name color /)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        '(70 . 0)
        (cons 62 color)
        '(6 . "Continuous")
      )
    )
  )
)

(defun hfarea:variant-list (v)
  (cond
    ((= (type v) 'VARIANT) (vlax-safearray->list (vlax-variant-value v)))
    ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
    ((listp v) v)
    (T nil)
  )
)

(defun hfarea:bbox (obj / mn mx)
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))
      )
    nil
    (list (hfarea:variant-list mn) (hfarea:variant-list mx))
  )
)

(defun hfarea:minx (bb) (car (car bb)))
(defun hfarea:miny (bb) (cadr (car bb)))
(defun hfarea:maxx (bb) (car (cadr bb)))
(defun hfarea:maxy (bb) (cadr (cadr bb)))
(defun hfarea:w (bb) (- (hfarea:maxx bb) (hfarea:minx bb)))
(defun hfarea:h (bb) (- (hfarea:maxy bb) (hfarea:miny bb)))

(defun hfarea:pt (code pt)
  (list code (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0))
)

(defun hfarea:mtext (pt txt h layer attach width / style)
  (setq style (getvar "TEXTSTYLE"))
  (if (null style) (setq style "Standard"))
  (entmake
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbMText")
      (hfarea:pt 10 pt)
      (cons 40 h)
      (cons 41 width)
      (cons 71 attach)
      (cons 72 5)
      (cons 1 txt)
      (cons 7 style)
    )
  )
)

(defun hfarea:line (p1 p2 layer)
  (entmake
    (list
      '(0 . "LINE")
      (cons 8 layer)
      (hfarea:pt 10 p1)
      (hfarea:pt 11 p2)
    )
  )
)

(defun hfarea:closed-p (obj / res)
  (setq res (vl-catch-all-apply 'vla-get-Closed (list obj)))
  (and (not (vl-catch-all-error-p res)) (= res :vlax-true))
)

(defun hfarea:collect (ss / i ent obj bb area-mm area-m2 area-py rows minw minx miny maxx maxy skipped)
  (setq rows '())
  (setq minw nil)
  (setq minx nil)
  (setq miny nil)
  (setq maxx nil)
  (setq maxy nil)
  (setq skipped 0)
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (if (and obj (hfarea:closed-p obj) (setq bb (hfarea:bbox obj)))
      (progn
        (setq area-mm (vla-get-Area obj))
        (setq area-m2 (/ area-mm 1000000.0))
        (setq area-py (/ area-mm 3305785.0))
        (setq rows
          (append rows
            (list
              (list
                (1+ (length rows))
                area-m2
                area-py
                bb
              )
            )
          )
        )
        (if (or (null minw) (< (hfarea:w bb) minw)) (setq minw (hfarea:w bb)))
        (setq minx (if minx (min minx (hfarea:minx bb)) (hfarea:minx bb)))
        (setq miny (if miny (min miny (hfarea:miny bb)) (hfarea:miny bb)))
        (setq maxx (if maxx (max maxx (hfarea:maxx bb)) (hfarea:maxx bb)))
        (setq maxy (if maxy (max maxy (hfarea:maxy bb)) (hfarea:maxy bb)))
      )
      (setq skipped (1+ skipped))
    )
    (setq i (1+ i))
  )
  (list rows minw (list minx miny maxx maxy) skipped)
)

(defun hfarea:label-rows (rows txt-h layer / row bb m pt txt)
  (foreach row rows
    (setq bb (nth 3 row))
    (setq m (* txt-h 0.5))
    (setq pt (list (- (hfarea:maxx bb) m) (+ (hfarea:miny bb) m) 0.0))
    (setq txt
      (strcat
        (rtos (nth 1 row) 2 2)
        "m2 / "
        (rtos (nth 2 row) 2 2)
        "PY"
      )
    )
    (hfarea:mtext pt txt txt-h layer 9 (* txt-h 18.0))
  )
)

(defun hfarea:table-point-auto (overall txt-h / x y)
  (setq x (+ (nth 2 overall) (* txt-h 6.0)))
  (setq y (nth 3 overall))
  (list x y 0.0)
)

(defun hfarea:make-table (rows base total-m2 total-py txt-h layer / cell-h widths col-x col-cx table-w table-h tx ty r i x y row)
  (setq cell-h (* txt-h 2.5))
  (setq widths (list (* txt-h 6.0) (* txt-h 9.0) (* txt-h 9.0)))
  (setq table-w (apply '+ widths))
  (setq table-h (* cell-h (+ (length rows) 2)))
  (setq tx (car base))
  (setq ty (cadr base))

  (setq i 0)
  (repeat (+ (length rows) 3)
    (setq y (- ty (* cell-h i)))
    (hfarea:line (list tx y 0.0) (list (+ tx table-w) y 0.0) layer)
    (setq i (1+ i))
  )

  (setq col-x (list tx))
  (foreach w widths
    (setq col-x (append col-x (list (+ (last col-x) w))))
  )
  (foreach x col-x
    (hfarea:line (list x ty 0.0) (list x (- ty table-h) 0.0) layer)
  )

  (setq col-cx '())
  (setq i 0)
  (foreach w widths
    (setq col-cx (append col-cx (list (+ (nth i col-x) (/ w 2.0)))))
    (setq i (1+ i))
  )

  (setq y (- ty (/ cell-h 2.0)))
  (hfarea:mtext (list (nth 0 col-cx) y 0.0) "NO" txt-h layer 5 (* txt-h 5.0))
  (hfarea:mtext (list (nth 1 col-cx) y 0.0) "AREA (m2)" txt-h layer 5 (* txt-h 8.0))
  (hfarea:mtext (list (nth 2 col-cx) y 0.0) "AREA (PY)" txt-h layer 5 (* txt-h 8.0))

  (setq r 1)
  (foreach row rows
    (setq y (- ty (* cell-h r) (/ cell-h 2.0)))
    (hfarea:mtext (list (nth 0 col-cx) y 0.0) (itoa (nth 0 row)) txt-h layer 5 (* txt-h 5.0))
    (hfarea:mtext (list (nth 1 col-cx) y 0.0) (rtos (nth 1 row) 2 2) txt-h layer 5 (* txt-h 8.0))
    (hfarea:mtext (list (nth 2 col-cx) y 0.0) (rtos (nth 2 row) 2 2) txt-h layer 5 (* txt-h 8.0))
    (setq r (1+ r))
  )

  (setq y (- ty (* cell-h r) (/ cell-h 2.0)))
  (hfarea:mtext (list (nth 0 col-cx) y 0.0) "TOTAL" txt-h layer 5 (* txt-h 5.0))
  (hfarea:mtext (list (nth 1 col-cx) y 0.0) (rtos total-m2 2 2) txt-h layer 5 (* txt-h 8.0))
  (hfarea:mtext (list (nth 2 col-cx) y 0.0) (rtos total-py 2 2) txt-h layer 5 (* txt-h 8.0))
)

(defun hfarea:run (manual-table / ss data rows minw overall skipped txt-h layer total-m2 total-py table-pt row)
  (setq hfarea:*old-osmode* (getvar "OSMODE"))
  (setq hfarea:*old-cmdecho* (getvar "CMDECHO"))
  (setq hfarea:*old-layer* (getvar "CLAYER"))
  (setq hfarea:*old-error* *error*)
  (setq *error* hfarea:error)
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)

  (setq layer "HF-AREA")
  (hfarea:ensure-layer layer 7)

  (princ "\n[HFA] Select closed polylines: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  (if ss
    (progn
      (setq data (hfarea:collect ss))
      (setq rows (nth 0 data))
      (setq minw (nth 1 data))
      (setq overall (nth 2 data))
      (setq skipped (nth 3 data))
      (if rows
        (progn
          (setq txt-h (/ minw 15.0))
          (if (< txt-h 50.0) (setq txt-h 50.0))
          (setq total-m2 0.0)
          (setq total-py 0.0)
          (foreach row rows
            (setq total-m2 (+ total-m2 (nth 1 row)))
            (setq total-py (+ total-py (nth 2 row)))
          )
          (hfarea:label-rows rows txt-h layer)
          (setq table-pt
            (if manual-table
              (getpoint "\n[HFA] Pick summary table upper-left point: ")
              (hfarea:table-point-auto overall txt-h)
            )
          )
          (if table-pt
            (hfarea:make-table rows table-pt total-m2 total-py (* txt-h 0.9) layer)
          )
          (princ (strcat "\n[HFA] Done: " (itoa (length rows)) " areas, total "
                         (rtos total-m2 2 2) "m2 / " (rtos total-py 2 2) "PY"))
          (if (> skipped 0)
            (princ (strcat "\n[HFA] Skipped open/non-area polylines: " (itoa skipped)))
          )
        )
        (princ "\n[HFA] No closed polylines found.")
      )
    )
    (princ "\n[HFA] Nothing selected.")
  )
  (command "_.REGEN")
  (hfarea:restore)
)

(defun c:HFA () (hfarea:run nil))
(defun c:HFAT () (hfarea:run T))

(princ "\nLoaded HF_AREA v6.0. Commands: HFA=auto table, HFAT=manual table point.")
(princ)
