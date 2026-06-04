;;; cad_toc_auto.lsp
;;; Semi-automatic drawing index maker for AutoCAD.
;;; Command: TOCAUTO
;;;
;;; Workflow:
;;; 1) Select title-block texts or the drawing-title area texts.
;;; 2) The routine finds DWG numbers such as FI-002, scales such as 1/150,
;;;    and sheet numbers such as 102.
;;; 3) It builds rows and outputs them as either plain TEXT or an AutoCAD TABLE.

(vl-load-com)

(defun toc:init-config ()
  (if (null *toc-dwgno-patterns*)
    (setq *toc-dwgno-patterns*
      "@-###,@@-###,@@@-###,@@@@-###,@-####,@@-####,@@@-####,@@@@-####"
    )
  )
  (if (null *toc-sheet-patterns*)
    (setq *toc-sheet-patterns* "###,####")
  )
  (if (null *toc-scale-patterns*)
    (setq *toc-scale-patterns* "NONE,*1/#*,*1/##*,*1/###*,*1/####*,A#:*1/#*,A#:*1/##*,A#:*1/###*")
  )
  (if (null *toc-ignore-patterns*)
    (setq *toc-ignore-patterns*
      "*DRAWING TITLE*,*DRAWING NAME*,*APPROVAL*,*APPROVED*,*PROJECT NO*,*CHECKED*,*ENGINEER*,*SCALE*,*DRAWN*,*DATE*,*DRAWING NO*,*SHEET NO*,*S/ NO*,*KIYEONG*,*INTERIOR*"
    )
  )
  (if (null *toc-title-mode*) (setq *toc-title-mode* "Table"))
  (if (null *toc-output-mode*) (setq *toc-output-mode* "Table"))
  (if (null *toc-paper-prefix*) (setq *toc-paper-prefix* "A3 : "))
  (if (null *toc-auto-format-scale*) (setq *toc-auto-format-scale* T))
  (princ)
)

(toc:init-config)

(defun toc:str-replace (find repl str / pos)
  (if (and find repl str (/= find ""))
    (while (setq pos (vl-string-search find str))
      (setq str
        (strcat
          (if (> pos 0) (substr str 1 pos) "")
          repl
          (substr str (+ pos (strlen find) 1))
        )
      )
    )
  )
  str
)

(defun toc:clean (s)
  (if (null s) (setq s ""))
  (setq s (vl-princ-to-string s))
  (setq s (toc:str-replace "\\P" " " s))
  (setq s (toc:str-replace "\\p" " " s))
  (setq s (toc:str-replace "\\~" " " s))
  (setq s (toc:str-replace "—" "-" s))
  (setq s (toc:str-replace "–" "-" s))
  (setq s (toc:str-replace "−" "-" s))
  (setq s (toc:str-replace "－" "-" s))
  (setq s (toc:str-replace "／" "/" s))
  (setq s (toc:str-replace "{" "" s))
  (setq s (toc:str-replace "}" "" s))
  (setq s (vl-string-trim " \t\r\n" s))
  (while (vl-string-search "  " s)
    (setq s (toc:str-replace "  " " " s))
  )
  s
)

(defun toc:compact (s)
  (strcase
    (toc:str-replace
      " "
      ""
      (toc:str-replace
        "\t"
        ""
        (toc:clean s)
      )
    )
  )
)

(defun toc:text (it) (nth 0 it))
(defun toc:x (it) (nth 1 it))
(defun toc:y (it) (nth 2 it))
(defun toc:h (it) (nth 3 it))

(defun toc:make-item (txt pt ht)
  (if (and txt pt)
    (list
      (toc:clean txt)
      (car pt)
      (cadr pt)
      (if (and ht (> ht 0.0)) ht 2.5)
    )
  )
)

(defun toc:variant->list (v)
  (cond
    ((= (type v) 'VARIANT) (vlax-safearray->list (vlax-variant-value v)))
    ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
    ((listp v) v)
    (T nil)
  )
)

(defun toc:effective-name (obj)
  (cond
    ((vlax-property-available-p obj 'EffectiveName)
      (vla-get-EffectiveName obj)
    )
    ((vlax-property-available-p obj 'Name)
      (vla-get-Name obj)
    )
    (T "")
  )
)

(defun toc:get-bbox (obj / mn mx)
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))
      )
    nil
    (list
      (toc:variant->list mn)
      (toc:variant->list mx)
    )
  )
)

(defun toc:bbox-min (bb) (car bb))
(defun toc:bbox-max (bb) (cadr bb))
(defun toc:bbox-minx (bb) (car (toc:bbox-min bb)))
(defun toc:bbox-miny (bb) (cadr (toc:bbox-min bb)))
(defun toc:bbox-maxx (bb) (car (toc:bbox-max bb)))
(defun toc:bbox-maxy (bb) (cadr (toc:bbox-max bb)))
(defun toc:bbox-w (bb) (- (toc:bbox-maxx bb) (toc:bbox-minx bb)))
(defun toc:bbox-h (bb) (- (toc:bbox-maxy bb) (toc:bbox-miny bb)))

(defun toc:norm-box (bb p1 p2 / w h minx maxx miny maxy)
  (setq w (toc:bbox-w bb))
  (setq h (toc:bbox-h bb))
  (if (and (> w 0.0) (> h 0.0))
    (progn
      (setq minx (min (car p1) (car p2)))
      (setq maxx (max (car p1) (car p2)))
      (setq miny (min (cadr p1) (cadr p2)))
      (setq maxy (max (cadr p1) (cadr p2)))
      (list
        (/ (- minx (toc:bbox-minx bb)) w)
        (/ (- miny (toc:bbox-miny bb)) h)
        (/ (- maxx (toc:bbox-minx bb)) w)
        (/ (- maxy (toc:bbox-miny bb)) h)
      )
    )
  )
)

(defun toc:denorm-box (bb nbox / w h)
  (setq w (toc:bbox-w bb))
  (setq h (toc:bbox-h bb))
  (list
    (+ (toc:bbox-minx bb) (* (nth 0 nbox) w))
    (+ (toc:bbox-miny bb) (* (nth 1 nbox) h))
    (+ (toc:bbox-minx bb) (* (nth 2 nbox) w))
    (+ (toc:bbox-miny bb) (* (nth 3 nbox) h))
  )
)

(defun toc:item-in-raw-box-p (it box / minx miny maxx maxy x y)
  (setq minx (min (nth 0 box) (nth 2 box)))
  (setq maxx (max (nth 0 box) (nth 2 box)))
  (setq miny (min (nth 1 box) (nth 3 box)))
  (setq maxy (max (nth 1 box) (nth 3 box)))
  (setq x (toc:x it))
  (setq y (toc:y it))
  (and (>= x minx) (<= x maxx) (>= y miny) (<= y maxy))
)

(defun toc:get-point (obj / p)
  (cond
    ((vlax-property-available-p obj 'InsertionPoint)
      (toc:variant->list (vlax-get obj 'InsertionPoint))
    )
    ((vlax-property-available-p obj 'TextAlignmentPoint)
      (toc:variant->list (vlax-get obj 'TextAlignmentPoint))
    )
    (T nil)
  )
)

(defun toc:get-height (obj)
  (cond
    ((vlax-property-available-p obj 'Height)
      (vlax-get obj 'Height)
    )
    ((vlax-property-available-p obj 'TextHeight)
      (vlax-get obj 'TextHeight)
    )
    (T 2.5)
  )
)

(defun toc:item-from-object (obj / txt pt ht)
  (if (vlax-property-available-p obj 'TextString)
    (progn
      (setq txt (vlax-get obj 'TextString))
      (setq pt (toc:get-point obj))
      (setq ht (toc:get-height obj))
      (if (and pt (/= (toc:clean txt) ""))
        (toc:make-item txt pt ht)
      )
    )
  )
)

(defun toc:items-from-ss (ss / i en obj items atts item)
  (setq i 0)
  (setq items '())
  (while (< i (sslength ss))
    (setq en (ssname ss i))
    (setq obj (vlax-ename->vla-object en))
    (cond
      ((= (vla-get-ObjectName obj) "AcDbBlockReference")
        (if (and
              (vlax-property-available-p obj 'HasAttributes)
              (= (vla-get-HasAttributes obj) :vlax-true)
            )
          (progn
            (setq atts (vlax-invoke obj 'GetAttributes))
            (foreach att atts
              (setq item (toc:item-from-object att))
              (if item (setq items (cons item items)))
            )
          )
        )
      )
      (T
        (setq item (toc:item-from-object obj))
        (if item (setq items (cons item items)))
      )
    )
    (setq i (1+ i))
  )
  (reverse items)
)

(defun toc:is-dwgno (s / u)
  (toc:init-config)
  (setq u (toc:compact s))
  (wcmatch u *toc-dwgno-patterns*)
)

(defun toc:is-sheetno (s / u)
  (toc:init-config)
  (setq u (toc:compact s))
  (wcmatch u *toc-sheet-patterns*)
)

(defun toc:is-scale (s / u)
  (toc:init-config)
  (setq u (toc:compact s))
  (wcmatch u *toc-scale-patterns*)
)

(defun toc:is-date-like (s / u)
  (setq u (toc:compact s))
  (or
    (wcmatch u "####.##")
    (wcmatch u "####.##.")
    (wcmatch u "####-##")
    (wcmatch u "####_##")
  )
)

(defun toc:is-label (s / u)
  (toc:init-config)
  (setq u (strcase (toc:clean s)))
  (or
    (= u "")
    (wcmatch u *toc-ignore-patterns*)
  )
)

(defun toc:is-title-candidate (it)
  (and
    (/= (toc:text it) "")
    (> (strlen (toc:compact (toc:text it))) 1)
    (not (wcmatch (toc:compact (toc:text it)) "#,##,-,--,---"))
    (not (toc:is-dwgno (toc:text it)))
    (not (toc:is-sheetno (toc:text it)))
    (not (toc:is-scale (toc:text it)))
    (not (toc:is-date-like (toc:text it)))
    (not (toc:is-label (toc:text it)))
  )
)

(defun toc:is-good-title (s / u)
  (setq u (toc:compact s))
  (and
    (> (strlen u) 1)
    (not (wcmatch u "#,##,-,--,---"))
  )
)

(defun toc:dist (a b / dx dy)
  (setq dx (- (toc:x a) (toc:x b)))
  (setq dy (- (toc:y a) (toc:y b)))
  (sqrt (+ (* dx dx) (* dy dy)))
)

(defun toc:avg-height (items / sum n)
  (setq sum 0.0)
  (setq n 0)
  (foreach it items
    (if (> (toc:h it) 0.0)
      (progn
        (setq sum (+ sum (toc:h it)))
        (setq n (1+ n))
      )
    )
  )
  (if (> n 0) (/ sum n) 2.5)
)

(defun toc:getreal-default (msg def / val)
  (initget 6)
  (setq val (getreal (strcat msg " <" (rtos def 2 2) ">: ")))
  (if val val def)
)

(defun toc:getkword-default (msg opts def / val)
  (initget opts)
  (setq val (getkword (strcat msg " <" def ">: ")))
  (if val val def)
)

(defun toc:getstring-default (msg def / val)
  (setq val (getstring T (strcat msg " <" def ">: ")))
  (if (= val "") def val)
)

(defun toc:bool-text (v)
  (if v "Yes" "No")
)

(defun toc:show-config ()
  (toc:init-config)
  (princ "\n--- TOC config ---")
  (princ (strcat "\nDWG patterns     : " *toc-dwgno-patterns*))
  (princ (strcat "\nSheet patterns   : " *toc-sheet-patterns*))
  (princ (strcat "\nScale patterns   : " *toc-scale-patterns*))
  (princ (strcat "\nIgnore patterns  : " *toc-ignore-patterns*))
  (princ (strcat "\nDetection default: " *toc-title-mode*))
  (princ (strcat "\nOutput default   : " *toc-output-mode*))
  (princ (strcat "\nScale prefix     : " *toc-paper-prefix*))
  (princ (strcat "\nAuto scale prefix: " (toc:bool-text *toc-auto-format-scale*)))
  (if *toc-form-block-name*
    (princ (strcat "\nForm block       : " *toc-form-block-name*))
  )
  (princ)
)

(defun toc:nearest-sheet (dwg items radius / best bestd d)
  (setq best nil)
  (setq bestd nil)
  (foreach it items
    (if (and (toc:is-sheetno (toc:text it)) (/= it dwg))
      (progn
        (setq d (toc:dist dwg it))
        (if (and (<= d radius) (or (null bestd) (< d bestd)))
          (progn
            (setq best it)
            (setq bestd d)
          )
        )
      )
    )
  )
  best
)

(defun toc:nearest-scale (dwg items radius / best bestd d)
  (setq best nil)
  (setq bestd nil)
  (foreach it items
    (if (and (toc:is-scale (toc:text it)) (/= it dwg))
      (progn
        (setq d (toc:dist dwg it))
        (if (and (<= d radius) (or (null bestd) (< d bestd)))
          (progn
            (setq best it)
            (setq bestd d)
          )
        )
      )
    )
  )
  best
)

(defun toc:same-row-p (a b rowtol)
  (<= (abs (- (toc:y a) (toc:y b))) rowtol)
)

(defun toc:same-row-sheet (dwg items radius rowtol / best bestdx dx)
  (setq best nil)
  (setq bestdx nil)
  (foreach it items
    (if
      (and
        (toc:is-sheetno (toc:text it))
        (/= it dwg)
        (toc:same-row-p dwg it rowtol)
        (< (toc:x it) (toc:x dwg))
      )
      (progn
        (setq dx (- (toc:x dwg) (toc:x it)))
        (if (and (<= dx (* radius 4.0)) (or (null bestdx) (< dx bestdx)))
          (progn
            (setq best it)
            (setq bestdx dx)
          )
        )
      )
    )
  )
  best
)

(defun toc:same-row-scale (dwg items radius rowtol / best bestdx dx)
  (setq best nil)
  (setq bestdx nil)
  (foreach it items
    (if
      (and
        (toc:is-scale (toc:text it))
        (/= it dwg)
        (toc:same-row-p dwg it rowtol)
        (> (toc:x it) (toc:x dwg))
      )
      (progn
        (setq dx (- (toc:x it) (toc:x dwg)))
        (if (and (<= dx (* radius 8.0)) (or (null bestdx) (< dx bestdx)))
          (progn
            (setq best it)
            (setq bestdx dx)
          )
        )
      )
    )
  )
  best
)

(defun toc:sort-items-by-x (items)
  (vl-sort items '(lambda (a b) (< (toc:x a) (toc:x b))))
)

(defun toc:join-title-items (items / out)
  (setq out "")
  (foreach it items
    (if (= out "")
      (setq out (toc:text it))
      (setq out (strcat out " " (toc:text it)))
    )
  )
  (toc:clean out)
)

(defun toc:same-row-title (dwg items scale radius rowtol / stopx candidates)
  (setq stopx nil)
  (if (and scale (> (toc:x scale) (toc:x dwg)))
    (setq stopx (toc:x scale))
  )
  (setq candidates '())
  (foreach it items
    (if
      (and
        (toc:is-title-candidate it)
        (/= it dwg)
        (toc:same-row-p dwg it rowtol)
        (> (toc:x it) (toc:x dwg))
        (<= (- (toc:x it) (toc:x dwg)) (* radius 8.0))
        (or (null stopx) (< (toc:x it) stopx))
      )
      (setq candidates (cons it candidates))
    )
  )
  (if candidates
    (toc:make-item
      (toc:join-title-items (toc:sort-items-by-x candidates))
      (list (toc:x (car (toc:sort-items-by-x candidates))) (toc:y dwg) 0.0)
      (toc:h dwg)
    )
  )
)

(defun toc:title-score (dwg it radius / d score dx dy)
  (setq d (toc:dist dwg it))
  (setq dx (abs (- (toc:x dwg) (toc:x it))))
  (setq dy (- (toc:y it) (toc:y dwg)))
  (setq score (- 1000.0 d))
  (if (> dy 0.0) (setq score (+ score 160.0)))
  (if (< dx radius) (setq score (+ score 40.0)))
  (setq score (+ score (* 12.0 (toc:h it))))
  score
)

(defun toc:best-title (dwg items radius / best bestscore score title-radius)
  (setq best nil)
  (setq bestscore nil)
  (setq title-radius (* radius 2.0))
  (foreach it items
    (if (and
          (toc:is-title-candidate it)
          (/= it dwg)
          (<= (toc:dist dwg it) title-radius)
        )
      (progn
        (setq score (toc:title-score dwg it radius))
        (if (or (null bestscore) (> score bestscore))
          (progn
            (setq best it)
            (setq bestscore score)
          )
        )
      )
    )
  )
  best
)

(defun toc:format-scale (s / u c)
  (toc:init-config)
  (setq c (toc:clean s))
  (setq c (toc:str-replace "SCALE" "" c))
  (setq c (toc:str-replace "scale" "" c))
  (setq c (vl-string-trim " :\t\r\n" c))
  (setq u (toc:compact c))
  (cond
    ((= u "") "")
    ((= u "NONE") "NONE")
    ((wcmatch u "A#:*") c)
    ((and *toc-auto-format-scale* (vl-string-search "/" c)) (strcat *toc-paper-prefix* c))
    (T c)
  )
)

(defun toc:row-sheet (r) (nth 0 r))
(defun toc:row-dwg (r) (nth 1 r))
(defun toc:row-title (r) (nth 2 r))
(defun toc:row-scale (r) (nth 3 r))
(defun toc:row-x (r) (nth 4 r))
(defun toc:row-y (r) (nth 5 r))

(defun toc:string< (a b / la lb i ca cb)
  (setq a (vl-princ-to-string a))
  (setq b (vl-princ-to-string b))
  (setq la (strlen a))
  (setq lb (strlen b))
  (setq i 1)
  (while
    (and
      (<= i la)
      (<= i lb)
      (= (ascii (substr a i 1)) (ascii (substr b i 1)))
    )
    (setq i (1+ i))
  )
  (cond
    ((> i la) (< la lb))
    ((> i lb) nil)
    (T
      (setq ca (ascii (substr a i 1)))
      (setq cb (ascii (substr b i 1)))
      (< ca cb)
    )
  )
)

(defun toc:row-sort< (a b / as bs)
  (setq as (atoi (toc:row-sheet a)))
  (setq bs (atoi (toc:row-sheet b)))
  (cond
    ((and (> as 0) (> bs 0) (/= as bs)) (< as bs))
    ((and (/= (toc:row-y a) (toc:row-y b)) (or (= as 0) (= bs 0)))
      (> (toc:row-y a) (toc:row-y b))
    )
    ((/= (toc:row-x a) (toc:row-x b)) (< (toc:row-x a) (toc:row-x b)))
    (T (toc:string< (toc:row-dwg a) (toc:row-dwg b)))
  )
)

(defun toc:merge-better-row (old new)
  (cond
    ((and (= (toc:row-title old) "??") (/= (toc:row-title new) "??")) new)
    ((and (= (toc:row-scale old) "") (/= (toc:row-scale new) "")) new)
    ((and (= (toc:row-sheet old) "") (/= (toc:row-sheet new) "")) new)
    (T old)
  )
)

(defun toc:dedupe-rows (rows / pairs key found old out)
  (setq pairs '())
  (foreach r rows
    (setq key (toc:compact (toc:row-dwg r)))
    (setq found (assoc key pairs))
    (if found
      (progn
        (setq old (cdr found))
        (setq pairs (subst (cons key (toc:merge-better-row old r)) found pairs))
      )
      (setq pairs (cons (cons key r) pairs))
    )
  )
  (foreach p pairs
    (setq out (cons (cdr p) out))
  )
  out
)

(defun toc:detect-near-rows (items radius rowtol / dwgs rows sheet scale title rowsheet rowscale rowtitle)
  (setq dwgs (vl-remove-if-not '(lambda (x) (toc:is-dwgno (toc:text x))) items))
  (setq rows '())
  (foreach dwg dwgs
    (setq rowsheet (toc:same-row-sheet dwg items radius rowtol))
    (setq rowscale (toc:same-row-scale dwg items radius rowtol))
    (setq rowtitle (toc:same-row-title dwg items rowscale radius rowtol))
    (setq sheet (if rowsheet rowsheet (toc:nearest-sheet dwg items radius)))
    (setq scale (if rowscale rowscale (toc:nearest-scale dwg items radius)))
    (setq title (if rowtitle rowtitle (toc:best-title dwg items radius)))
    (setq rows
      (cons
        (list
          (if sheet (toc:text sheet) "")
          (toc:compact (toc:text dwg))
          (if title (toc:text title) "??")
          (if scale (toc:format-scale (toc:text scale)) "")
          (toc:x dwg)
          (toc:y dwg)
        )
        rows
      )
    )
  )
  (vl-sort (toc:dedupe-rows rows) 'toc:row-sort<)
)

(defun toc:first-after-x (anchor items pred maxdx rowtol / best bestdx dx)
  (setq best nil)
  (setq bestdx nil)
  (foreach it items
    (if
      (and
        (/= it anchor)
        (toc:same-row-p anchor it rowtol)
        (> (toc:x it) (toc:x anchor))
        (apply pred (list (toc:text it)))
      )
      (progn
        (setq dx (- (toc:x it) (toc:x anchor)))
        (if (and (<= dx maxdx) (or (null bestdx) (< dx bestdx)))
          (progn
            (setq best it)
            (setq bestdx dx)
          )
        )
      )
    )
  )
  best
)

(defun toc:last-before-x (anchor items pred maxdx rowtol / best bestdx dx)
  (setq best nil)
  (setq bestdx nil)
  (foreach it items
    (if
      (and
        (/= it anchor)
        (toc:same-row-p anchor it rowtol)
        (< (toc:x it) (toc:x anchor))
        (apply pred (list (toc:text it)))
      )
      (progn
        (setq dx (- (toc:x anchor) (toc:x it)))
        (if (and (<= dx maxdx) (or (null bestdx) (< dx bestdx)))
          (progn
            (setq best it)
            (setq bestdx dx)
          )
        )
      )
    )
  )
  best
)

(defun toc:title-between (anchor items left right maxdx rowtol / lx rx candidates sorted txt)
  (setq lx (if left (toc:x left) (toc:x anchor)))
  (setq rx (if right (toc:x right) (+ (toc:x anchor) maxdx)))
  (setq candidates '())
  (foreach it items
    (if
      (and
        (/= it anchor)
        (or (null left) (/= it left))
        (or (null right) (/= it right))
        (toc:is-title-candidate it)
        (toc:same-row-p anchor it rowtol)
        (> (toc:x it) lx)
        (< (toc:x it) rx)
        (<= (- (toc:x it) (toc:x anchor)) maxdx)
      )
      (setq candidates (cons it candidates))
    )
  )
  (if candidates
    (progn
      (setq sorted (toc:sort-items-by-x candidates))
      (setq txt (toc:join-title-items sorted))
      (if (toc:is-good-title txt)
        (toc:make-item txt (list (toc:x (car sorted)) (toc:y anchor) 0.0) (toc:h anchor))
      )
    )
  )
)

(defun toc:detect-table-rows (items radius rowtol / sheets rows dwg scale title)
  (setq sheets (vl-remove-if-not '(lambda (x) (toc:is-sheetno (toc:text x))) items))
  (setq rows '())
  (foreach sheet sheets
    (setq dwg (toc:first-after-x sheet items 'toc:is-dwgno (* radius 5.0) rowtol))
    (if dwg
      (progn
        (setq scale (toc:first-after-x dwg items 'toc:is-scale (* radius 10.0) rowtol))
        (if (null scale)
          (setq scale (toc:first-after-x sheet items 'toc:is-scale (* radius 12.0) rowtol))
        )
        (setq title (toc:title-between sheet items dwg scale (* radius 10.0) rowtol))
        (if (null title)
          (setq title (toc:same-row-title dwg items scale radius rowtol))
        )
        (setq rows
          (cons
            (list
              (toc:text sheet)
              (toc:compact (toc:text dwg))
              (if title (toc:text title) "??")
              (if scale (toc:format-scale (toc:text scale)) "")
              (toc:x sheet)
              (toc:y sheet)
            )
            rows
          )
        )
      )
    )
  )
  (vl-sort (toc:dedupe-rows rows) 'toc:row-sort<)
)

(defun toc:detect-rows (items radius rowtol mode / rows)
  (toc:init-config)
  (cond
    ((= mode "Near") (toc:detect-near-rows items radius rowtol))
    ((= mode "Auto")
      (setq rows (toc:detect-table-rows items radius rowtol))
      (if rows rows (toc:detect-near-rows items radius rowtol))
    )
    (T (toc:detect-table-rows items radius rowtol))
  )
)

(defun toc:preview-rows (rows / i)
  (setq i 0)
  (princ (strcat "\nRecognized rows: " (itoa (length rows))))
  (foreach r rows
    (if (< i 12)
      (princ
        (strcat
          "\n  "
          (toc:row-sheet r)
          " | "
          (toc:row-dwg r)
          " | "
          (toc:row-title r)
          " | "
          (toc:row-scale r)
        )
      )
    )
    (setq i (1+ i))
  )
  (if (> (length rows) 12)
    (princ (strcat "\n  ... +" (itoa (- (length rows) 12)) " more"))
  )
)

(defun toc:current-space (doc)
  (if (= 1 (getvar "TILEMODE"))
    (vla-get-ModelSpace doc)
    (if (= 1 (getvar "CVPORT"))
      (vla-get-PaperSpace doc)
      (vla-get-ModelSpace doc)
    )
  )
)

(defun toc:make-table (rows / doc sp pt rh c1 c2 c3 c4 tbl r)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq sp (toc:current-space doc))
  (setq pt (getpoint "\nTable insertion point: "))
  (if pt
    (progn
      (setq rh (toc:getreal-default "\nRow height" 8.0))
      (setq c1 (toc:getreal-default "\nColumn width SHEET" 22.0))
      (setq c2 (toc:getreal-default "\nColumn width DWG NO" 32.0))
      (setq c3 (toc:getreal-default "\nColumn width TITLE" 110.0))
      (setq c4 (toc:getreal-default "\nColumn width SCALE" 34.0))
      (setq tbl
        (vla-AddTable
          sp
          (vlax-3d-point pt)
          (+ (length rows) 1)
          4
          rh
          c3
        )
      )
      (vla-SetColumnWidth tbl 0 c1)
      (vla-SetColumnWidth tbl 1 c2)
      (vla-SetColumnWidth tbl 2 c3)
      (vla-SetColumnWidth tbl 3 c4)
      (vla-SetText tbl 0 0 "SHEET")
      (vla-SetText tbl 0 1 "DWG NO")
      (vla-SetText tbl 0 2 "TITLE")
      (vla-SetText tbl 0 3 "SCALE")
      (setq r 1)
      (foreach row rows
        (vla-SetText tbl r 0 (toc:row-sheet row))
        (vla-SetText tbl r 1 (toc:row-dwg row))
        (vla-SetText tbl r 2 (toc:row-title row))
        (vla-SetText tbl r 3 (toc:row-scale row))
        (setq r (1+ r))
      )
      (princ "\nTOCAUTO table created.")
    )
  )
)

(defun toc:entmake-text (pt ht val)
  (if (and pt val)
    (entmake
      (list
        '(0 . "TEXT")
        (cons 8 (getvar "CLAYER"))
        (cons 7 (getvar "TEXTSTYLE"))
        (cons 10 pt)
        (cons 40 ht)
        (cons 1 val)
        '(50 . 0.0)
      )
    )
  )
)

(defun toc:row-point (base y)
  (list (car base) y (if (caddr base) (caddr base) 0.0))
)

(defun toc:write-text (rows / p1 p2 p3 p4 gap ht y)
  (setq p1 (getpoint "\nPick first row SHEET text point: "))
  (if p1
    (progn
      (setq p2 (getpoint "\nPick first row DWG NO text point: "))
      (setq p3 (getpoint "\nPick first row TITLE text point: "))
      (setq p4 (getpoint "\nPick first row SCALE text point: "))
      (if (and p2 p3 p4)
        (progn
          (setq gap (toc:getreal-default "\nRow gap downward" 8.0))
          (setq ht (toc:getreal-default "\nText height" (getvar "TEXTSIZE")))
          (setq y (cadr p1))
          (foreach row rows
            (toc:entmake-text (toc:row-point p1 y) ht (toc:row-sheet row))
            (toc:entmake-text (toc:row-point p2 y) ht (toc:row-dwg row))
            (toc:entmake-text (toc:row-point p3 y) ht (toc:row-title row))
            (toc:entmake-text (toc:row-point p4 y) ht (toc:row-scale row))
            (setq y (- y gap))
          )
          (princ "\nTOCAUTO texts created.")
        )
      )
    )
  )
)

(defun toc:inside-box-p (it p1 p2 / minx maxx miny maxy x y)
  (setq minx (min (car p1) (car p2)))
  (setq maxx (max (car p1) (car p2)))
  (setq miny (min (cadr p1) (cadr p2)))
  (setq maxy (max (cadr p1) (cadr p2)))
  (setq x (toc:x it))
  (setq y (toc:y it))
  (and
    (>= x minx)
    (<= x maxx)
    (>= y miny)
    (<= y maxy)
  )
)

(defun toc:exclude-window (items / ans p1 p2 before)
  (setq ans (toc:getkword-default "\nExclude an area from scan? [No/Yes]" "No Yes" "No"))
  (if (= ans "Yes")
    (progn
      (setq p1 (getpoint "\nPick first corner of old TOC/list area: "))
      (if p1
        (progn
          (setq p2 (getcorner p1 "\nPick opposite corner: "))
          (if p2
            (progn
              (setq before (length items))
              (setq items
                (vl-remove-if
                  '(lambda (it) (toc:inside-box-p it p1 p2))
                  items
                )
              )
              (princ
                (strcat
                  "\nExcluded "
                  (itoa (- before (length items)))
                  " text items."
                )
              )
            )
          )
        )
      )
    )
  )
  items
)

(defun toc:exclude-window-default (items msg def / ans p1 p2 before)
  (setq ans (toc:getkword-default msg "Yes No" def))
  (if (= ans "Yes")
    (progn
      (setq p1 (getpoint "\nPick first corner of area to exclude: "))
      (if p1
        (progn
          (setq p2 (getcorner p1 "\nPick opposite corner: "))
          (if p2
            (progn
              (setq before (length items))
              (setq items
                (vl-remove-if
                  '(lambda (it) (toc:inside-box-p it p1 p2))
                  items
                )
              )
              (princ
                (strcat
                  "\nExcluded "
                  (itoa (- before (length items)))
                  " text items."
                )
              )
            )
          )
        )
      )
    )
  )
  items
)

(defun toc:run-with-mode (items mode / oldforce)
  (setq oldforce *toc-force-detect-mode*)
  (setq *toc-force-detect-mode* mode)
  (toc:process-items items)
  (setq *toc-force-detect-mode* oldforce)
  (princ)
)

(defun toc:process-items (items / avg radius rowtol rows proceed mode detectmode dwgcount)
  (toc:init-config)
  (if (null items)
    (princ "\nNo readable TEXT/MTEXT/ATTRIB found.")
    (progn
      (setq dwgcount
        (length
          (vl-remove-if-not
            '(lambda (x) (toc:is-dwgno (toc:text x)))
            items
          )
        )
      )
      (princ
        (strcat
          "\nReadable text items: "
          (itoa (length items))
          " / DWG number candidates: "
          (itoa dwgcount)
        )
      )
      (setq avg (toc:avg-height items))
      (setq radius (toc:getreal-default "\nNearby search radius" (max 80.0 (* avg 80.0))))
      (setq rowtol (toc:getreal-default "\nSame-row title tolerance" (max 2.0 (* avg 1.6))))
      (if *toc-force-detect-mode*
        (progn
          (setq detectmode *toc-force-detect-mode*)
          (princ (strcat "\nDetection mode: " detectmode))
        )
        (setq detectmode (toc:getkword-default "\nDetection mode [Table/Near/Auto]" "Table Near Auto" *toc-title-mode*))
      )
      (setq rows (toc:detect-rows items radius rowtol detectmode))
      (if (null rows)
        (princ "\nNo DWG number pattern found. Expected examples: FI-002, DT-015, L-011.")
        (progn
          (toc:preview-rows rows)
          (setq proceed (toc:getkword-default "\nCreate output? [Yes/No]" "Yes No" "Yes"))
          (if (= proceed "Yes")
            (progn
              (setq mode (toc:getkword-default "\nOutput mode [Table/Text]" "Table Text" *toc-output-mode*))
              (if (= mode "Table")
                (toc:make-table rows)
                (toc:write-text rows)
              )
            )
          )
        )
      )
    )
  )
)

(defun toc:ss-all-current (/ ss)
  (ssget "_X" '((0 . "TEXT,MTEXT,ATTRIB,INSERT")))
)

(defun toc:ss-text-all (/ ss)
  (ssget "_X" '((0 . "TEXT,MTEXT,ATTRIB,INSERT")))
)

(defun toc:ss-insert-all (/ ss)
  (ssget "_X" '((0 . "INSERT")))
)

(defun toc:pick-form-block (/ en obj)
  (while
    (progn
      (setq en (car (entsel "\nStep 1/5: Select one repeated sheet/title form BLOCK: ")))
      (cond
        ((null en) nil)
        (T
          (setq obj (vlax-ename->vla-object en))
          (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
            nil
            (progn
              (princ "\nSelected object is not a block reference.")
              T
            )
          )
        )
      )
    )
  )
  obj
)

(defun toc:pick-field-box (label bb / p1 p2 nb)
  (princ (strcat "\nStep: draw a window around the VALUE CELL for " label "."))
  (princ "\nDo not pick the whole form here; box only the cell where that changing value appears.")
  (setq p1 (getpoint "\nPick first corner of that cell/window: "))
  (if p1
    (progn
      (setq p2 (getcorner p1 "\nPick opposite corner of that cell/window: "))
      (if p2
        (progn
          (setq nb (toc:norm-box bb p1 p2))
          (if nb
            nb
            (progn
              (princ "\nCould not calculate relative field window.")
              nil
            )
          )
        )
      )
    )
  )
)

(defun toc:form-field-box (field)
  (cdr (assoc field *toc-form-fields*))
)

(defun toc:sort-items-reading (items)
  (vl-sort
    items
    '(lambda (a b)
      (if (/= (toc:y a) (toc:y b))
        (> (toc:y a) (toc:y b))
        (< (toc:x a) (toc:x b))
      )
    )
  )
)

(defun toc:join-items-raw (items / out)
  (setq out "")
  (foreach it (toc:sort-items-reading items)
    (if (= out "")
      (setq out (toc:text it))
      (setq out (strcat out " " (toc:text it)))
    )
  )
  (toc:clean out)
)

(defun toc:items-in-form-field (field bb items / nbox box)
  (setq nbox (toc:form-field-box field))
  (if nbox
    (progn
      (setq box (toc:denorm-box bb nbox))
      (vl-remove-if-not
        '(lambda (it) (toc:item-in-raw-box-p it box))
        items
      )
    )
  )
)

(defun toc:best-field-text (field bb items / found filtered)
  (setq found (toc:items-in-form-field field bb items))
  (cond
    ((null found) "")
    ((= field "SHEET")
      (setq filtered (vl-remove-if-not '(lambda (it) (toc:is-sheetno (toc:text it))) found))
      (if filtered (toc:text (car filtered)) (toc:join-items-raw found))
    )
    ((= field "DWG")
      (setq filtered (vl-remove-if-not '(lambda (it) (toc:is-dwgno (toc:text it))) found))
      (if filtered (toc:compact (toc:text (car filtered))) (toc:join-items-raw found))
    )
    ((= field "SCALE")
      (setq filtered (vl-remove-if-not '(lambda (it) (toc:is-scale (toc:text it))) found))
      (if filtered (toc:format-scale (toc:text (car filtered))) (toc:format-scale (toc:join-items-raw found)))
    )
    (T
      (setq filtered (vl-remove-if-not 'toc:is-title-candidate found))
      (if filtered (toc:join-items-raw filtered) (toc:join-items-raw found))
    )
  )
)

(defun toc:form-blocks (/ ss i obj name out)
  (setq out '())
  (if (and *toc-form-block-name* (setq ss (toc:ss-insert-all)))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (setq name (toc:effective-name obj))
        (if (= (strcase name) (strcase *toc-form-block-name*))
          (setq out (cons obj out))
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse out)
)

(defun toc:scan-form-rows (/ ss items blocks bb sheet dwg title scale rows)
  (setq rows '())
  (setq ss (toc:ss-text-all))
  (if ss
    (setq items (toc:items-from-ss ss))
    (setq items '())
  )
  (setq blocks (toc:form-blocks))
  (foreach blk blocks
    (setq bb (toc:get-bbox blk))
    (if bb
      (progn
        (setq sheet (toc:best-field-text "SHEET" bb items))
        (setq dwg (toc:best-field-text "DWG" bb items))
        (setq title (toc:best-field-text "TITLE" bb items))
        (setq scale (toc:best-field-text "SCALE" bb items))
        (if (or (/= sheet "") (/= dwg "") (/= title "") (/= scale ""))
          (setq rows
            (cons
              (list
                sheet
                dwg
                (if (/= title "") title "??")
                scale
                (toc:bbox-minx bb)
                (toc:bbox-miny bb)
              )
              rows
            )
          )
        )
      )
    )
  )
  (vl-sort (toc:dedupe-rows rows) 'toc:row-sort<)
)

(defun toc:form-ready-p ()
  (and
    *toc-form-block-name*
    (toc:form-field-box "SHEET")
    (toc:form-field-box "DWG")
    (toc:form-field-box "TITLE")
    (toc:form-field-box "SCALE")
  )
)

(defun c:TOCAUTO (/ ss items)
  (vl-load-com)
  (toc:init-config)
  (princ "\nTOCAUTO - select title-block/text objects. Avoid old TOC if possible.")
  (setq ss (ssget '((0 . "TEXT,MTEXT,ATTRIB,INSERT"))))
  (if ss
    (progn
      (setq items (toc:items-from-ss ss))
      (toc:process-items items)
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun c:TOCAUTOALL (/ ss items)
  (vl-load-com)
  (toc:init-config)
  (princ "\nTOCAUTOALL - scanning all TEXT/MTEXT/ATTRIB/INSERT objects in the whole DWG.")
  (setq ss (toc:ss-all-current))
  (if ss
    (progn
      (setq items (toc:items-from-ss ss))
      (setq items (toc:exclude-window items))
      (toc:process-items items)
    )
    (princ "\nNo TEXT/MTEXT/ATTRIB/INSERT objects found.")
  )
  (princ)
)

(defun c:TOCTABLESCAN (/ ss items oldmode)
  (vl-load-com)
  (toc:init-config)
  (setq oldmode *toc-title-mode*)
  (setq *toc-title-mode* "Table")
  (princ "\nTOCTABLESCAN - select an existing list/table area.")
  (setq ss (ssget '((0 . "TEXT,MTEXT,ATTRIB,INSERT"))))
  (if ss
    (progn
      (setq items (toc:items-from-ss ss))
      (toc:process-items items)
    )
    (princ "\nNothing selected.")
  )
  (setq *toc-title-mode* oldmode)
  (princ)
)

(defun c:TOCNEARSCAN (/ ss items oldmode)
  (vl-load-com)
  (toc:init-config)
  (setq oldmode *toc-title-mode*)
  (setq *toc-title-mode* "Near")
  (princ "\nTOCNEARSCAN - select title-block/drawing-title area texts.")
  (setq ss (ssget '((0 . "TEXT,MTEXT,ATTRIB,INSERT"))))
  (if ss
    (progn
      (setq items (toc:items-from-ss ss))
      (toc:process-items items)
    )
    (princ "\nNothing selected.")
  )
  (setq *toc-title-mode* oldmode)
  (princ)
)

(defun c:TOCSEMI (/ ss items)
  (vl-load-com)
  (toc:init-config)
  (princ "\nTOCSEMI - select finished drawing/title-block text areas. Do not select the old drawing list.")
  (setq ss (ssget '((0 . "TEXT,MTEXT,ATTRIB,INSERT"))))
  (if ss
    (progn
      (setq items (toc:items-from-ss ss))
      (toc:run-with-mode items "Near")
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun c:TOCSEMIALL (/ ss items)
  (vl-load-com)
  (toc:init-config)
  (princ "\nTOCSEMIALL - scan the whole DWG and read finished drawing/title-block text.")
  (setq ss (toc:ss-all-current))
  (if ss
    (progn
      (setq items (toc:items-from-ss ss))
      (setq items
        (toc:exclude-window-default
          items
          "\nExclude existing drawing list/table area? [Yes/No]"
          "Yes"
        )
      )
      (toc:run-with-mode items "Near")
    )
    (princ "\nNo TEXT/MTEXT/ATTRIB/INSERT objects found.")
  )
  (princ)
)

(defun c:TOCFORMSET (/ blk bb sheet dwg title scale)
  (vl-load-com)
  (toc:init-config)
  (setq blk (toc:pick-form-block))
  (if blk
    (progn
      (setq bb (toc:get-bbox blk))
      (if bb
        (progn
          (setq *toc-form-block-name* (toc:effective-name blk))
          (princ (strcat "\nSheet form block: " *toc-form-block-name*))
          (setq sheet (toc:pick-field-box "SHEET NO" bb))
          (setq dwg (toc:pick-field-box "DWG NO" bb))
          (setq title (toc:pick-field-box "TITLE" bb))
          (setq scale (toc:pick-field-box "SCALE" bb))
          (if (and sheet dwg title scale)
            (progn
              (setq *toc-form-fields*
                (list
                  (cons "SHEET" sheet)
                  (cons "DWG" dwg)
                  (cons "TITLE" title)
                  (cons "SCALE" scale)
                )
              )
              (princ "\nTOCFORMSET complete. Run TOCFORMSCAN.")
            )
            (princ "\nTOCFORMSET canceled or incomplete.")
          )
        )
        (princ "\nCould not read selected block bounding box.")
      )
    )
    (princ "\nNothing selected.")
  )
  (princ)
)

(defun c:TOCFORMSCAN (/ rows proceed mode)
  (vl-load-com)
  (toc:init-config)
  (if (not (toc:form-ready-p))
    (princ "\nForm scan is not calibrated. Run TOCFORMSET first.")
    (progn
      (setq rows (toc:scan-form-rows))
      (if rows
        (progn
          (toc:preview-rows rows)
          (setq proceed (toc:getkword-default "\nCreate output? [Yes/No]" "Yes No" "Yes"))
          (if (= proceed "Yes")
            (progn
              (setq mode (toc:getkword-default "\nOutput mode [Table/Text]" "Table Text" *toc-output-mode*))
              (if (= mode "Table")
                (toc:make-table rows)
                (toc:write-text rows)
              )
            )
          )
        )
        (princ "\nNo rows found from calibrated sheet forms.")
      )
    )
  )
  (princ)
)

(defun c:TOCCFG (/ action yn)
  (vl-load-com)
  (toc:init-config)
  (setq action (toc:getkword-default "\nTOC config [Show/Edit/Reset]" "Show Edit Reset" "Show"))
  (cond
    ((= action "Reset")
      (setq *toc-dwgno-patterns* nil)
      (setq *toc-sheet-patterns* nil)
      (setq *toc-scale-patterns* nil)
      (setq *toc-ignore-patterns* nil)
      (setq *toc-title-mode* nil)
      (setq *toc-output-mode* nil)
      (setq *toc-paper-prefix* nil)
      (setq *toc-auto-format-scale* nil)
      (toc:init-config)
      (princ "\nTOC config reset to defaults.")
      (toc:show-config)
    )
    ((= action "Edit")
      (setq *toc-dwgno-patterns*
        (toc:getstring-default "\nDWG number wcmatch patterns" *toc-dwgno-patterns*)
      )
      (setq *toc-sheet-patterns*
        (toc:getstring-default "\nSheet number wcmatch patterns" *toc-sheet-patterns*)
      )
      (setq *toc-scale-patterns*
        (toc:getstring-default "\nScale wcmatch patterns" *toc-scale-patterns*)
      )
      (setq *toc-ignore-patterns*
        (toc:getstring-default "\nIgnore title/label wcmatch patterns" *toc-ignore-patterns*)
      )
      (setq *toc-title-mode*
        (toc:getkword-default "\nDefault detection [Table/Near/Auto]" "Table Near Auto" *toc-title-mode*)
      )
      (setq *toc-output-mode*
        (toc:getkword-default "\nDefault output [Table/Text]" "Table Text" *toc-output-mode*)
      )
      (setq *toc-paper-prefix*
        (toc:getstring-default "\nScale prefix for raw 1/50 text" *toc-paper-prefix*)
      )
      (setq yn
        (toc:getkword-default
          "\nAuto-add scale prefix? [Yes/No]"
          "Yes No"
          (if *toc-auto-format-scale* "Yes" "No")
        )
      )
      (setq *toc-auto-format-scale* (= yn "Yes"))
      (toc:show-config)
    )
    (T
      (toc:show-config)
    )
  )
  (princ)
)

(princ "\nLoaded cad_toc_auto.lsp. Commands: TOCFORMSET, TOCFORMSCAN, TOCSEMI, TOCSEMIALL, TOCCFG, TOCAUTO, TOCAUTOALL, TOCTABLESCAN, TOCNEARSCAN")
(princ)
