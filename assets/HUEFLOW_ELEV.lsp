(vl-load-com)

;; =====================================================
;; HUEFLOW_ELEV.lsp  —  명령어: HF
;; 개구부 입력 제거 / 시작점 클릭 한 번으로 즉시 생성
;; 벽 꺾임 지점에 ▽ 삼각형 마커 표시
;; =====================================================

;; LINE 생성
(defun hf:make-line (x1 y1 x2 y2 layer /)
  (entmake
    (list
      (cons 0  "LINE")
      (cons 8  layer)
      (cons 10 (list x1 y1 0.0))
      (cons 11 (list x2 y2 0.0))
    )
  )
)

;; SOLID(채운 삼각형) 생성 — ▽ 뒤집힌 삼각형
;; 꼭짓점(tip)이 천정선(ty)에 닿고, 위로 퍼지는 형태
(defun hf:make-triangle-down (cx ty size layer / top hw)
  ;; ty  = 꼭짓점 y (천정선에 딱 닿음)
  ;; top = 삼각형 윗변 y
  (setq top (+ ty size))
  (setq hw  (* size 0.5))   ; 반폭 (슬림하게)
  (entmake
    (list
      (cons 0  "SOLID")
      (cons 8  layer)
      (cons 10 (list (- cx hw) top 0.0))  ; 좌상
      (cons 11 (list (+ cx hw) top 0.0))  ; 우상
      (cons 12 (list cx        ty  0.0))  ; 하단 꼭짓점 (천정선)
      (cons 13 (list cx        ty  0.0))  ; 동일점 반복
    )
  )
)

;; DIMSTYLE 리스트
(defun hf:get-dimstyle-list (/ rec out)
  (setq out '())
  (setq rec (tblnext "DIMSTYLE" T))
  (while rec
    (setq out (cons (cdr (assoc 2 rec)) out))
    (setq rec (tblnext "DIMSTYLE"))
  )
  (reverse out)
)

;; DIMSTYLE 선택
(defun hf:pick-dimstyle (/ lst msg sty done s)
  (setq lst (hf:get-dimstyle-list))
  (setq done nil)
  (setq sty (getvar "DIMSTYLE"))
  (if lst
    (progn
      (setq msg "\n사용 가능한 딤스타일: ")
      (foreach s lst (setq msg (strcat msg s "  ")))
      (princ msg)
      (while (not done)
        (setq sty (getstring T (strcat "\n적용할 딤스타일 입력 <" (getvar "DIMSTYLE") ">: ")))
        (if (= sty "")
          (progn (setq sty (getvar "DIMSTYLE")) (setq done T))
          (if (tblsearch "DIMSTYLE" sty)
            (setq done T)
            (princ "\n존재하지 않는 딤스타일입니다.")
          )
        )
      )
    )
  )
  sty
)

;; 폴리라인 정점 추출
(defun hf:get-verts (ent / obj coords pts)
  (setq obj (vlax-ename->vla-object ent))
  (if (= (vla-get-ObjectName obj) "AcDbPolyline")
    (progn
      (setq coords (vlax-safearray->list
                     (vlax-variant-value (vla-get-Coordinates obj))))
      (setq pts '())
      (while coords
        (setq pts (append pts (list (list (car coords) (cadr coords)))))
        (setq coords (cddr coords))
      )
      ;; 닫힌 폴리선이면 마지막 중복점 제거
      (if (and (> (length pts) 2)
               (equal (car pts) (car (last pts)) 1e-6))
        (setq pts (reverse (cdr (reverse pts))))
      )
      pts
    )
    nil
  )
)

;; 리스트 회전 (시작점 기준 재정렬)
(defun hf:rotate-list (lst idx / out i n)
  (setq out '())
  (setq i 0)
  (setq n (length lst))
  (while (< i n)
    (setq out (append out (list (nth (rem (+ idx i) n) lst))))
    (setq i (1+ i))
  )
  out
)

;; 가장 가까운 정점 index
(defun hf:nearest-vertex-index (pt verts / i best-dist best-idx d)
  (setq i 0) (setq best-dist 1e99) (setq best-idx 0)
  (while (< i (length verts))
    (setq d (distance pt (nth i verts)))
    (if (< d best-dist)
      (progn (setq best-dist d) (setq best-idx i))
    )
    (setq i (1+ i))
  )
  best-idx
)

;; 레이어 생성
(defun hf:ensure-layer (name color /)
  (if (not (tblsearch "LAYER" name))
    (command "_.-LAYER" "_M" name "_C" color name "")
    (command "_.-LAYER" "_C" color name "")  ; 이미 있어도 색 강제 업데이트
  )
)

;; 수평 치수
(defun hf:dim-h (x1 y1 x2 y2 xt yt dsty layer / oldos oldlay)
  (setq oldos  (getvar "OSMODE"))
  (setq oldlay (getvar "CLAYER"))
  (setvar "OSMODE" 0)
  (setvar "CLAYER" layer)
  (command "_.DIMSTYLE" "_R" dsty)
  (command "_.DIMLINEAR"
    (strcat (rtos x1 2 4) "," (rtos y1 2 4))
    (strcat (rtos x2 2 4) "," (rtos y2 2 4))
    (strcat (rtos xt 2 4) "," (rtos yt 2 4)))
  (setvar "OSMODE" oldos)
  (setvar "CLAYER" oldlay)
)

;; 수직 치수
(defun hf:dim-v (x1 y1 x2 y2 xt yt dsty layer / oldos oldlay)
  (setq oldos  (getvar "OSMODE"))
  (setq oldlay (getvar "CLAYER"))
  (setvar "OSMODE" 0)
  (setvar "CLAYER" layer)
  (command "_.DIMSTYLE" "_R" dsty)
  (command "_.DIMLINEAR"
    (strcat (rtos x1 2 4) "," (rtos y1 2 4))
    (strcat (rtos x2 2 4) "," (rtos y2 2 4))
    (strcat (rtos xt 2 4) "," (rtos yt 2 4)))
  (setvar "OSMODE" oldos)
  (setvar "CLAYER" oldlay)
)

;; =========================
;; 메인 명령어 HF
;; =========================
(defun c:HF ( / oldlay oldos oldcmdecho
                ent verts vcount startpt sidx
                i pt1 pt2 wall-len
                dimsty ceilh basept curx basey
                wx1 wx2 wy1 wy2
                tri-size tri-y dscale
                total-width dimy1 dimy2 )

  (setq oldlay     (getvar "CLAYER"))
  (setq oldos      (getvar "OSMODE"))
  (setq oldcmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (hf:ensure-layer "A-ELEV"     "2")  ; 입면선 — 노랑
  (hf:ensure-layer "A-ELEV-DIM" "1")
  (hf:ensure-layer "A-ELEV-MRK" "1")  ; 꺾쇠 마커 — 빨강

  ;; 1. 딤스타일
  (setq dimsty (hf:pick-dimstyle))
  (princ (strcat "\n선택된 딤스타일: " dimsty))

  ;; 2. 천정고
  (setq ceilh (getreal "\n천정고 입력 <2400>: "))
  (if (null ceilh) (setq ceilh 2400.0))

  ;; 삼각형 크기 = 천정고 × 4.5% (기존 3% × 1.5배)
  (setq tri-size (* ceilh 0.045))

  ;; 3. 외곽 폴리라인
  (setq ent (car (entsel "\n외곽 폴리라인 선택: ")))
  (if (null ent)
    (progn (princ "\n취소됨")
      (setvar "CLAYER" oldlay) (setvar "OSMODE" oldos)
      (setvar "CMDECHO" oldcmdecho) (princ) (exit))
  )
  (setq verts (hf:get-verts ent))
  (if (or (null verts) (< (length verts) 3))
    (progn (princ "\n⚠ LWPolyline 외곽선이 아니거나 꼭짓점 추출 실패")
      (setvar "CLAYER" oldlay) (setvar "OSMODE" oldos)
      (setvar "CMDECHO" oldcmdecho) (princ) (exit))
  )

  ;; 4. 시작 꼭짓점 클릭 → 자동으로 가장 가까운 정점 찾아 회전
  (setq startpt (getpoint "\n시작할 벽의 꼭짓점 클릭: "))
  (if (null startpt)
    (progn (princ "\n취소됨")
      (setvar "CLAYER" oldlay) (setvar "OSMODE" oldos)
      (setvar "CMDECHO" oldcmdecho) (princ) (exit))
  )
  (setq sidx  (hf:nearest-vertex-index startpt verts))
  (setq verts (hf:rotate-list verts sidx))
  (setq vcount (length verts))

  ;; 5. 입면 삽입 위치
  (setq basept (getpoint "\n입면이 시작될 위치를 클릭하세요: "))
  (if (null basept)
    (progn (princ "\n취소됨")
      (setvar "CLAYER" oldlay) (setvar "OSMODE" oldos)
      (setvar "CMDECHO" oldcmdecho) (princ) (exit))
  )
  (setq curx  (car  basept))
  (setq basey (cadr basept))

  ;; DIMSCALE 읽어서 치수 간격 보정 (어떤 도면에서도 정확한 간격 유지)
  (setq dscale (getvar "DIMSCALE"))
  (if (or (null dscale) (= dscale 0.0)) (setq dscale 1.0))

  ;; 6. 입면 생성
  (setq i 0)
  (while (< i vcount)
    (setq pt1      (nth i verts))
    (setq pt2      (nth (rem (1+ i) vcount) verts))
    (setq wall-len (distance pt1 pt2))

    (if (>= wall-len 1.0)
      (progn
        (setq wx1 curx)
        (setq wx2 (+ curx wall-len))
        (setq wy1 basey)
        (setq wy2 (+ basey ceilh))

        ;; 벽 외곽 4선
        (hf:make-line wx1 wy1 wx2 wy1 "A-ELEV")  ; 바닥
        (hf:make-line wx1 wy2 wx2 wy2 "A-ELEV")  ; 천정
        (hf:make-line wx1 wy1 wx1 wy2 "A-ELEV")  ; 좌측
        (hf:make-line wx2 wy1 wx2 wy2 "A-ELEV")  ; 우측

        ;; 치수 위치 (실제 도면 좌표 기준 고정값)
        (setq dimy1 (+ wy2 800.0))
        (setq dimy2 (+ wy2 1100.0))

        ;; 벽 폭 치수
        (hf:dim-h wx1 wy2 wx2 wy2
                  (/ (+ wx1 wx2) 2.0) dimy1
                  dimsty "A-ELEV-DIM")

        ;; 천정고 치수 (첫 벽만)
        (if (= i 0)
          (hf:dim-v wx1 wy1 wx1 wy2
                    (- wx1 800.0) (/ (+ wy1 wy2) 2.0)
                    dimsty "A-ELEV-DIM")
        )

        ;; 벽 꺾임 마커 ▽ — 천정선(wy2)에 꼭 붙게
        ;; SOLID의 상단 두 점이 wy2에 딱 맞게: tri-y = wy2
        (if (= i 0)
          (hf:make-triangle-down wx1 wy2 tri-size "A-ELEV-MRK")
        )
        (hf:make-triangle-down wx2 wy2 tri-size "A-ELEV-MRK")

      )
    )

    (setq curx (+ curx wall-len))
    (setq i (1+ i))
  )

  ;; 7. 전체 총치수
  (setq i 0)
  (setq total-width 0.0)
  (while (< i vcount)
    (setq pt1 (nth i verts))
    (setq pt2 (nth (rem (1+ i) vcount) verts))
    (setq total-width (+ total-width (distance pt1 pt2)))
    (setq i (1+ i))
  )
  (hf:dim-h
    (car basept) (+ basey ceilh)
    (+ (car basept) total-width) (+ basey ceilh)
    (+ (car basept) (/ total-width 2.0))
    (+ basey ceilh 1100.0)
    dimsty "A-ELEV-DIM")

  (setvar "CLAYER" oldlay)
  (setvar "OSMODE" oldos)
  (setvar "CMDECHO" oldcmdecho)
  (princ "\n\n✔ HF 입면 생성 완료")
  (princ)
)
