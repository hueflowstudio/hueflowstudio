;;; ============================================================
;;; HF_AREA.lsp v5.0
;;; Hueflow Studio - 폴리라인 면적/평수 자동 표기 + 합계표 생성
;;; 
;;; 사용법:
;;;   1. APPLOAD로 로드
;;;   2. HFA 입력
;;;   3. 폴리라인들 선택 → Enter
;;;   4. 합계표 삽입 위치 클릭
;;;
;;; 결과:
;;;   - 각 폴리라인 오른쪽 아래: 12.81㎡ / 3.88PY
;;;   - 합계표: 선(LINE) + 텍스트(MTEXT)로 직접 그림
;;;     ┌──────┬──────────┬──────────┐
;;;     │ 실명 │ AREA(㎡) │ AREA(PY) │
;;;     ...
;;;
;;; v5.0 변경사항:
;;;   - TABLE 객체 포기 (버그 너무 많음)
;;;   - LINE + MTEXT 조합으로 표 직접 그리기
;;;   - 모든 셀이 100% 표시됨 (보장)
;;; ============================================================

(defun c:HFA ( / ss i ent obj min-pt max-pt 
                 insert-pt area-mm area-m2 area-py
                 text-str old-osmode old-cmdecho
                 layer-name txt-height margin
                 processed skipped txt-style
                 box-width
                 min-width valid-list cur-data
                 table-pt total-m2 total-py
                 area-data-list idx
                 tbl-txt-h cell-w cell-h
                 total-rows total-cols
                 table-w table-h
                 tx ty x1 y1 x2 y2
                 col-widths col-x col-cx)
  
  (princ "\n[HF_AREA] 면적 표기 시작...")
  
  (setq layer-name "HF-AREA")
  
  (setq old-osmode (getvar "OSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  
  (if (not (tblsearch "LAYER" layer-name))
    (entmake 
      (list 
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 layer-name)
        '(70 . 0)
        '(62 . 7)
        '(6 . "Continuous")
      )
    )
  )
  
  (setq txt-style (getvar "TEXTSTYLE"))
  (if (null txt-style) (setq txt-style "Standard"))
  
  (princ "\n면적 표기할 폴리라인 선택: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if ss
    (progn
      ;; 스캔
      (setq i 0)
      (setq valid-list '())
      (setq min-width nil)
      (setq skipped 0)
      
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        (if (= (vla-get-Closed obj) :vlax-true)
          (progn
            (vla-GetBoundingBox obj 'min-pt 'max-pt)
            (setq min-pt (vlax-safearray->list min-pt))
            (setq max-pt (vlax-safearray->list max-pt))
            (setq box-width (- (car max-pt) (car min-pt)))
            
            (setq valid-list 
              (cons (list obj min-pt max-pt box-width) valid-list))
            
            (if (or (null min-width) (< box-width min-width))
              (setq min-width box-width))
          )
          (setq skipped (1+ skipped))
        )
        (setq i (1+ i))
      )
      
      (setq valid-list (reverse valid-list))
      
      ;; 글씨 크기
      (if min-width
        (progn
          (setq txt-height (/ min-width 15.0))
          (if (< txt-height 50) (setq txt-height 50))
          (setq margin (* txt-height 0.5))
          
          (princ (strcat "\n[HF_AREA] 글씨 높이: " 
                         (rtos txt-height 2 1) "mm"))
        )
      )
      
      ;; 텍스트 박기
      (setq processed 0)
      (setq total-m2 0.0)
      (setq total-py 0.0)
      (setq area-data-list '())
      (setq idx 1)
      
      (foreach cur-data valid-list
        (setq obj (nth 0 cur-data))
        (setq min-pt (nth 1 cur-data))
        (setq max-pt (nth 2 cur-data))
        
        (setq area-mm (vla-get-Area obj))
        (setq area-m2 (/ area-mm 1000000.0))
        (setq area-py (/ area-mm 3305785.0))
        
        (setq total-m2 (+ total-m2 area-m2))
        (setq total-py (+ total-py area-py))
        
        (setq area-data-list 
          (append area-data-list 
                  (list (list idx area-m2 area-py))))
        
        (setq text-str 
          (strcat 
            (rtos area-m2 2 2)
            "㎡ / "
            (rtos area-py 2 2)
            "PY"
          )
        )
        
        (setq insert-pt 
          (list 
            (- (car max-pt) margin)
            (+ (cadr min-pt) margin)
            0.0))
        
        (entmake 
          (list 
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            (cons 8 layer-name)
            '(100 . "AcDbMText")
            (cons 10 insert-pt)
            (cons 40 txt-height)
            (cons 41 (* txt-height 20))
            (cons 71 9)
            (cons 72 5)
            (cons 1 text-str)
            (cons 7 txt-style)
          )
        )
        
        (setq processed (1+ processed))
        (setq idx (1+ idx))
      )
      
      (princ (strcat "\n[HF_AREA] 면적 표기 완료: " 
                     (itoa processed) "개"))
      
      ;; ============================================
      ;; 합계표 - LINE + MTEXT로 직접 그리기
      ;; ============================================
      (princ "\n\n합계표 삽입 위치 클릭(왼쪽 상단): ")
      (setq table-pt (getpoint))
      
      (if table-pt
        (progn
          ;; 표 치수 설정 - 가독성을 위해 여백 충분히
          (setq tbl-txt-h (* txt-height 0.9))     ; 폴리라인 글씨보다 살짝 작게
          (setq cell-h (* tbl-txt-h 2.8))         ; 행 높이 (글씨의 2.8배)
          
          ;; 컬럼별 너비 (실명, AREA㎡, AREA PY) - 여유 있게
          (setq col-widths 
            (list 
              (* tbl-txt-h 6.0)   ; 실명 컬럼
              (* tbl-txt-h 8.0)   ; AREA(㎡)
              (* tbl-txt-h 8.0)   ; AREA(PY)
            )
          )
          
          (setq total-rows (+ (length area-data-list) 2))  ; 헤더+데이터+합계
          (setq total-cols 3)
          
          ;; 표 전체 크기
          (setq table-w (apply '+ col-widths))
          (setq table-h (* cell-h total-rows))
          
          ;; 기준점 (왼쪽 위)
          (setq tx (car table-pt))
          (setq ty (cadr table-pt))
          
          ;; ============================================
          ;; 1) 가로 선들 그리기 (행 구분)
          ;; ============================================
          (setq i 0)
          (repeat (+ total-rows 1)
            (setq y1 (- ty (* cell-h i)))
            (entmake 
              (list 
                '(0 . "LINE")
                (cons 8 layer-name)
                (list 10 tx y1 0.0)
                (list 11 (+ tx table-w) y1 0.0)
              )
            )
            (setq i (1+ i))
          )
          
          ;; ============================================
          ;; 2) 세로 선들 그리기 (컬럼 구분)
          ;; ============================================
          ;; 컬럼별 X 좌표 계산
          (setq col-x (list tx))
          (setq i 0)
          (foreach w col-widths
            (setq col-x 
              (append col-x (list (+ (last col-x) w))))
          )
          
          ;; 세로선 그리기
          (foreach x col-x
            (entmake 
              (list 
                '(0 . "LINE")
                (cons 8 layer-name)
                (list 10 x ty 0.0)
                (list 11 x (- ty table-h) 0.0)
              )
            )
          )
          
          ;; ============================================
          ;; 3) 텍스트 채우기 - 각 셀 중앙에
          ;; ============================================
          ;; 셀 중앙 X 좌표 미리 계산
          (setq col-cx '())
          (setq i 0)
          (foreach w col-widths
            (setq col-cx 
              (append col-cx 
                (list (+ (nth i col-x) (/ w 2.0)))))
            (setq i (1+ i))
          )
          
          ;; 헤더 행 (0번 행)
          (setq y1 (- ty (/ cell-h 2.0)))
          (HF-make-mtext 
            (list (nth 0 col-cx) y1 0.0) "실명" 
            tbl-txt-h txt-style layer-name)
          (HF-make-mtext 
            (list (nth 1 col-cx) y1 0.0) "AREA (㎡)" 
            tbl-txt-h txt-style layer-name)
          (HF-make-mtext 
            (list (nth 2 col-cx) y1 0.0) "AREA (PY)" 
            tbl-txt-h txt-style layer-name)
          
          ;; 데이터 행
          (setq i 0)
          (foreach cur-data area-data-list
            (setq y1 (- ty (* cell-h (+ i 1)) (/ cell-h 2.0)))
            ;; 실명 자리 - 번호 자동 입력
            (HF-make-mtext 
              (list (nth 0 col-cx) y1 0.0) 
              (itoa (nth 0 cur-data))
              tbl-txt-h txt-style layer-name)
            ;; AREA ㎡
            (HF-make-mtext 
              (list (nth 1 col-cx) y1 0.0) 
              (rtos (nth 1 cur-data) 2 2)
              tbl-txt-h txt-style layer-name)
            ;; AREA PY
            (HF-make-mtext 
              (list (nth 2 col-cx) y1 0.0) 
              (rtos (nth 2 cur-data) 2 2)
              tbl-txt-h txt-style layer-name)
            (setq i (1+ i))
          )
          
          ;; 합계 행 (마지막)
          (setq y1 (- ty (* cell-h (+ (length area-data-list) 1)) 
                       (/ cell-h 2.0)))
          (HF-make-mtext 
            (list (nth 0 col-cx) y1 0.0) "TOTAL"
            tbl-txt-h txt-style layer-name)
          (HF-make-mtext 
            (list (nth 1 col-cx) y1 0.0) 
            (rtos total-m2 2 2)
            tbl-txt-h txt-style layer-name)
          (HF-make-mtext 
            (list (nth 2 col-cx) y1 0.0) 
            (rtos total-py 2 2)
            tbl-txt-h txt-style layer-name)
          
          (princ "\n[HF_AREA] 합계표 생성 완료")
          (princ (strcat "\n  - 총 면적: " 
                         (rtos total-m2 2 2) "㎡ / "
                         (rtos total-py 2 2) "PY"))
          (princ "\n  - 1,2,3 번호 자리에 실명을 더블클릭해서 입력하세요")
        )
        (princ "\n[HF_AREA] 표 생성 취소됨")
      )
      
      (if (> skipped 0)
        (princ (strcat "\n(닫히지 않은 폴리라인 " 
                       (itoa skipped) "개 스킵)")))
    )
    (princ "\n[HF_AREA] 선택된 폴리라인이 없습니다.")
  )
  
  (setvar "OSMODE" old-osmode)
  (setvar "CMDECHO" old-cmdecho)
  
  (princ)
)

;; ============================================
;; 보조 함수: 중앙 정렬 MTEXT 생성
;; insert-pt 위치에 텍스트를 중앙(MC) 정렬로 박음
;; attachment point 5 = Middle Center
;; ============================================
(defun HF-make-mtext (pt txt h style lyr / )
  (entmake 
    (list 
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 lyr)
      '(100 . "AcDbMText")
      (cons 10 pt)
      (cons 40 h)
      (cons 41 (* h 20))
      (cons 71 5)   ; Middle Center
      (cons 72 5)
      (cons 1 txt)
      (cons 7 style)
    )
  )
)

(princ "\n========================================")
(princ "\n  HF_AREA v5.0 - Hueflow Studio")
(princ "\n  명령어: HFA")
(princ "\n  - 면적 표기: ○○㎡ / ○○PY")
(princ "\n  - 합계표: LINE+MTEXT 직접 그리기")
(princ "\n  - 모든 셀 100% 표시 보장")
(princ "\n========================================")
(princ)
