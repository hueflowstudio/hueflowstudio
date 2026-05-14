;;; ============================================================
;;; HF_AREA.lsp v2.0
;;; Hueflow Studio - 폴리라인 면적/평수 자동 표기 + 합계표 생성
;;; 
;;; v2.0 변경사항 (2026-05-14):
;;;   - OSNAP/CMDECHO 안전 복구 (에러/ESC 시에도 복구되도록)
;;;   - *error* 핸들러 추가
;;;   - REGEN 자동 실행 (화면 갱신 보장)
;;;
;;; v1.0 변경사항:
;;;   - TABLE 객체 포기 (버그 너무 많음)
;;;   - LINE + MTEXT 조합으로 표 직접 그리기
;;; ============================================================

;; ============================================
;; 전역 변수: 원래 설정 백업용
;; ============================================
(setq HF-AREA:OLD-OSMODE nil)
(setq HF-AREA:OLD-CMDECHO nil)
(setq HF-AREA:OLD-ERROR nil)

;; ============================================
;; 에러 핸들러: ESC나 에러 발생해도 OSNAP 복구
;; ============================================
(defun HF-AREA:restore ( / )
  ;; OSMODE 복구
  (if HF-AREA:OLD-OSMODE
    (progn
      (setvar "OSMODE" HF-AREA:OLD-OSMODE)
      (setq HF-AREA:OLD-OSMODE nil)
    )
  )
  ;; CMDECHO 복구
  (if HF-AREA:OLD-CMDECHO
    (progn
      (setvar "CMDECHO" HF-AREA:OLD-CMDECHO)
      (setq HF-AREA:OLD-CMDECHO nil)
    )
  )
  ;; 에러 핸들러 복구
  (if HF-AREA:OLD-ERROR
    (progn
      (setq *error* HF-AREA:OLD-ERROR)
      (setq HF-AREA:OLD-ERROR nil)
    )
  )
)

(defun HF-AREA:error-handler (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
    (princ (strcat "\n[HF_AREA 에러] " msg))
    (princ "\n[HF_AREA] 사용자 취소")
  )
  (HF-AREA:restore)
  (princ)
)

;; ============================================
;; 메인 명령어
;; ============================================
(defun c:HFA ( / ss i ent obj min-pt max-pt 
                 insert-pt area-mm area-m2 area-py
                 text-str
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
                 col-widths col-x col-cx w x)
  
  (princ "\n[HF_AREA v2.0] 면적 표기 시작...")
  
  (setq layer-name "HF-AREA")
  
  ;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ;; 안전 백업 (전역 변수에 저장)
  ;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  (setq HF-AREA:OLD-OSMODE (getvar "OSMODE"))
  (setq HF-AREA:OLD-CMDECHO (getvar "CMDECHO"))
  (setq HF-AREA:OLD-ERROR *error*)
  (setq *error* HF-AREA:error-handler)
  
  ;; OSMODE를 0으로 (스냅 끔)
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  
  ;; 레이어 생성
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
      ;; 합계표
      ;; ============================================
      (princ "\n\n합계표 삽입 위치 클릭(왼쪽 상단): ")
      (setq table-pt (getpoint))
      
      (if table-pt
        (progn
          (setq tbl-txt-h (* txt-height 0.9))
          (setq cell-h (* tbl-txt-h 2.8))
          
          (setq col-widths 
            (list 
              (* tbl-txt-h 6.0)
              (* tbl-txt-h 8.0)
              (* tbl-txt-h 8.0)
            )
          )
          
          (setq total-rows (+ (length area-data-list) 2))
          (setq total-cols 3)
          
          (setq table-w (apply '+ col-widths))
          (setq table-h (* cell-h total-rows))
          
          (setq tx (car table-pt))
          (setq ty (cadr table-pt))
          
          ;; 가로 선
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
          
          ;; 세로 선
          (setq col-x (list tx))
          (foreach w col-widths
            (setq col-x 
              (append col-x (list (+ (last col-x) w))))
          )
          
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
          
          ;; 셀 중앙 X
          (setq col-cx '())
          (setq i 0)
          (foreach w col-widths
            (setq col-cx 
              (append col-cx 
                (list (+ (nth i col-x) (/ w 2.0)))))
            (setq i (1+ i))
          )
          
          ;; 헤더
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
            (HF-make-mtext 
              (list (nth 0 col-cx) y1 0.0) 
              (itoa (nth 0 cur-data))
              tbl-txt-h txt-style layer-name)
            (HF-make-mtext 
              (list (nth 1 col-cx) y1 0.0) 
              (rtos (nth 1 cur-data) 2 2)
              tbl-txt-h txt-style layer-name)
            (HF-make-mtext 
              (list (nth 2 col-cx) y1 0.0) 
              (rtos (nth 2 cur-data) 2 2)
              tbl-txt-h txt-style layer-name)
            (setq i (1+ i))
          )
          
          ;; 합계 행
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
  
  ;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ;; 안전 복구 (정상 종료 시)
  ;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  (HF-AREA:restore)
  
  ;; 화면 갱신
  (command "_.REGEN")
  
  (princ)
)

;; ============================================
;; 보조 함수: 중앙 정렬 MTEXT 생성
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
      (cons 71 5)
      (cons 72 5)
      (cons 1 txt)
      (cons 7 style)
    )
  )
)

(princ "\n========================================")
(princ "\n  HF_AREA v2.0 - Hueflow Studio")
(princ "\n  명령어: HFA")
(princ "\n  v2.0: OSNAP 안전 복구 + 에러 핸들러")
(princ "\n========================================")
(princ)
