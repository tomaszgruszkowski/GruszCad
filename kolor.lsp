(defun c:kolor ( / ss ent i)
  ;; Select all LWPOLYLINE entities on layer 'tkd_zbr'
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE") (8 . "tkd_zbr"))))
  (if ss
      ;; If there are entities selected
      (progn
        (setq i 0)
        ;; Loop through each selected entity
        (repeat (sslength ss)
          (setq ent (ssname ss i))
          (if ent
              ;; If the entity is valid, apply colorization functions
              (progn
                (c:kolor_zs ent)   ; Apply colorization based on xdata
                (c:kolor_zss ent)  ; Apply colorization based on global width
                )
            )
          (setq i (1+ i))
          )
        )
    ;; If no entities are selected
    (princ "\nNo polylines found on layer 'tkd_zbr'.")
    )
  (princ)
  )

(defun c:kolor_zs (ent / funkyProp propValue startPos endPos numberString name color)
  ;; Get the property list for the "funky" property
  (setq funkyProp (cdadr (assoc -3 (entget ent '("funky")))))
  
  ;; Find the value associated with key 1000
  (setq propValue (cdr (assoc 1000 funkyProp)))
  
  ;; Check if 'propValue' is a string and contains "@P"
  (if (and (= (type propValue) 'STR) (vl-string-search "@P" propValue))
      (progn
        ;; Find the starting position of "@P"
        (setq startPos (vl-string-search "@P" propValue))
        
        ;; Find the ending position of "@A"
        (setq endPos (vl-string-search "@A" propValue startPos))
        
        ;; Extract the substring between "@P" and "@", convert it to real and divide by 10
        (setq diameter (/ (atof(substr propValue (+ startPos 3) (- endPos startPos 2))) 10))
        
        ;; Set color based on diameter value
        (c:kolorowanie diameter)
        )
    )
  )

(defun c:kolor_zss (ent / entdata diameter color)
  ;; Get the entity data
  (setq entdata (entget ent))
  ;; Get the diameter from the entity data
  (setq diameter (cdr (assoc 43 entdata)))
  (if (and (numberp diameter) (/= diameter 0.0))
      (progn
        ;; If diameter is a valid number, apply colorization based on diameter
        (c:kolorowanie diameter)
        )
    )
  (princ)
  )

(defun c:kolorowanie (diameter / color)
  ;; Set color based on diameter value
  ;; All atypical diameters are set as white
  (setq color (cond
                ((= diameter 0.45) 7)   ; White
                ((= diameter 0.5) 7)    ; White
                ((= diameter 0.55) 7)   ; White
                ((= diameter 0.6) 11)   ; Cyan
                ((= diameter 0.8) 5)    ; Blue
                ((= diameter 1.0) 6)    ; Magenta
                ((= diameter 1.2) 4)    ; Cyan
                ((= diameter 1.4) 7)    ; White
                ((= diameter 1.6) 3)    ; Green
                ((= diameter 1.8) 7)    ; White
                ((= diameter 2.0) 2)    ; Yellow
                ((= diameter 2.2) 7)    ; White
                ((= diameter 2.5) 1)    ; Red
                ((= diameter 2.8) 7)    ; White
                ((= diameter 3.2) 30)   ; Orange
                ((= diameter 4.0) 7)    ; White
                (t 7)                   ; Different to white
                ))
  ;; Modify the entity with the determined color
  (if (not (assoc 62 (setq name (entget ent))))
      (progn
        (entmod (append name (list (cons 62 color))))
        (entupd ent)
        )
    (entmod (append (entget ent) (list (cons 62 color))))
    )
  )
