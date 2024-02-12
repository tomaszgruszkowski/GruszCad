(defun c:2wwy ( / ss ent i)
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
                ;; Get the 'funky' property of the entity
                (setq funkyProp (cdadr (assoc -3 (entget ent '("funky")))))
                ;; Extract the value associated with key 5 from the 'funky' property
                (setq wwy (cdr (nth 5 funkyProp)))
                ;; Check if wwy equals 2
                (if (= wwy 2)
                    ;; If wwy equals 2, set the color to 254
                    (if (not (assoc 62 (setq name (entget ent))))
                        ;; If the color property doesn't exist, add it and update the entity
                        (progn
                          (entmod (append name (list (cons 62 254))))
                          (entupd ent)
                          )
                      ;; If the color property exists, update it
                      (entmod (append (entget ent) (list (cons 62 254))))
                      )
                  )
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
