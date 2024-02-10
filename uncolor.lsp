(defun c:unkolor ( / ss ent i ents)
  ;; Select all LWPOLYLINE entities on layer 'tkd_zbr'
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE") (8 . "tkd_zbr"))))
  (if ss
      (progn
        ;; Initialize index and empty list for entity names
        (setq i 0
              ents '())
        ;; Loop through each selected entity
        (repeat (sslength ss)
          (setq ent (ssname ss i))
          (if ent
              ;; Add entity name to the list
              (setq ents (cons ent ents)))
          (setq i (1+ i)))
        ;; Loop through each entity in the list
        (foreach ent ents
          ;; Add color 256 (ByLayer) to the entity
          (entmod (append (entget ent) (list (cons 62 256))))))
    ;; If no entities found, print message
    (princ "No entities found."))
  (princ)
)