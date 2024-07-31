(defun c::maplayouts (/ layouts opt doc layoutObj viewports count ll ur)
  (defun draw-rect (ll ur)
    (entmakex
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 4)
        (cons 70 1)
        (cons 10 (car ll)) (cons 20 (cadr ll))
        (cons 10 (car ur)) (cons 20 (cadr ll))
        (cons 10 (car ur)) (cons 20 (cadr ur))
        (cons 10 (car ll)) (cons 20 (cadr ur))
        (cons 10 (car ll)) (cons 20 (cadr ll))
      )
    )
  )

  (defun create-view (name ll ur)
    (command "._-view" "_n" name (car ll) (cadr ll) (car ur) (cadr ur))
  )

  (defun get-layouts ()
    (vl-remove-if
      '(lambda (x) (member x '("Model")))
      (layoutlist)
    )
  )

  (defun get-viewports (layout)
    (setq vp-list nil)
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq layoutObj (vla-Item (vla-get-Layouts doc) layout))
    (vlax-for item (vla-get-Block layoutObj)
      (if (eq (vla-get-ObjectName item) "AcDbViewport")
        (setq vp-list (cons item vp-list))
      )
    )
    vp-list
  )

  (defun layoutlist ()
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq layouts (vla-get-Layouts doc))
    (setq layoutList nil)
    (vlax-for layout layouts
      (setq layoutList (cons (vla-get-Name layout) layoutList))
    )
    layoutList
  )

  (setq layouts (get-layouts))
  (setq opt (getstring "\nEnter option (F for Frames, V for Named Views, B for Both): "))

  (foreach layout layouts
    (setvar "ctab" layout)
    (command "._zoom" "_all")
    (setq viewports (get-viewports layout))
    (setq count 1)

    (foreach viewport viewports
      (if (> (vla-get-Number viewport) 1) ; Exclude the main viewport
        (progn
          (setq ll (vlax-get viewport 'LowerLeftCorner))
          (setq ur (vlax-get viewport 'UpperRightCorner))
          (if (member opt '("F" "B"))
            (draw-rect ll ur)
          )
          (if (member opt '("V" "B"))
            (create-view (strcat layout (if (> count 1) (itoa count) "")) ll ur)
          )
          (setq count (1+ count))
        )
      )
    )
  )
  (princ)
)

(princ)
