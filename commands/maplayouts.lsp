(defun c::maplayouts ( / doc layoutList layout viewportList viewport vpCenter vpHeight vpWidth halfWidth halfHeight pt1 pt2 pt3 pt4 polyline)
  ;; Get the active document
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Function to get the list of all layouts
  (defun getLayoutList ( / layoutList)
    (setq layoutList '())
    (vlax-for layout (vla-get-Layouts doc)
      (setq layoutList (cons layout layoutList))
    )
    (reverse layoutList)
  )

  ;; Function to get the list of viewports in a layout
  (defun getViewportList (layout / viewportList blkObj)
    (setq viewportList '())
    (vlax-for blkObj (vla-get-Block layout)
      (if (eq (vla-get-ObjectName blkObj) "AcDbViewport")
        (setq viewportList (cons blkObj viewportList))
      )
    )
    viewportList
  )

  ;; Function to draw edges of a viewport in paper space
  (defun drawViewportEdges (viewport / vpCenter vpHeight vpWidth halfWidth halfHeight pt1 pt2 pt3 pt4 polyline)
    (setq vpCenter (vlax-get viewport 'Center)
          vpHeight (vlax-get viewport 'Height)
          vpWidth (vlax-get viewport 'Width)
          halfWidth (/ vpWidth 2.0)
          halfHeight (/ vpHeight 2.0)
          pt1 (list (- (car vpCenter) halfWidth) (- (cadr vpCenter) halfHeight) 0)
          pt2 (list (+ (car vpCenter) halfWidth) (- (cadr vpCenter) halfHeight) 0)
          pt3 (list (+ (car vpCenter) halfWidth) (+ (cadr vpCenter) halfHeight) 0)
          pt4 (list (- (car vpCenter) halfWidth) (+ (cadr vpCenter) halfHeight) 0))
    ;; Draw polyline in paper space
    (command "_.PLINE" pt1 pt2 pt3 pt4 "_C")
    (entlast)
  )

  ;; Main function
  (setq layoutList (getLayoutList))
  (foreach layout layoutList
    (if (/= (vla-get-Name layout) "Model")
      (progn
        (vla-put-ActiveLayout doc layout)
        (setq viewportList (getViewportList layout))
        (foreach viewport viewportList
          (if (/= (dxf 69 (entget (vlax-vla-object->ename viewport))) 1)  ;; Exclude the main viewport
            (progn
              (setq polyline (drawViewportEdges viewport))
              (command "_.CHSPACE" polyline "" "Model")
            )
          )
        )
      )
    )
  )

  (princ)
)

(princ "\nType MAPLAYOUTS to draw the edges of viewports in model space for each layout.\n")
