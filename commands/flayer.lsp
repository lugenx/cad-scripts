(defun make-spaces (n)
  (if (> n 0)
    (strcat " " (make-spaces (1- n)))
    ""
  )
)

(defun c::flayer ( / input-layer layer-name layer-list counter key-list key-char page layers-per-page layer-names total-pages all-layer-names layer-map user-input max-layer-name-length start-index end-index current-layers i current-layer)
  (textscr)  ; Open the text screen
  (setq input-layer (getstring "\nEnter layer name or partial name to filter: "))
  (setq input-layer (strcase input-layer))  ; Convert input to uppercase for case-insensitive comparison
  (setq layer-list (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))  ; Get all layers
  (setq counter 0)
  (setq layers-per-page 15)
  (setq key-list '("S" "D" "F" "J" "K" "L" "G" "H" "W" "E" "R" "U" "I" "O" "P"))  ; List of keys without "A"
  (setq all-layer-names '())
  (setq layer-map '())
  (setq max-layer-name-length 0)
  (setq current-layer (getvar "CLAYER"))

  (vlax-for layer layer-list
    (setq layer-name (strcase (vla-get-name layer)))  ; Convert layer name to uppercase for case-insensitive comparison
    (if (wcmatch layer-name (strcat "*" input-layer "*"))
      (progn
        (setq all-layer-names (append all-layer-names (list layer-name)))
        (setq max-layer-name-length (max max-layer-name-length (strlen layer-name)))
      )
    )
  )

  (setq total-pages (/ (+ (length all-layer-names) (1- layers-per-page)) layers-per-page))
  (setq page 1)

  (defun show-page ()
    (setq start-index (* (1- page) layers-per-page))
    (setq end-index (min (length all-layer-names) (* page layers-per-page)))
    (setq current-layers '())
    (setq i start-index)
    (while (< i end-index)
      (setq current-layers (append current-layers (list (nth i all-layer-names))))
      (setq i (1+ i))
    )
    (setq layer-map '())  ; Reset layer map for each cycle
    (if (> total-pages 1)
      (princ (strcat "\nPage " (itoa page) "/" (itoa total-pages) ":\n"))
      (princ "\n")
    )
    (setq counter 0)
    (while (< counter (length current-layers))
      (setq key-char (nth counter key-list))  ; Get the corresponding key character
      (setq layer (vla-item layer-list (nth counter current-layers)))  ; Get the layer object
      (setq status "")
      (setq status (strcat "(" 
                           (if (= (vla-get-layeron layer) :vlax-true) "On" "Off") ", "  ; Layer on/off status
                           (if (= (vla-get-lock layer) :vlax-true) "Locked" "Unlocked") ", "  ; Layer lock/unlock status
                           (if (= (vla-get-freeze layer) :vlax-true) "Frozen" "Unfrozen") ", "  ; Layer freeze/unfreeze status
                           (if (= (vla-get-plottable layer) :vlax-true) "Plottable" "Not Plottable")  ; Layer plot/not plot status
                           ")"))
      (setq layer-map (cons (cons key-char (nth counter current-layers)) layer-map))  ; Map the key to the layer name
      (princ (strcat "[" key-char "] " (nth counter current-layers) (make-spaces (- (+ max-layer-name-length 2) (strlen (nth counter current-layers)))) status "\n"))  ; Align the status
      (setq counter (1+ counter))  ; Increment the counter
    )
    (if (> total-pages 1)
      (princ (strcat "\nEnd of Page " (itoa page) "/" (itoa total-pages) "\n"))
    )
  )

  (show-page)

  (while t
    (setq user-input (strcase (getstring "\nType 'n' to see the next page, or type a letter to make that layer current, or 'a' to apply action to all filtered layers, or 'o', 'x', 'l', 'r', 'f', 't', 'p', 'q' for specific actions: ")))  ; Convert user input to uppercase
    (cond
      ((equal user-input "N")
       (setq page (if (= page total-pages) 1 (1+ page)))
       (show-page)
      )
      ((= (substr user-input 1 1) "A")
       (cond
         ((= (substr user-input 2 1) "F")  ; Freeze all except current
          (foreach layer-name all-layer-names
            (if (not (equal layer-name current-layer))
              (progn
                (setq layer (vla-item layer-list layer-name))
                (vla-put-freeze layer :vlax-true)
              )
            )
          )
          (princ "\nAll applicable layers are now frozen.\n")
         )
         ((= (substr user-input 2 1) "T")  ; Unfreeze all
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-freeze layer :vlax-false)
          )
          (princ "\nAll filtered layers are now unfrozen.\n")
         )
         ((= (substr user-input 2 1) "L")  ; Lock all
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-lock layer :vlax-true)
          )
          (princ "\nAll filtered layers are now locked.\n")
         )
         ((= (substr user-input 2 1) "R")  ; Unlock all
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-lock layer :vlax-false)
          )
          (princ "\nAll filtered layers are now unlocked.\n")
         )
         ((= (substr user-input 2 1) "O")  ; Turn on all
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-layeron layer :vlax-true)
          )
          (princ "\nAll filtered layers are now on.\n")
         )
         ((= (substr user-input 2 1) "X")  ; Turn off all
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-layeron layer :vlax-false)
          )
          (princ "\nAll filtered layers are now off.\n")
         )
         ((= (substr user-input 2 1) "P")  ; Make all plottable
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-plottable layer :vlax-true)
          )
          (princ "\nAll filtered layers are now plottable.\n")
         )
         ((= (substr user-input 2 1) "Q")  ; Make all not plottable
          (foreach layer-name all-layer-names
            (setq layer (vla-item layer-list layer-name))
            (vla-put-plottable layer :vlax-false)
          )
          (princ "\nAll filtered layers are now not plottable.\n")
         )
       )
       (show-page)  ; Show the same page again after performing the action
      )
      ((assoc (substr user-input 1 1) layer-map)
       (setq layer-name (cdr (assoc (substr user-input 1 1) layer-map)))
       (cond
         ((= (substr user-input 2 1) "F")  ; Freeze
          (if (not (equal layer-name current-layer))
            (vla-put-freeze (vla-item layer-list layer-name) :vlax-true)
            (princ (strcat "\nLayer " layer-name " cannot be frozen because it is the current layer.\n"))
          )
         )
         ((= (substr user-input 2 1) "T")  ; Unfreeze
          (vla-put-freeze (vla-item layer-list layer-name) :vlax-false)
          (princ (strcat "\nLayer " layer-name " is now unfrozen.\n"))
         )
         ((= (substr user-input 2 1) "L")  ; Lock
          (vla-put-lock (vla-item layer-list layer-name) :vlax-true)
          (princ (strcat "\nLayer " layer-name " is now locked.\n"))
         )
         ((= (substr user-input 2 1) "R")  ; Unlock
          (vla-put-lock (vla-item layer-list layer-name) :vlax-false)
          (princ (strcat "\nLayer " layer-name " is now unlocked.\n"))
         )
         ((= (substr user-input 2 1) "O")  ; Turn on
          (vla-put-layeron (vla-item layer-list layer-name) :vlax-true)
          (princ (strcat "\nLayer " layer-name " is now on.\n"))
         )
         ((= (substr user-input 2 1) "X")  ; Turn off
          (vla-put-layeron (vla-item layer-list layer-name) :vlax-false)
          (princ (strcat "\nLayer " layer-name " is now off.\n"))
         )
         ((= (substr user-input 2 1) "P")  ; Make plottable
          (vla-put-plottable (vla-item layer-list layer-name) :vlax-true)
          (princ (strcat "\nLayer " layer-name " is now plottable.\n"))
         )
         ((= (substr user-input 2 1) "Q")  ; Make not plottable
          (vla-put-plottable (vla-item layer-list layer-name) :vlax-false)
          (princ (strcat "\nLayer " layer-name " is now not plottable.\n"))
         )
         (t  ; Make current
          (command "CLAYER" layer-name)
          (princ (strcat "\nLayer " layer-name " is now the current layer.\n"))
         )
       )
       (show-page)  ; Show the same page again after performing the action
      )
      (t
       (princ "\nInvalid input. Please try again.\n")
      )
    )
  )
)
