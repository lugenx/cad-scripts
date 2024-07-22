(defun c::flayer ( / input-layer layer-name layer-list counter key-list key-char page layers-per-page layer-names total-pages all-layer-names layer-map user-input)
  (princ "\nOpening command line...\n")
  (command "textscr")  ; Open the text screen
  (setq input-layer (getstring "\nEnter layer name or partial name to filter: "))
  (setq input-layer (strcase input-layer))  ; Convert input to uppercase for case-insensitive comparison
  (setq layer-list (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))  ; Get all layers
  (setq counter 0)
  (setq layers-per-page 15)
  (setq key-list '("S" "D" "F" "J" "K" "L" "G" "H" "W" "E" "R" "U" "I" "O" "P"))  ; List of keys without "A"
  (setq all-layer-names '())
  (setq layer-map '())

  (vlax-for layer layer-list
    (setq layer-name (strcase (vla-get-name layer)))  ; Convert layer name to uppercase for case-insensitive comparison
    (if (wcmatch layer-name (strcat "*" input-layer "*"))
      (setq all-layer-names (append all-layer-names (list layer-name)))
    )
  )

  (setq total-pages (/ (+ (length all-layer-names) (1- layers-per-page)) layers-per-page))
  (setq page 1)

  (while t
    (setq layer-names all-layer-names)
    (setq page 1)
    (setq layer-map '())  ; Reset layer map for each cycle
    (while layer-names
      (if (> total-pages 1)
        (princ (strcat "\nPage " (itoa page) "/" (itoa total-pages) ":\n"))
        (princ "\n")
      )
      (setq counter 0)
      (while (and layer-names (< counter layers-per-page))
        (setq key-char (nth counter key-list))  ; Get the corresponding key character
        (setq layer-map (cons (cons key-char (car layer-names)) layer-map))  ; Map the key to the layer name
        (princ (strcat "[" key-char "] " (car layer-names) "\n"))
        (setq layer-names (cdr layer-names))  ; Remove the first element
        (setq counter (1+ counter))  ; Increment the counter
      )
      (if layer-names
        (progn
          (if (> total-pages 1)
            (princ (strcat "\nEnd of Page " (itoa page) "/" (itoa total-pages) "\n"))
          )
          (setq user-input (strcase (getstring "\nPress Enter to see the next page or type a letter to make that layer current, or 'sf', 'sl', 'so', 'sp' for other actions: ")))  ; Convert user input to uppercase
          (if (and user-input (assoc (substr user-input 1 1) layer-map))
            (progn
              (setq layer-name (cdr (assoc (substr user-input 1 1) layer-map)))
              (cond
                ((= (substr user-input 2 1) "F")  ; Freeze/unfreeze
                 (if (= (vla-get-freeze (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-freeze (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-freeze (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-freeze (vla-item layer-list layer-name)) :vlax-true) "frozen" "unfrozen") ".\n"))
                )
                ((= (substr user-input 2 1) "L")  ; Lock/unlock
                 (if (= (vla-get-lock (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-lock (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-lock (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-lock (vla-item layer-list layer-name)) :vlax-true) "locked" "unlocked") ".\n"))
                )
                ((= (substr user-input 2 1) "O")  ; On/off
                 (if (= (vla-get-layeron (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-layeron (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-layeron (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-layeron (vla-item layer-list layer-name)) :vlax-true) "on" "off") ".\n"))
                )
                ((= (substr user-input 2 1) "P")  ; Plot/not plot
                 (if (= (vla-get-plottable (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-plottable (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-plottable (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-plottable (vla-item layer-list layer-name)) :vlax-true) "plottable" "not plottable") ".\n"))
                )
                (t  ; Make current
                 (command "CLAYER" layer-name)
                 (princ (strcat "\nLayer " layer-name " is now the current layer.\n"))
                )
              )
              (getstring "\nPress Enter to continue...")  ; Wait for the user to press Enter to continue
            )
            (if (= user-input "")
              (setq page (1+ page))
            )
          )
        )
        (progn
          (setq user-input (strcase (getstring "\nPress Enter to see the first page again or type a letter to make that layer current, or 'sf', 'sl', 'so', 'sp' for other actions: ")))  ; Convert user input to uppercase
          (if (and user-input (assoc (substr user-input 1 1) layer-map))
            (progn
              (setq layer-name (cdr (assoc (substr user-input 1 1) layer-map)))
              (cond
                ((= (substr user-input 2 1) "F")  ; Freeze/unfreeze
                 (if (= (vla-get-freeze (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-freeze (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-freeze (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-freeze (vla-item layer-list layer-name)) :vlax-true) "frozen" "unfrozen") ".\n"))
                )
                ((= (substr user-input 2 1) "L")  ; Lock/unlock
                 (if (= (vla-get-lock (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-lock (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-lock (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-lock (vla-item layer-list layer-name)) :vlax-true) "locked" "unlocked") ".\n"))
                )
                ((= (substr user-input 2 1) "O")  ; On/off
                 (if (= (vla-get-layeron (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-layeron (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-layeron (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-layeron (vla-item layer-list layer-name)) :vlax-true) "on" "off") ".\n"))
                )
                ((= (substr user-input 2 1) "P")  ; Plot/not plot
                 (if (= (vla-get-plottable (vla-item layer-list layer-name)) :vlax-false)
                   (vla-put-plottable (vla-item layer-list layer-name) :vlax-true)
                   (vla-put-plottable (vla-item layer-list layer-name) :vlax-false)
                 )
                 (princ (strcat "\nLayer " layer-name " is now " (if (= (vla-get-plottable (vla-item layer-list layer-name)) :vlax-true) "plottable" "not plottable") ".\n"))
                )
                (t ; Make current
                 (command "CLAYER" layer-name)
                 (princ (strcat "\nLayer " layer-name " is now the current layer.\n"))
                )
              )
              (getstring "\nPress Enter to continue...") ; Wait for the user to press Enter to continue
            )
          )
        )
      )
    )
  )
)




















