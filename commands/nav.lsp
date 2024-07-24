(defun c::nav (/ input command arg full-path)
  ;; Get the command
  (setq command (getstring "\nEnter command (cd, .., ls, openfile, opendir): "))
  
  ;; Handle the cd command separately to allow space in directory names
  (if (equal command "cd")
    (progn
      ;; Get the directory path
      (setq arg (getstring "\nEnter directory name: "))
      (setq full-path (vl-filename-makepath *current-dir* arg))
      (if (vl-directory-files full-path nil -1)
        (progn
          (setq *current-dir* full-path)
          (princ (strcat "\nCurrent directory: " *current-dir*))
        )
        (princ "\nInvalid directory")
      )
    )
    (cond
      ((equal command "..")
        (setq parent-dir (vl-filename-directory *current-dir*))
        (if (and parent-dir (not (equal parent-dir "")))
          (progn
            (setq *current-dir* parent-dir)
            (princ (strcat "\nCurrent directory: " *current-dir*))
          )
          (princ "\nAlready at the root directory")
        )
      )
      ((equal command "ls")
        (if *current-dir*
          (foreach file (vl-directory-files *current-dir* nil -1)
            (princ (strcat "\n" file))
          )
          (princ "\nNo directory set")
        )
      )
      ((equal command "openfile")
        (setq arg (getstring "\nEnter file name: "))
        (setq full-path (vl-filename-makepath *current-dir* arg))
        (if (findfile full-path)
          (command "_.OPEN" full-path)
          (princ "\nFile not found in current directory")
        )
      )
      ((equal command "opendir")
        (startapp "explorer" *current-dir*)
      )
      (t (princ "\nInvalid command"))
    )
  )
  (princ)
)

(defun vl-filename-makepath (path filename)
  (if (or (= (substr path (strlen path)) "\\")
          (= (substr path (strlen path)) "/"))
      (strcat path filename)
      (strcat path "\\" filename))
)

(setq *current-dir* "C:\\")  ; Set initial directory
(princ (strcat "\nInitial directory: " *current-dir*))
