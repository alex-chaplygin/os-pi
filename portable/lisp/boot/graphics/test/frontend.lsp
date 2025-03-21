(defun frontend-test ()
  (init-screen 320 200)
  (set-colour 15)
  (gsave)
  (scale 5 5)
  (translate '(5 . 5))
  (new-path)
  (move-to 10 5)
  (line-to 20 10)
  (line-to 5 10)
  (move-to 10 15)
  (line-to 15 18)
  (line-to 20 8)
  (close-path)
  (stroke-path)
  (grestore)
  
;  (cubic-bezier 10 10 200 10 200 150 50 150)
  (set-colour 2)
  (rectangle 10 10 30 30)
  (translate '(50 . 10))
  (set-colour 3)
  (new-path)
  (move-to 100 50)
  (line-to 200 100)
  (line-to 50 100)
  (close-path)
  (stroke-path)
  (scale 1 2)
  (set-colour 4)
  (rectangle 10 10 30 30)
  (draw-screen)
)

(frontend-test)
