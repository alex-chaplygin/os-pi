
(clear-screen)
(let ((el (make-element 0 10 1 30 14 +white+ +red+ 0 0 0))
      (el2 (make-element 0 5 15 30 5 +red+ +red+ 0 0 0))
      (el3 (make-element 0 60 12 13 5 +yellow+ +blue+ 0 0 0)))
  (draw el)
  (draw el2)
  (draw el3)
  )

(draw-screen)
