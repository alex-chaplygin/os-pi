(setq elem2 (make-element 1 1 5 5 "test" +blue+ +red+ +green+ nil nil 0 0))
(setq elem (make-element 5 5 20 20 "test" +cyan+ +magenta+ +green+ nil nil 0 0))
(add-child elem elem2)
(draw elem)
