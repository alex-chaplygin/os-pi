(defvar *entity-list* ())
(defvar *entity-temp-list* ())
(defvar *no-solid-tiles*)
(defvar *level*)
(defvar *back-collision-handler*)
(defvar *obj-collision-handler*)
(defclass entity () (mob sprite mark-add mark-del))

(defun new-entity (pos sprite)
  "Cоздать игровую сущность (врага, игрового персонажа)"
  (set-sprite-pos sprite pos)
  (setq *entity-temp-list* (append
			    *entity-temp-list*
			    (list (make-entity
				   (new-mob pos)
				   sprite
				   T
				   nil)))))

(defun delete-entity (entity)
  "Удалить игровую сущность"
  (entity-set-mark-del entity T))

(defmethod update-entity ((self entity))
  "Выполнить движение сущности (можно переопределить метод)"
  (move-mob (entity-mob self))
  (set-sprite-pos (entity-sprite self) (mob-pos (entity-mob self))))

(defun plat-set-level (level)
  "Задать уровень двумерным массивом тайлов"
  (setq *level* level)
  (clear-screen)
  (set-background *level*))

(defun plat-set-no-solid-tiles (tiles)
  "Задать прозрачные тайлы списком тайлов"
  (setq *no-solid-tiles* tiles))

(defun plat-update ()
  "Обновляет состояние игры (передвигает объекты, обрабатывает столкновения)"
  (add-entities)
  (rm-entities)
  (app #'(lambda (ent) (resolve-back-collision (get-collided-back-tiles ent) ent)
			(resolve-object-collision (get-collided-objects ent) ent)
			(update-entity ent))
       *entity-list*))
	 

(defun plat-on-back-collision (f)
  "Зарегистрировать функцию f, которая обрабатывает столкновения с фоном" 
  "f entity tile: функция, принимающая сущность, которая столкнулась, и тайл, с которым произошло столкновение"
  (setq *back-collision-handler* f))

(defun plat-on-obj-collision (f)
  "Зарегистрировать функцию f, которая обрабатывает столкновения между объектами"
  "f entity1 entity2: функция, принимающая две сущности, между которыми произошло столкновение"
  (setq *obj-collision-handler* f))

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^API^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(defun rm-entities ()
  "Удаляет сущности с пометкой на удаление из списка сущностей"
  (setq *entity-list* (filter #'(lambda (ent) (null (entity-mark-del ent))) *entity-list*)))

(defun add-entities ()
  "Добавляет сущности с пометкой на добавление в список сущностей"
  (app #'(lambda (ent)
	   (when (= (entity-mark-add ent) T)
	     (progn
	       (entity-set-mark-add ent nil)
	       (setq *entity-list* (append *entity-list* (list ent))))))
       *entity-temp-list*)
  (setq *entity-temp-list* (list)))

(defun resolve-back-collision (tiles entity)
  "Применяет обработчик столкновений между сущностью и фоном"
  (when (and (not (null *back-collision-handler*)) (not (null tiles)))
    (app #'(lambda (tile) (funcall *back-collision-handler* entity tile)) tiles)))

(defun resolve-object-collision (entities entity)
  "Применяет обработчик столкновений между сущностями"
  (when (and (not (null *obj-collision-handler*)) (not (null entities)))
    (app #'(lambda (ent) (funcall *obj-collision-handler* entity ent)) entities)))

(defun get-collided-back-tiles (entity)
  "Возвращает список тайлов фона, с которым столкнулась entity"
  (let* ((ent-pos (mob-pos (entity-mob entity)))
	 (ent-x (vec2-x ent-pos))
	 (ent-y (vec2-y ent-pos))
	 (ent-img (sprite-image (entity-sprite entity)))
	 (ent-x2 (+ ent-x (image-width ent-img)))
	 (ent-y2 (+ ent-y (image-height ent-img)))
	 (start-tile-ind (get-tile-index ent-x ent-y))
	 (end-tile-ind (get-tile-index ent-x2 ent-y2))
	 (start-row (car start-tile-ind))
	 (start-column (cdr start-tile-ind))
	 (end-row (car end-tile-ind))
	 (end-column (cdr end-tile-ind))
	 (result-list ()))
    (for row-ind start-row end-row
	 (let ((row (aref *level* row-ind)))
	   (for column-ind start-column end-column
		(let ((cur-tile (aref row column-ind)))
		  (when (not (contains *no-solid-tiles* cur-tile))
		    (setq result-list (append result-list (list cur-tile))))))))
    result-list))

(defun get-collided-objects (entity)
  "Возвращает список сущностей, с которыми произошло столкновение entity"
  (let ((result-list ()))
    (app #'(lambda (ent)
	     (when (and (!= ent entity) (is-object-collision ent entity))
	       (setq result-list (append result-list (list ent)))))
	 *entity-list*)
    result-list))

(defun is-object-collision (entity1 entity2)
  "Возвращает T, если entity1 столкнулась с entity2"
  (let* ((ent1-x (vec2-x (mob-pos (entity-mob entity1))))
	 (ent1-y (vec2-y (mob-pos (entity-mob entity1))))
	 (ent2-x (vec2-x (mob-pos (entity-mob entity2))))
	 (ent2-y (vec2-y (mob-pos (entity-mob entity2))))
	 (ent1-x2 (+ ent1-x (image-width (sprite-image (entity-sprite entity1)))))
	 (ent1-y2 (+ ent1-y (image-height (sprite-image (entity-sprite entity1)))))
	 (ent2-x2 (+ ent2-x (image-width (sprite-image (entity-sprite entity2)))))
	 (ent2-y2 (+ ent2-y (image-height (sprite-image (entity-sprite entity2))))))
    (if (or
	 (and (and (> ent1-x ent2-x) (< ent1-x ent2-x2))
	      (and (> ent1-y ent2-y) (< ent1-y ent2-y2)))
	 (and (and (> ent1-x2 ent2-x) (< ent1-x2 ent2-x2))
	      (and (> ent1-y2 ent2-y) (< ent1-y2 ent2-y2))))
	T
	nil)))
  
