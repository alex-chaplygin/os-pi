Комбинаторный парсер
--------------------

Парсер - это функция, которая принимает на вход список объектов, выполняет разбор, возвращает список вариантов разбора. Каждый вариант - это пара: результат разбора и остаток списка. nil в рeзультате означает неудачный разбор.
::

   parser :: lambda (list) -> [(res1 . rest1) ... (resn . restn)]

Элементарный парсер ожидает в списке заданный элемент.
::

   (defun parse-4-digit () (parse-element 4)) ; ожидает 4
   (funcall parse-4-digit '(4 5 6)) ; возвращает ((4 . (5 6)))
   (funcall parse-4-digit '(3 5 6)) ; возвращает ((nil . (3 5 6)))

Комбинатор parse-many позволяет разобрать 0 или более повторений заданного парсера. Результат разбора - список собранных значений.
::

   (funcall (parse-many (parse-element 4)) '(1 2 3)) ; -> ((() . (1 2 3)))
   (funcall (parse-many (parse-element 4)) '(4 2 3)) ; -> (((4) . (2 3)))
   (funcall (parse-many (parse-element 4)) '(4 4 4 2 3)) ; -> (((4 4 4) . (2 3)))      
