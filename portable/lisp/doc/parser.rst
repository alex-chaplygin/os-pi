Комбинаторный парсер
--------------------

Парсер - это функция, которая принимает на вход абстрактный поток данных (может быть строка, массив, список), выполняет разбор, возвращает пару: результат разбора и остаток потока (новое состояние потока). nil в рeзультате означает неудачный разбор.
::

   parser :: lambda (stream) -> (res . stream1)

Элементарный парсер **parse-elem** ожидает в списке заданный элемент.
::

   (defun parse-4-digit () (parse-elem 4)) ; ожидает 4
   (funcall parse-4-digit (stream-from-list (4 5 6))) ; возвращает (4 . <stream1 (5 6)>)
   (funcall parse-4-digit (stream-from-list (3 5 6))) ; возвращает nil

Комбинатор **parse-many** позволяет разобрать 0 или более повторений заданного парсера. Результат разбора - список собранных значений.
::

   (funcall (parse-many (parse-elem 4)) (stream-from-list (1 2 3))) ; -> nil
   (funcall (parse-many (parse-elem 4)) (stream-from-list (4 2 3))) ; -> ((4) . <stream (2 3)>)
   (funcall (parse-many (parse-elem 4)) (stream-from-list (4 4 2 4 3))) ; -> ((4 4) . <stream (2 4 3))      

Разбор по предикату **parse-pred**, предикат - функция, которая на вход получает символ, на выходе - nil или t.
Сама функция парсинга возвращает в результате парсинга в случае успешного разбора сам символ, в случае неудачного - nil.
::

   (funcall (parse-pred #'is-digit) (stream-from-list (#\2 s A))) ; -> (#\2 . <stream (s A)>)
   (funcall (parse-pred #'is-digit) (stream-from-list (#\D 2 A))) ; -> nil

Последовательный комбинатор **&&&** применяет несколько парсеров parsers подряд к списку, каждый следующий parser применяется к остатку от работы предыдущего parser. Возвращает список результатов всех парсеров или nil, если хотя бы один разбор неудачный.
::

   (funcall (&&& (parse-elem #\() (parse-pred #'is-alpha) (parse-elem #\)))
      (stream-from-str "(a)")) ; -> ((#\( #\a #\)) . <stream>)

Параллельный разбор двух и более парсеров **parse-or**. Результат будет у первого успешного разбора или nil если все альтернативы неуспешны.
::

  (let ((s1 (stream-from-str "abc")))
    (funcall (parse-or (parse-elem #\a) (parse-elem #\b) (parse-elem #\c)) s1) ; -> (#\a . <>)

      
Применение функции к результату разбора **parse-app**:
::

     (funcall (parse-app (&&& (parse-elem #\a) (parse-elem #\b)) #'car) (stream-from-str "abcd")) ; -> (#\a . <>)
