Библиотека функционального реактивного программирования
=======================================================

Система обработки событий в реальном времени включает:
* источники событий;
* обработчики событий;
* средства комбинирования и преобразования событий.

Поток событий - объект, который хранит текущее значение, связи с другими потоками и список реакций. Например, поток датчика помнит последнее показание, даже если датчик некоторое время ничего не присылает.

Источник создается через frp-event и принимает данные извне. Производный поток получается из комбинатора: его значение вычисляется по значениям входов. Любой такой объект можно передать в комбинатор.
Подавать внешние значения разрешено только в источники, поскольку значение производного потока задается его зависимостями.

Реакция - функция одного аргумента. Она получает значение потока и выполняет действие.

Создание источника событий
--------------------------

(frp-event <значение>)

Создает новый источник и возвращает объект потока. Опциональный параметр - начальное значение, по умолчанию nil.

::

   (defvar sensor1 (frp-event 1)) ; sensor1 - новый поток событий
   (defvar sensor2 (frp-event)) ; по умолчанию значение nil
   
Генерация события
-----------------

(frp-emit source value)

source - источник, созданный через frp-event, value - объект. Функция записывает значение, распространяет это обновление, выполняет реакции и возвращает переданное value.
Если значение - nil, то поток становится пустым, реакции нет.

::
   (defvar sensor (frp-event))
   (frp-react sensor #'(lambda (x) (print x)))

   (frp-emit sensor 1)   ; печатает 1, вызов возвращает 1
   (frp-emit sensor 'A)  ; печатает A
   (frp-emit sensor 'A)  ; снова печатает A
   (frp-emit sensor nil) ; поток выключается, реакции нет
   (frp-now sensor)      ; nil

   (defvar sensor1 (frp-event))

   (defun sensor1 (value)
      "Передает value в поток sensor1 и возвращает value"
      (frp-emit sensor1 value))

   (frp-react sensor1 #'(lambda (x) (print x)))
   (sensor1 1) ; печатает 1


Назначение обработчика события
------------------------------

frp-react <сенсор> <обработчик>

Обработчик - функция с одним параметром. У сенсора может быть сколько угодно обработчиков.
Каждая реацкия получает одно и то же итоговое значение своего потока, возвращаемое обработчиком значение игнорируется.
Печать и другие побочные действия следует выполнять здесь, а функции для frp-map и frp-filter оставлять чистыми.

::
   (defvar sensor (frp-event))
   (defvar *last-sensor* nil)

   (frp-react sensor #'(lambda (x) (print x)))
   (frp-react sensor #'(lambda (x) (setq *last-sensor* x)))

   (frp-emit sensor 8) ; сначала печатает 8, затем запись *last-sensor*
   *last-sensor*      ; 8

   (frp-emit sensor nil)
   *last-sensor*      ; все еще 8, поскольку сброс не вызвал обработчик
   

Комбинация событий
------------------

Каждый комбинатор создает новый поток событий.

(frp-and left right)

Создает и возвращает новый поток. При обновлении любого входа смотрит на текущие значения обоих. Если оба отличаются от nil, то результат равен t, иначе результат сбрасывается в nil.

::
   (defvar sensor1 (frp-event))
   (defvar sensor2 (frp-event))
   (defvar bell (frp-and sensor1 sensor2))
   (frp-react bell #'(lambda (x) (print "Bell")))

   (frp-emit sensor1 1)   ; bell = nil, ничего не печатется
   (frp-emit sensor2 1)   ; bell = t, печатает "Bell"
   (frp-emit sensor1 nil) ; bell = nil, ничего не печатется
   (frp-emit sensor1 1)   ; bell = t, печатает "Bell"
   (frp-emit sensor1 1)   ; печатает "Bell"


(frp-or left right)

Создает новый поток. Если текущее значение left отлично от nil, выбирает его. Иначе выбирает значение rihgt. Если оба входа равны nil, результат тоже nil.
Функция проводит выбор по текущим состояниям, поэтому обновление правого входа при активном левом снова выдаст значение левого:

::
   (defvar left (frp-event))
   (defvar right (frp-event))
   (defvar choice (frp-or left right))
   (frp-react choice #'(lambda (x) (print x)))

   (frp-emit right 'B)  ; B
   (frp-emit left 'A)   ; A
   (frp-emit right 'C)  ; A
   (frp-emit left nil)  ; C
   (frp-emit right nil) ; ничего не происходит

Фильтрация потока событий
-------------------------

(frp-filter stream predicate)

Принимает поток и чистую функцию одного аргумент, возвращает уже новый поток. Если вход активен, то вызывает предикат, при результате, отличном от nil, выход получает исходное значение, а при ложном результате выход становится nil. При входном nil предикат не вызывается, а выход сбрасывается.

::
   (defvar sensor (frp-event))
   (defvar above-one (frp-filter sensor #'(lambda (x) (> x 1))))
   (frp-react above-one #'(lambda (x) (print x)))

   (frp-emit sensor 3) ; печатает 3
   (frp-emit sensor 1) ; не печатает
   (frp-emit sensor 4) ; печатает 4

Преобразование потока
---------------------

(frp-map stream function) ; с одним входом

(frp-map left right function) с двумя входами

Создает новый поток и применяет к активным значениям входа чистую функцию одного или двух аргументов, значение результата функции становится значением выходного потока.

Функция для двух потоков принимает два потока и чистую функцию двух аргументов, возвращает новый поток. При обновлении любого входа берет текущие значения обоих, если они активны, то передает значение left первым аргументом, а значение right вторым. Если хотя бы одно равно nil, функция не вызывается, реакции нет.

::
   (defvar sensor (frp-event))
   (defvar incremented (frp-map sensor #'(lambda (x) (+ x 1))))
   (frp-react incremented #'(lambda (x) (print x)))

   (frp-emit sensor 2)   ; печатает 3
   (frp-emit sensor 2)   ; печатает 3
   (frp-emit sensor 0)   ; печатает 1, 0 обрабатывается как обычное число
   (frp-emit sensor nil) ; функция не вызывается

   (defvar left (frp-event))
   (defvar right (frp-event))
   (defvar total (frp-map left right #'(lambda (x y) (+ x y))))
   (frp-react total #'(lambda (x) (print x)))

   (frp-emit left 2)    ; ничего не печатает, right = nil
   (frp-emit right 3)   ; 5
   (frp-emit left 4)    ; печатает 7, right все еще равен 3
   (frp-emit right 3)   ; печатает 7
   (frp-emit left nil)  ; ничего не печатает, total = nil
   (frp-emit right 10)  ; ничего не печатает, left = nil
   (frp-emit left 0)    ; печатает 10
   
Пример: сумматор
================

::
   (defun xor (a b)
      "Исключающее ИЛИ"
      (frp-map a b #'(lambda(a b) (if (!= a b) 1 0))))

   ;; также делаем el-and и el-or через frp-map

   (defun sum1(a b p)
      "Сумматор одного разряда"
      (let* ((xorab (xor a b)) ; промежуточный узел
             (sum (xor xorab p)) ;; выход для суммы
             (p (el-or (el-and a b) (el-and p xorab)))) ;; выход переноса
	 #'(lambda (x) ; выходы sum и p
	     (case x ('SUM sum) ('P p)))))

   (defun sum2(x1 x2 y1 y2)
      "Полный сумматор 2 разряда"
      (let* ((p0 (frp-event 0))
             (h1 (sum1 x1 y1 p0)) ; первый разряд
             (h2 (sum1 x2 y2 (h1 'p)))) ; второй разряд
	  #'(lambda (x) ; выходы o1 o2 p и комбинация всего
	     (case x ('o1 (h1 'sum))
	             ('o2 (h2 'sum))
	             ('p (h2 'p))
		     ('all (frp-map (h1 'sum) (h2 'sum) (h2 'p) #'(lambda(o1 o2 o3) (list o1 o2 o3))))))))

   (setq x1 (frp-event 1)) ; 01
   (setq x2 (frp-event 0))
   (setq y1 (frp-event 1)) ; 11
   (setq y2 (frp-event 1))
   (setq sum (sum2 x1 x2 y1 y2))
   (frp-react (sum 'all) #'(lambda(all) (print all)))
   (frp-emit x1 1) ; напечатает список (0 0 1)
