; returns: список, в котором элементы увеличены в N раз.
; warning: Если элемент - список, то все его элементы умножаются на N.
;          Прим. (multyplyAllElemnts '(1 (1(1)) 1) 2) Вывод: (1 (2(2)) 1) 2 2 1 2)
(defun multyplyAllElemnts(numbers N)
  (cond
      ((null numbers) ())
      ((atom numbers)  (* N numbers))
      (t (cons (multyplyAllElemnts(car numbers) N) (multyplyAllElemnts(cdr numbers) N)) )
  )
)

; returns: список, в котором элменты на чётных позициях увеличены в N раз.
; warning: Если элемент на чётной позиции - список, то все его элементы умножаются на N.
;          Прим. (multyplyElementsAtEvenPos '(1 (1(1)) 1) 2) Вывод: (1 (2(2)) 1)
(defun multyplyElementsAtEvenPos(numbers N)
  (cond
      ((EQUAL 1 (length numbers)) (list(car numbers)))
      (t 
        (append (list (car numbers))
                (list (multyplyAllElemnts(cadr numbers) N))            
                (if(null (cddr numbers)) 
                    ()
                    (multyplyElementsAtEvenPos (cddr numbers) N)
                )
        )
      )
  )
)

; returns: есть ли в списке elements атом x.
(defun anyEqual(elements x)
  (cond
      ((null elements) NIL)
      ((atom elements)  (EQUAL x elements))
      (t 
        (or 
          (anyEqual (car elements) x)
          (anyEqual (cdr elements) x)
        )
      )
  )
)

; returns: пересечение атомов множеств lhs и rhs.
(defun intersect(lhs rhs)
  (cond
      ((null lhs) NIL)
      (t
        (append
          (if (anyEqual rhs (car lhs))
              (list (car lhs))
              ()
          )
          (intersect (cdr lhs) rhs)
        )
      )
  )
)

; 7. (a) напишите функцию, имеющую два аргумента: числовой список и целое число N. Функция должна возвращать список, в котором элементы, стоящие на четных позициях, увеличены в N раз.
(defun aShow()
  (format t "(multyplyElementsAtEvenPos '(1 1 1 1 1 1 1 1 1 1) = ~D~%\(multyplyElementsAtEvenPos '(1 (1 1 1 (1 1 1)) 1 1 1 1) 3) = ~D~%" 
      (multyplyElementsAtEvenPos '(1 1 1 1 1 1 1 1 1 1 1) 2)
      (multyplyElementsAtEvenPos '(1 (1 1 1 (1 1 1)) 1 1 1 1) 3)
  )
)
; 7. (б) Определите функцию, на вход которой подаются два списка – множества. Функция должна искать пересечение этих множеств.
(defun bShow()
  (format t "(intersect '(1 2 3 4 5) '(5 2 1)) = ~D~%\(intersect '(1 2 3 4 5) '(6)) = ~D~%" 
      (intersect '(1 2 3 4 5) '(5 2 1)) 
      (intersect '(1 2 3 4 5) '(6))
  )
)


