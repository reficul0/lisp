(defun power(num n)
  (if(= n 0)
    1
    (* num (power num ( - n 1 ))) ) )

(defun powerP ()
  (format t "~D~%" (power 2 2))
)

(defun factorial(n)
  (if(= n 0)
    1
    (* n (factorial ( - n 1 ))) ) )

(defun factorialP ()
  (format t "~D! = ~D~%" 2 (factorial 2)) )


(defun listOp(elements)
  (cond
      ((NULL elements) ())
      ((ATOM elements)  elements)
      ;((ATOM (car elements))  (cons (car elements) ( listOp(cdr elements) )))
      ;((LISTP (car elements)) (cons (listOp(car elements)) ( listOp(cdr elements) )))
      (t (cons (listOp(car elements)) ( listOp(cdr elements) )))
  )
)

(defun listOpP ()
  (format t "~D~%" (listOp '(1 (2))))
)

(defun sum(elements)
  (cond
      ((NULL elements) 0)
      ((ATOM elements)  elements)
      (t (+ (car elements) (sum(cdr elements))) )
  )
)
(defun sumP()
  (format t "~D" (sum '(1 2)))
)

;((atom (car list)) ( cons (car list)  (listOp(cdr list)) ))
;(t (cons (car list)  (listOp(cdr list) )))

;(#t (cons(car list) (replace-nth (cdr list) (- n 1) elem))))

; returns: список, в котором элементы увеличены в N раз.
; warning: Если элемент - список, то все его элементы умножаются на N.
;          Прим. (multyplyAllElemnts '(1 (1(1)) 1) 2) Вывод: (1 (2(2)) 1) 2 2 1 2)
(defun multyplyAllElemnts(numbers N)
  (cond
      ((NULL numbers) ())
      ((ATOM numbers)  (* N numbers))
      (t (cons (multyAll(car numbers) N) (multyAll(cdr numbers) N)) )
  )
)

; returns: список, в котором элменты на чётных позициях увеличены в N раз.
; warning: Если элемент на чётной позиции - список, то все его элементы умножаются на N.
;          Прим. (multyplyElementsAtEvenPos '(1 (1(1)) 1) 2) Вывод: (1 (2(2)) 1)
(defun multyplyElementsAtEvenPos(numbers N)
  (cond
      ((EQUAL 1 (length numbers)) (car numbers))
      (t 
        (append (list (car numbers))
                (list (multyplyAllElemnts(cadr numbers) N))                
                (if(NULL (cddr numbers)) 
                    ()
                    (multy2 (cddr numbers) N)
                )
        )
      )
  )
)

; 7. (a) напишите функцию, имеющую два аргумента: числовой список и целое число N. Функция должна возвращать список, в котором элементы, стоящие на четных позициях, увеличены в N раз.
(defun aShow()
  (format t "~D~%" (multyplyElementsAtEvenPos '(1 (1 1 1 (1 1 1)) 1 1 1 1) 2))
)

; returns: есть ли в списке elements элемент pred.
(defun anyEqual(elements pred)
  (cond
      ((NULL elements) NIL)
      ((ATOM elements)  (EQUAL pred elements))
      (t 
        (OR 
          (anyEqual (car elements) pred) 
          (anyEqual (cdr elements) pred)
        )
      )
  )
)

(defun intersect(lhs rhs)
  (cond
      ((NULL lhs) NIL)
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

; 7. (б) Определите функцию, на вход которой подаются два списка – множества. Функция должна искать пересечение этих множеств
(defun bShow()
  (format t "~D~%" (intersect '(1 2 3 4 5) '(5 2 1)))
)


(defun mem (elements x) 
   (cond
      ((null elements) ()) 
      ((equal (car elements) x) elements) 
      (t (mem (cdr elements) x))
    )
)
(defun inclus (lhs rhs) 
   (cond 
      ((null lhs) t) 
      ((mem (car lhs) rhs)(inclus (cdr lhs) rhs)) 
      (t ())
    )
)
; returns: есть ли в списке elements атом x.
(defun anyEqual2(elements x)
  (cond
      ((atom x)  (member x elements))
      ((null elements) NIL)
      (t 
        (or 
          (cond
            ((listp (car elements))  
              ;(and 
                  (inclus (list(car elements)) x) 
                   ;(inclus x (car elements)))
            )
            (t ())
          )
          (anyEqual (cdr elements) x)
        )
      )
  )
)
