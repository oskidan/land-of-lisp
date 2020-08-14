; NOTE:
;   defparameter - defines a new variable. It will overwrite previous values of
;                  a global variable.
;   defvar       - defines a new variable. It won't overwrite previous values of
;                  a global variable.
(defparameter *big* 100)
(defparameter *small* 1)

; NOTE:
;   as far as I understand, 'ash' stands for 'Argument SHift'. The following
;   code is equivalent to this C/C++: `(big + small) >> 1`.
;
; Examples of the 'ash' function:
;   (ash 1  1) ; is 2
;   (ash 2  1) ; is 4
;   (ash 1  3) ; is 8
;   (ash 8 -1) ; is 4
;   (ash 8 -3) ; is 1
(defun guess-my-number ()
  (ash (+ *big* *small*) -1))

; NOTE:
;   the 'setf' function changes the value of the given variable. 
;   The '1-' function subtracts 1 from its argument.
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

; NOTE:
;   the 'setf' function changes the value of the given variable. 
;   The '1+' function adds 1 to its argument.
(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *big* 100)
  (defparameter *small* 1)
  (guess-my-number))
