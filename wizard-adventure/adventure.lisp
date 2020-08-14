; Contains descriptions of the locations that exist in our game.
(defvar *nodes*
  
  '((living-room
      (you are in the living-room. a wizard is snoring loudly on the couch.))
    
    (garden
      (you are in a beautiful garden. there is a well in front of you.))
    
    (attic
      (you are in the attic. there is a giant welding torch in the corner.))))

; NOTE: the above code uses symbols instead of strings. This is an old-school
; Lisp technique. Strings will be introduced in Chapter 11.

; To find an item in the list using a key:
; 	(assoc 'garden *nodes*)

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

; Contains descriptions of paths to other locations.
(defvar *edges*
  
  '((living-room
      (garden west     door)
      (attic  upstairs ladder))

    (garden
      (living-room east door))

    (attic
      (living-room downstairs ladder))))

; This is like a printf funciton.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; NOTE:
;   in the above function, 
;     (cdr (assoc 'living-room *edges*)) 
;   looks up the location in the list of edges.

; NOTE:
;   next, the edges are converted to descriptions.
;     (mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))

; NOTE:
;   the mapcar function takes another function and a list, and then applies this
;   function to every member of a list. Here's an example:
;     (mapcar #'1+ '(1 2 3))
;   will produce: (2 3 4)

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; NOTE:
;   append joins several lists into one big list.

; List of game objects.
(defvar *objects* '(whiskey bucket frog chain))

; A list to track the location of each object.
(defvar *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))

(defun objects-at (location objects object-locations)
  (labels ((at-location (object)
                       (eq (cadr (assoc object object-locations)) location)))
    (remove-if-not #'at-location objects)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-object (object)
                            `(you see a ,object on the floor.)))
    (apply #'append (mapcar #'describe-object 
                            (objects-at location objects object-locations)))))


