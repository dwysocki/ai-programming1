(defconstant *locations*
  '(living-room garden attic))

;; ACTIONS ;;

(defun look ()
  (let ((location (get-location)))
    (mapcar #'print-list (describe-location location))
    (mapcar #'print-list (describe-paths-from location))
    (mapcar #'print-list (describe-objects-in location))
    NIL))

(defun pickup (obj)
  (let* ((location (get-location))
         (location-objects (get location 'objects)))
    (if (find obj location-objects)
      (let ((your-objects (get 'you 'objects)))
        (setf (get 'you 'objects)
              (cons obj your-objects))
        (setf (get location 'objects)
              (remove obj location-objects
                      :count 1))
        nil)
      (format t "Cannot pick up ~A in location ~A" obj location))))

(defun inventory ()
  (let ((inventory (get 'you 'objects)))
    (if (null inventory)
      (print "YOU ARE CARRYING NOTHING AT ALL")
      (format t "YOU ARE CARRYING ~{THE ~A~^ AND ~}" inventory))))

(defun walk (direction)
  (let* ((location (get-location))
         (paths    (get location 'paths)))
    (doseq (path paths)
           (when (equal direction (path-direction path))
             (setf (get 'you 'location)
                   (path-destination path))
             (return-from walk)))
    (format t "Sorry - you cannot move ~A from the ~A" direction location)))

;; WORLD INITIALIZATION ;;

(defun bind-property (locations property-name properties)
  (mapcar (lambda (location property) (setf (get location property-name)
                                            property))
          locations
          properties))

(defun init-descriptions ()
  (bind-property *locations* 'description
                 '(((you are in the living-room)
                    (a wizzard is snoring loudly on the couch))

                   ((you are in a beautiful garden)
                    (there is a well in front of you))

                   ((you are in the attic)
                    (there is a giant welding torch in the corner)))))

(defun init-paths ()
  (bind-property *locations* 'paths
                 '(((garden west door)
                    (attic upstairs ladder))

                   ((living-room east door))

                   ((living-room downstairs ladder)))))

(defun init-objects ()
  (bind-property *locations* 'objects
                 '((whiskey bucket)

                   (frog chain)

                   ())))

(defun init-player ()
  (setf (get 'you 'location) (random-element *locations*))
  (setf (get 'you 'objects)  ()))

(defun init-world ()
  (init-descriptions)
  (init-paths)
  (init-objects)
  (init-player)

  nil)

;; ACCESSORS ;;

(defun path-destination (path)
  (first  path))

(defun path-direction   (path)
  (second path))

(defun path-connector   (path)
  (third  path))

(defun get-location ()
  (get 'you 'location))

;; DESCRIPTORS ;;

(defun pls (locations)
  "Print Locations (even though it doesn't print)"
  (mapcar #'symbol-plist locations))

(defun dw ()
  "Display World"
  (doseq (thing '(living-room garden attic you))
         (print thing)
         (print (symbol-plist thing)))
  (format t "~%~%")
  NIL)

(defun describe-location (location)
  (get location 'description))


(defun describe-path (path)
  (list 'there 'is 'a
        (path-connector path)
        'going
        (path-direction path)
        'from 'here))

(defun describe-obj (obj)
  (list 'you 'see 'a
        obj
        'on 'the 'floor))

(defun describe-paths-from (location)
  (mapcar #'describe-path (get location 'paths)))

(defun describe-objects-in (location)
  (mapcar #'describe-obj (get location 'objects)))


;; UTILITIES ;;
(defun random-element (list)
  (nth (random (length list))
       list))

(defun print-list (list)
  (format t "~{~a~^ ~}~%" list))
