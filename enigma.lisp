;;;; Last modified : 2013-08-03 14:59:21 tkych

;; cl-enigma/enigma.lisp


;;====================================================================
;; Enigma
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-enigma
  (:nicknames #:enigma)
  (:use :cl)
  (:export #:machine
           #:set-rotor
           #:make-rotor
           #:make-reflector
           #:*alphabet*
           #:I #:II #:III #:IV #:V #:VI #:VII #:VIII #:Beta #:Gamma
           #:A #:B #:C))

(in-package #:cl-enigma)


;;--------------------------------------------------------------------
;; Util
;;--------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro left-fold (x &body forms)
    "Inserts x as the second item in the first forms,
making a list of it if it is not a list already. If there are more
formss, inserts the first forms as the second item in second forms, etc.
c.f. clojure's `->', http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/-> "
    (cond ((null forms)
           x)
          ((null (cdr forms))
           (let ((first (car forms)))
             (if (consp first)
                 `(,(car first) ,x ,@(cdr first))
                 `(,first ,x))))
          (t
           `(left-fold (left-fold ,x ,(car forms)) ,@(cdr forms)))))

  ) ;end of eval-when


;;--------------------------------------------------------------------
;; Main
;;--------------------------------------------------------------------

(defparameter *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defclass convertor-mixn ()
  ((convertor :type simple-vector :reader convertor :initarg :convertor)))

(defclass invertor-mixn ()
  ((invertor :type simple-vector :reader invertor :initarg :invertor)))

(defgeneric set-rotor  (rotor init-position))
(defgeneric rotate     (rotor))
(defgeneric convert    (char-num convertor))
(defgeneric invert     (char-num invertor))
(defgeneric reflect    (char-num reflector))
(defgeneric turnover-p (rotor))

;; The form (generate-exchanger "abcdefghijklmnopqrstuvwxyz"
;;                              "bacdefghijklmnopqrstuvwxyz")
;; generates the exchanger, s.t. a->b and b->a .
(defun generate-exchanger (from-alphabet to-alphabet)
  (let ((sv (make-array (length *alphabet*) :element-type t)))
    (loop
       :for c :across to-alphabet
       :for i :from 0
       :do (setf (svref sv i)
                 (position c from-alphabet :test #'char-equal)))
    sv))


;;--------------------------------------
;; Rotors
;;--------------------------------------

(defclass rotor (convertor-mixn invertor-mixn)
  ((notch-positions :type list    :reader   notch-positions :initarg :notch-positions)
   (to-alphabet     :type string  :reader   to-alphabet     :initarg :to-alphabet)
   (bar-position    :type integer :accessor bar-position    :initarg :bar-position)))

(defun make-rotor (to-alphabet &rest notch-chars)
  (make-instance 'rotor
                 :notch-positions (mapcar (lambda (notch-char)
                                            (position notch-char *alphabet*
                                                      :test #'char-equal))
                                          notch-chars)
                 :to-alphabet     to-alphabet
                 :bar-position    0
                 :convertor       (generate-exchanger *alphabet* to-alphabet)
                 :invertor        (generate-exchanger to-alphabet *alphabet*)))

(defmethod set-rotor ((r rotor) (c character))
  (setf (bar-position r)
        (position c *alphabet* :test #'char-equal))
  r)

(defmethod set-rotor ((r rotor) (i integer))
  (unless (<= 1 i (length *alphabet*))
    (error "~D is not in range 1 <= i <= ~D."
           i (length *alphabet*)))
  (setf (bar-position r) (1- i))
  r)

(defmethod rotate ((r rotor))
  (with-slots (bar-position) r
    (setf bar-position
          (mod (1+ bar-position) (length *alphabet*))))
  r)

(defmethod turnover-p ((prev-rotor rotor))
  (with-slots (notch-positions bar-position) prev-rotor
    (some (lambda (pos) (= pos bar-position))
          notch-positions)))

(defmethod convert ((char-num integer) (r rotor))
  (with-slots (convertor bar-position) r
    (let ((num-alphabet (length *alphabet*)))
      (mod (- (svref convertor (mod (+ char-num bar-position)
                                    num-alphabet))
              bar-position)
           num-alphabet))))

(defmethod invert ((char-num integer) (r rotor))
  (with-slots (invertor bar-position) r
    (let ((num-alphabet (length *alphabet*)))
      (mod (- (svref invertor (mod (+ char-num bar-position)
                                   num-alphabet))
              bar-position)
           num-alphabet))))


;;--------------------------------------
;; Reflectors
;;--------------------------------------
(defclass reflector (convertor-mixn) ())

(defun make-reflector (to-alphabet)
  (make-instance 'reflector
                 :convertor (generate-exchanger *alphabet* to-alphabet)))

(defmethod reflect ((char-num integer) (r reflector))
  (svref (convertor r) char-num))


;;--------------------------------------
;; Plugboards
;;--------------------------------------

(defclass plugboard (convertor-mixn invertor-mixn) ())

(defun make-plugboard (plugs)
  (flet ((to-char (a) (etypecase a
                        (character a)
                        (string    (char a 0))
                        (symbol    (char (string a) 0)))))
    (let ((to-alphabet (loop
                          :with alphabet := (copy-seq *alphabet*)
                          :for (a1 a2) :in plugs
                          :do (let* ((p1 (position (to-char a1) alphabet :test #'char-equal))
                                     (p2 (position (to-char a2) alphabet :test #'char-equal)))
                                (rotatef (char alphabet p1)
                                         (char alphabet p2)))
                          :finally (return alphabet))))
      (make-instance 'plugboard
                     :convertor (generate-exchanger *alphabet*  to-alphabet)
                     :invertor  (generate-exchanger to-alphabet *alphabet*)))))

(defmethod convert ((char-num integer) (pb plugboard))
  (svref (convertor pb) char-num))

(defmethod invert ((char-num integer) (pb plugboard))
  (svref (invertor pb) char-num))


;;--------------------------------------
;; Built-In Rotors and Reflectors
;;--------------------------------------
;; c.f. http://www.codesandciphers.org.uk/enigma/rotorspec.htm

;; rotors
(defparameter I    (make-rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" #\Q))
(defparameter II   (make-rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" #\E))
(defparameter III  (make-rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" #\V))
(defparameter IV   (make-rotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" #\J))
(defparameter V    (make-rotor "VZBRGITYUPSDNHLXAWMJQOFECK" #\Z))
(defparameter VI   (make-rotor "JPGVOUMFYQBENHZRDKASXLICTW" #\Z #\M))
(defparameter VII  (make-rotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" #\Z #\M))
(defparameter VIII (make-rotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" #\Z #\M))
(defparameter Beta  (make-rotor "LEYJVCNIXWPBQMDRTAKZGFUHOS"))
(defparameter Gamma (make-rotor "FSOKANUERHMBTIYCWLQPZXVGJD"))

;; reflectors
(defparameter A (make-reflector "EJMZALYXVBWFCRQUONTSPIKHGD"))
(defparameter B (make-reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT"))
(defparameter C (make-reflector "FVPJIAOYEDRZXWGCTKUQSBNMHL"))


;;--------------------------------------
;; Machine
;;--------------------------------------

(defun machine (reflector
                left-rotor middle-rotor right-rotor
                plugs
                input-string)
  (let ((plugboard   (make-plugboard plugs))
        (upper-case? (upper-case-p (char input-string 0))))
    (loop
       :for char :across input-string
       :for char-num := (position char *alphabet* :test #'char-equal)
       :do (progn
             (when (turnover-p middle-rotor)
               (rotate left-rotor)
               ;; double stepping
               (rotate middle-rotor))
             (when (turnover-p right-rotor)
               (rotate middle-rotor))
             (rotate right-rotor))
       :collect (char *alphabet*
                      (left-fold char-num
                        (convert plugboard)
                        (convert right-rotor)
                        (convert middle-rotor)
                        (convert left-rotor)
                        (reflect reflector)
                        (invert  left-rotor)
                        (invert  middle-rotor)
                        (invert  right-rotor)
                        (invert  plugboard)))
       :into acc
       :finally (return (if upper-case?
                            (string-upcase   (coerce acc 'string))
                            (string-downcase (coerce acc 'string)))))))


;;--------------------------------------------------------------------
;; Test
;;--------------------------------------------------------------------

;; (assert (string= (machine B
;;                           (set-rotor I   #\a)
;;                           (set-rotor II  #\b)
;;                           (set-rotor III #\w)
;;                           '((a b) (c d))
;;                           "aaaaaaaaaaaaaaaa")
;;                  "mlwicywkxqkroolc"))

;; (assert (string= (machine B
;;                           (set-rotor I   #\a)
;;                           (set-rotor II  #\b)
;;                           (set-rotor III #\w)
;;                           '((#\a #\b) (#\c #\d))
;;                           "aaaaaaaaaaaaaaaa")
;;                  "mlwicywkxqkroolc"))

;; (assert (string= (machine B
;;                           (set-rotor I   #\a)
;;                           (set-rotor II  #\b)
;;                           (set-rotor III #\w)
;;                           '((#\a #\b) (#\c #\d))
;;                           "mlwicywkxqkroolc")
;;                  "aaaaaaaaaaaaaaaa"))

;; (assert (string-equal
;;          (machine
;;           B (set-rotor I #\a) (set-rotor II #\a) (set-rotor III #\a) nil
;;           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
;;          "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTHEORXPQPKOVHCBUBTZSZSOOSTGOTFSODBBZZLXLCYZXIFGWFDZEEQIBMGFJBWZFCKPFMGBXQCIVIBBRNCOCJUV"))

;; (assert (string-equal
;;          (machine
;;           B (set-rotor I #\a) (set-rotor II #\a) (set-rotor III #\a) nil
;;           "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTHEORXPQPKOVHCBUBTZSZSOOSTGOTFSODBBZZLXLCYZXIFGWFDZEEQIBMGFJBWZFCKPFMGBXQCIVIBBRNCOCJUV")
;;          "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))


;;====================================================================
