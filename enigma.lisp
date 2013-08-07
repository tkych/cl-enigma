;;;; Last modified : 2013-08-07 21:05:29 tkych

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

(defmacro -> (x &body forms)
  "Threads the `x' through the `forms'. Inserts `x' as the
second item in the first form, making a list of it if it is not a
list already. If there are more forms, inserts the first form as the
second item in second form, etc.
c.f. clojure's `->', http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/->"
  (cond ((null forms)
         x)
        ((null (cdr forms))
         (let ((first-form (car forms)))
           (if (consp first-form)
               `(,(car first-form) ,x ,@(cdr first-form))
               `(,first-form ,x))))
        (t
         `(-> (-> ,x ,(car forms)) ,@(cdr forms)))))


;;--------------------------------------------------------------------
;; Main
;;--------------------------------------------------------------------

(defparameter *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The alphabet string used in a plaintext and a cyphertext.
Default is \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\".")

(defgeneric rotate     (rotor))
(defgeneric convert    (char-num convertor))
(defgeneric invert     (char-num invertor))
(defgeneric reflect    (char-num reflector))
(defgeneric turnover-p (rotor))

(defclass convertor-mixn ()
  ((convertor :type simple-vector :reader convertor :initarg :convertor)))

(defclass invertor-mixn ()
  ((invertor :type simple-vector :reader invertor :initarg :invertor)))

;; The form (generate-exchanger "abcdefghijklmnopqrstuvwxyz"
;;                              "bacdefghijklmnopqrstuvwxyz")
;; generates exchange-vector, s.t. a->b and b->a .
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
  ((to-alphabet     :type string  :reader   to-alphabet     :initarg :to-alphabet)
   (notch-positions :type list    :reader   notch-positions :initarg :notch-positions)
   (bar-position    :type integer :accessor bar-position    :initarg :bar-position)))

(defun make-rotor (to-alphabet &rest notch-chars)
  "Make a rotor.
TO-ALPHABET is a permutation of *alphabet*.
NOTCH-CHARACTERS are a characters which replesents the position of turnover.
e.g.
 (make-rotor \"BACDEFGHIJKLMNOPQRSTUVWXYZ\" #\N #\W) makes rotor which
interchanges A <-> B and has notchs for #\N and #\W."
  (unless (string-equal (sort (copy-seq to-alphabet) #'< :key #'char-code)
                        (sort (copy-seq *alphabet*) #'< :key #'char-code))
    (error "~S is not a permutation of alphabet ~S."
           to-alphabet *alphabet*))
  (make-instance 'rotor
                 :notch-positions (mapcar (lambda (notch-char)
                                            (position notch-char *alphabet*
                                                      :test #'char-equal))
                                          notch-chars)
                 :to-alphabet     to-alphabet
                 :bar-position    0
                 :convertor       (generate-exchanger *alphabet* to-alphabet)
                 :invertor        (generate-exchanger to-alphabet *alphabet*)))

(defgeneric set-rotor  (rotor init-position)
  (:documentation
   "Set bar-position of the ROTOR to INIT-POSITION.
INIT-POSITION must be a character (a...z) or an integer (1...26)."))

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
  "Make a reflector.
TO-ALPHABET is a permutation of *alphabet*."
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
(defparameter I    (make-rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" #\Q)
  "Built-in rotor. I := (make-rotor \"EKMFLGDQVZNTOWYHXUSPAIBRCJ\" #\\Q)")
(defparameter II   (make-rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" #\E)
  "Built-in rotor. II := (make-rotor \"AJDKSIRUXBLHWTMCQGZNPYFVOE\" #\\E)")
(defparameter III  (make-rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" #\V)
  "Built-in rotor. III := (make-rotor \"BDFHJLCPRTXVZNYEIWGAKMUSQO\" #\\V)")
(defparameter IV   (make-rotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" #\J)
  "Built-in rotor. IV := (make-rotor \"ESOVPZJAYQUIRHXLNFTGKDCMWB\" #\\J)")
(defparameter V    (make-rotor "VZBRGITYUPSDNHLXAWMJQOFECK" #\Z)
  "Built-in rotor. V := (make-rotor \"VZBRGITYUPSDNHLXAWMJQOFECK\" #\\Z)")
(defparameter VI   (make-rotor "JPGVOUMFYQBENHZRDKASXLICTW" #\Z #\M)
  "Built-in rotor. VI := (make-rotor \"JPGVOUMFYQBENHZRDKASXLICTW\" #\\Z #\\M)")
(defparameter VII  (make-rotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" #\Z #\M)
  "Built-in rotor. VII := (make-rotor \"NZJHGRCXMYSWBOUFAIVLPEKQDT\" #\\Z #\\M)")
(defparameter VIII (make-rotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" #\Z #\M)
  "Built-in rotor. VIII := (make-rotor \"FKQHTLXOCBJSPDZRAMEWNIUYGV\" #\\Z #\\M)")
(defparameter Beta  (make-rotor "LEYJVCNIXWPBQMDRTAKZGFUHOS") ;no-notch
  "Built-in rotor. Beta := (make-rotor \"LEYJVCNIXWPBQMDRTAKZGFUHOS\")")
(defparameter Gamma (make-rotor "FSOKANUERHMBTIYCWLQPZXVGJD") ;no-notch
  "Built-in rotor. Gamma := (make-rotor \"FSOKANUERHMBTIYCWLQPZXVGJD\")")

;; reflectors
(defparameter A (make-reflector "EJMZALYXVBWFCRQUONTSPIKHGD")
  "Built-in reflector. A := (make-reflector \"EJMZALYXVBWFCRQUONTSPIKHGD\")")
(defparameter B (make-reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT")
  "Built-in reflector. B := (make-reflector \"YRUHQSLDPXNGOKMIEBFZCWVJAT\")")
(defparameter C (make-reflector "FVPJIAOYEDRZXWGCTKUQSBNMHL")
  "Built-in reflector. C := (make-reflector \"FVPJIAOYEDRZXWGCTKUQSBNMHL\")")


;;--------------------------------------
;; Machine
;;--------------------------------------

(defgeneric machine (reflector
                     left-rotor middle-rotor right-rotor
                     plugs
                     input-string)
  (:documentation
   "Encode/Decode INPUT-STRING.
 * REFLECTOR is a one of reflectors (built-in reflector A,B,C, or user defined reflector).
 * LEFT-ROTOR, MIDDLE-ROTOR, RIGHT-ROTOR are a one of rotors
   (built-in rotor I,II,III,IV,V,VI,VII,VIII,Beta,Gamma or user defined rotor).
 * PLUGS is a list of a two characters/symbols which interchanges each other.
   e.g. plugs, ((#\a #\b) (c d)) makes plugboard a <-> b and c <-> d.
 * INPUT-STRING is a string to be encode/decode by machine."))

(defmethod machine ((reflector reflector)
                    (left-rotor rotor) (middle-rotor rotor) (right-rotor rotor)
                    plugs
                    (input-string string))
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
                      (-> char-num
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

;; (assert (string=
;;          (machine
;;           B (set-rotor I #\a) (set-rotor II #\a) (set-rotor III #\a) nil
;;           "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
;;          "bdzgowcxltksbtmcdlpbmuqofxyhcxtgyjflinhnxshiuntheorxpqpkovhcbubtzszsoostgotfsodbbzzlxlcyzxifgwfdzeeqibmgfjbwzfckpfmgbxqcivibbrncocjuv"))

;; (assert (string=
;;          (machine
;;           B (set-rotor I #\a) (set-rotor II #\a) (set-rotor III #\a) nil
;;           "bdzgowcxltksbtmcdlpbmuqofxyhcxtgyjflinhnxshiuntheorxpqpkovhcbubtzszsoostgotfsodbbzzlxlcyzxifgwfdzeeqibmgfjbwzfckpfmgbxqcivibbrncocjuv")
;;          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))


;;====================================================================
