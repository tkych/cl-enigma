;;;; Last modified : 2013-08-03 13:48:14 tkych

;; cl-enigma/cl-enigma.asd


;;====================================================================
;; CL-ENIGMA: 
;;====================================================================
;; cl-enigma/
;;   cl-enigma.asd
;;   enigma.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for CL-ENIGMA
;;====================================================================

(asdf:defsystem #:cl-enigma
  :name        "cl-enigma"
  :description "Enigma Machine Simulator for Common Lisp."
  :version     "0.1.00"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>" 
  :components  ((:file "enigma"))
  )

;;====================================================================
