Last modified : 2013-08-03 16:06:50 tkych

Version: 0.1.00 (beta)


CL-Enigma
=========

cl-enigma is a simulator of the [enigma machine](https://en.wikipedia.org/wiki/Enigma_machine) for Common Lisp.
The enigma machine is a secret German cipher machine during WW2 period.
It was broken at Bletchley Park, England.

This is a toy project.
I enjoyed coding the enigma machine.
I feel happy as long as you're having a good time (:-D)


Reference
---------

 * [www.enigmaco.de enigma](http://enigmaco.de/enigma/enigma.html)
 * [The Enigma cipher machine](http://www.codesandciphers.org.uk/enigma/index.htm)
   * [Technical Specification of the Enigma](http://www.codesandciphers.org.uk/enigma/rotorspec.htm)
 * Eiichi, Wada [Enigmaへの挑戦](http://www.ipsj.or.jp/07editj/promenade/4510.pdf),
   Program Promenade, IPSJ Magazine, vol.45, No.10, Oct. 2004.
 * [Numberphile](http://www.numberphile.com/index.html)
   * [158,962,555,217,826,360,000](http://www.numberphile.com/videos/enigma.html)
   * [Flaw in the Enigma Code](http://www.numberphile.com/videos/enigma_flaw.html)
   * [Enigma Extra Footage](http://www.youtube.com/watch?v=BdrrJ7qd4HA)
 * [Dirk Rijmenants' Cipher Machines and Cryptology, Enigma Page](http://users.telenet.be/d.rijmenants/en/enigmamenu.htm)
 * [Paper Enigma Machine by Mike Koss](http://mckoss.com/Crypto/Enigma.htm)


Depends-on
----------

 * Independent


Installation
------------

 0. SHELL$   `git clone https://github.com/tkych/cl-enigma.git`
 1. CL-REPL> `(push #p"/path-to-cl-enigma/cl-enigma/" asdf:*central-registry*)`
 2. CL-REPL> `(ql:quickload :cl-enigma)` or `(asdf:load-system :cl-enigma)`


Examples
--------

```lisp
ENIGMA> (machine B
                 (set-rotor Beta  #\G)
                 (set-rotor Gamma #\B)
                 (set-rotor VIII  #\D)
                 '((a y) (l x))
                 "SUPERCALIFRAGILISTICEXPIALIDOCIOUS")
=> "NPFJDLEGBRBUVNZVFXSJHTXUMUGQSZGUWG"

ENIGMA> (machine B
                 (set-rotor Beta  #\G)
                 (set-rotor Gamma #\B)
                 (set-rotor VIII  #\D)
                 '((a y) (l x))
                 *)
=> "SUPERCALIFRAGILISTICEXPIALIDOCIOUS"
```


Referece Manual
---------------

#### [Function] MACHINE _reflector_ _left-rotor_ _middle-rotor_ _right-rotor_ _plugs_ _input-string_ => _encoded/decoded-string_

Encode/Decode _input-string_.

Argumants:

 * _reflector_ is a one of reflectors (built-in reflector A,B,C, or user defined reflector).
 * _left-rotor_, _middle-rotor_, _right-rotor_ are a one of rotors (built-in rotor I,II,III,IV,V,VI,VII,VIII,Beta,Gamma or user defined rotor).
 * _plugs_ is a list of a two characters/symbols which interchanges each other.
e.g. _plugs_, `((#\a #\b) (c d))` makes plugboard a <-> b and c <-> d.
 * _input-string_ is a string to be encode/decode by machine.


#### [Function] SET-ROTOR _rotor_ _init-position_ => _rotor_

Set  position of the _rotor_ to _init-position_.
_init-position_ must be a character (a...z) or an integer (1...26).


#### [Function] MAKE-ROTOR _to-alphabet_ _&rest_ _notch-characters_ => _rotor_

Make a rotor.

_to-alphabet_ is permutation of \*ALPHABET\*.
_notch-characters_ are characters which replesents the position of turnover.

e.g.
`(make-rotor "BACDEFGHIJKLMNOPQRSTUVWXYZ" #\N #\W)`
makes rotor which interchanges `A <-> B` and has notchs for `#\N` and `#\W`.


#### [Special Variables] I, II, III, IV, V, VII, VIII, Beta, Gamma

Built-in rotors.

Definitions:

 * I     := `(make-rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" #\Q)`
 * II    := `(make-rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" #\E)`
 * III   := `(make-rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" #\V)`
 * IV    := `(make-rotor "ESOVPZJAYQUIRHXLNFTGKDCMWB" #\J)`
 * V     := `(make-rotor "VZBRGITYUPSDNHLXAWMJQOFECK" #\Z)`
 * VI    := `(make-rotor "JPGVOUMFYQBENHZRDKASXLICTW" #\Z #\M)`
 * VII   := `(make-rotor "NZJHGRCXMYSWBOUFAIVLPEKQDT" #\Z #\M)`
 * VIII  := `(make-rotor "FKQHTLXOCBJSPDZRAMEWNIUYGV" #\Z #\M)`
 * Beta  := `(make-rotor "LEYJVCNIXWPBQMDRTAKZGFUHOS")`
 * Gamma := `(make-rotor "FSOKANUERHMBTIYCWLQPZXVGJD")`


#### [Function] MAKE-REFLECTOR _to-alphabet_ => _reflector_

Make a reflector.

_to-alphabet_ is permutation of \*ALPHABET\*.
 

#### [Special Variables] A, B, C

Built-in reflectors.

Definitions:

 * A := `(make-reflector "EJMZALYXVBWFCRQUONTSPIKHGD")`
 * B := `(make-reflector "YRUHQSLDPXNGOKMIEBFZCWVJAT")`
 * C := `(make-reflector "FVPJIAOYEDRZXWGCTKUQSBNMHL")`


#### [Special Variable] \*ALPHABET\*

String of alphabet, default is `"ABCDEFGHIJKLMNOPQRSTUVWXYZ"`.


TODO
----

 * ADD: local-alphabet extension.


Author, License, Copyright
--------------------------

 - Takaya OCHIAI  <#.(reverse "moc.liamg@lper.hcykt")>

 - MIT License

 - Copyright (C) 2013 Takaya OCHIAI
