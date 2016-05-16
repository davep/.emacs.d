;;; constellations.el --- Constellation tools.
;; Copyright 2005 by Dave Pearson <davep@davep.org>
;; $Revision: 1.2 $

;; constellations.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; constellations.el provides a constellation abbreviation lookup tool. I
;; can never remember the abbreviations for most constellations so, when I'm
;; writing my observing logs (http://www.davep.org/astronomy/logs/) I find
;; this little tool handy -- saves me from having to remember.
;;
;; The latest constellations.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/#constellations.el>

;;; INSTALLATION:
;;
;; o Drop constellations.el somwehere into your `load-path'. Try your
;;   site-lisp directory for example (you might also want to byte-compile the
;;   file).
;;
;; o Add the following autoload statement to your ~/.emacs file:
;;
;;   (autoload 'constellation-insert-abbr "constellations" "Insert constellation abbreviation from name." t)

;;; TODO:
;;
;; All sorts of extra stuff could be added. The first obvious tool would be
;; a method of going the other way (abbreviation to name).

;;; Code:

(defvar constellation-names '(("Andromeda"           . "And")
                              ("Antlia"              . "Ant")
                              ("Apus"                . "Aps")
                              ("Aquarius"            . "Aqr")
                              ("Aquila"              . "Aql")
                              ("Ara"                 . "Ara")
                              ("Aries"               . "Ari")
                              ("Auriga"              . "Aur")
                              ("Boötes"              . "Boo")
                              ("Caelum"              . "Cae")
                              ("Camelopardalis"      . "Cam")
                              ("Cancer"              . "Cnc")
                              ("Canes Venatici"      . "CVn")
                              ("Canis Major"         . "CMa")
                              ("Canis Minor"         . "CMi")
                              ("Capricornus"         . "Cap")
                              ("Carina"              . "Car")
                              ("Cassiopeia"          . "Cas")
                              ("Centaurus"           . "Cen")
                              ("Cepheus"             . "Cep")
                              ("Cetus"               . "Cet")
                              ("Chamaeleon"          . "Cha")
                              ("Circinus"            . "Cir")
                              ("Columba"             . "Col")
                              ("Coma Berenices"      . "Com")
                              ("Corona Australis"    . "CrA")
                              ("Corona Borealis"     . "CrB")
                              ("Corvus"              . "Crv")
                              ("Crater"              . "Crt")
                              ("Crux"                . "Cru")
                              ("Cygnus"              . "Cyg")
                              ("Delphinus"           . "Del")
                              ("Dorado"              . "Dor")
                              ("Draco"               . "Dra")
                              ("Equuleus"            . "Eql")
                              ("Eridanus"            . "Eri")
                              ("Fornax"              . "For")
                              ("Gemini"              . "Gem")
                              ("Grus"                . "Gru")
                              ("Hercules"            . "Her")
                              ("Horologium"          . "Hor")
                              ("Hydra"               . "Hya")
                              ("Hydrus"              . "Hyi")
                              ("Indus"               . "Ind")
                              ("Lacerta"             . "Lac")
                              ("Leo"                 . "Leo")
                              ("Leo Minor"           . "LMi")
                              ("Lepus"               . "Lep")
                              ("Libra"               . "Lib")
                              ("Lupus"               . "Lup")
                              ("Lynx"                . "Lyn")
                              ("Lyra"                . "Lyr")
                              ("Mensa"               . "Men")
                              ("Microscopium"        . "Mic")
                              ("Monoceros"           . "Mon")
                              ("Musca"               . "Mus")
                              ("Norma"               . "Nor")
                              ("Octans"              . "Oct")
                              ("Ophiuchus"           . "Oph")
                              ("Orion"               . "Ori")
                              ("Pavo"                . "Pav")
                              ("Pegasus"             . "Peg")
                              ("Perseus"             . "Per")
                              ("Phoenix"             . "Phe")
                              ("Pictor"              . "Pic")
                              ("Pisces"              . "Psc")
                              ("Piscis Austrinus"    . "PsA")
                              ("Puppis"              . "Pup")
                              ("Pyxis"               . "Pyx")
                              ("Reticulum"           . "Ret")
                              ("Sagitta"             . "Sge")
                              ("Sagittarius"         . "Sgr")
                              ("Scorpius"            . "Sco")
                              ("Sculptor"            . "Scl")
                              ("Scutum"              . "Sct")
                              ("Serpens"             . "Ser")
                              ("Sextans"             . "Sex")
                              ("Taurus"              . "Tau")
                              ("Telescopium"         . "Tel")
                              ("Triangulum"          . "Tri")
                              ("Triangulum Australe" . "TrA")
                              ("Tucana"              . "Tuc")
                              ("Ursa Major"          . "UMa")
                              ("Ursa Minor"          . "UMi")
                              ("Vela"                . "Vel")
                              ("Virgo"               . "Vir")
                              ("Volans"              . "Vol")
                              ("Vulpecula"           . "Vul"))
  "List of constellations and their abbreviations.")

;;;###autoload
(defun constellation-insert-abbr (name)
  "Given constellation name NAME, insert its abbreviation."
  (interactive (list (completing-read "Name: " constellation-names)))
  (when name
    (let ((const (assoc name constellation-names)))
      (when const
        (insert (cdr const))))))

(provide 'constellations)

;;; constellations.el ends here.
