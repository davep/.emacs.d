;;; constants.el --- enter constant definitions into source code
;; Copyright (c) 2003 Carsten Dominik

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Version: 1.1
;; Keywords: programming, languages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;---------------------------------------------------------------------------
;;
;;; Commentary:
;; 
;; This package provides a single function, `constants-insert'.  It
;; prompts for one or more variable names and inserts definition
;; statements for numerical constants into source code.
;;
;; The package knows many constants and units, both in the SI and in
;; the cgs unit system.  It also understands the usual unit prefixes
;; (like "M" for Mega=10^6 or "m" for milli=10^-3).  So the built-in
;; constant "pc" (parsec, an astronomical distance), can also be used
;; in kpc (1000 pc), Mpc (10^6 pc) etc.  For a full list of all
;; available constants, units, and prefixes, use `M-x constants-help'.
;;
;; The unit system (SI or cgs) can be selected using the option
;; `constants-unit-system'.  Additional constants or units can be
;; defined by customizing `constants-user-defined'.  You can tell the
;; package to use different names for some of the constants (option
;; `constants-rename').
;;
;; The code inserted into the buffer is mode dependent.  Constants.el
;; has defaults for some programming languages: FORTRAN, C, IDL,
;; MATLAB, OCTAVE, PERL, EMACS-LISP.  You can change these defaults
;; and add definitions for other languages with the variable
;; `constants-languages'.
;;
;; INSTALLATION
;; ------------
;; Byte-compile this file, put it on your load path and copy the
;; following code into your .emacs file.  Change the key definition,
;; variable name aliasing and the unit system to your liking.
;;
;;   (autoload 'constants-insert "constants" 
;;     "Insert constants into source code" t)
;;   (define-key global-map "\C-cc" 'constants-insert)
;;   (setq constants-unit-system 'SI)
;;
;;   ;; Use cc as the variable name for speed of light,
;;   ;; bk for Boltzmann's constant, and hp for Planck's constant
;;   (setq constants-rename '(("cc" . "c") ("bk" . "k") ("hp" . "h")))
;;
;;   ;; A default list of constants to insert when none are specified
;;   (setq constants-default-list "cc,bk,hp")
;;
;; USAGE
;; -----
;; In a programming mode, call the function and at the prompt enter
;; for example
;;
;;    Name[, Name2...}: cc,k,Mmoon,Mpc
;;
;; In a FORTRAN buffer, this would insert
;;
;;      doubleprecision cc
;;      parameter(cc=2.99792458d8)          ! Speed of light [SI]
;;      doubleprecision k
;;      parameter(k=1.3806503d-23)          ! Boltzmann's constant [SI]
;;      doubleprecision Mmoon
;;      parameter(Mmoon=7.35d22)            ! Moon mass [SI]
;;      doubleprecision Mpc
;;      parameter(Mpc=3.085677582d+22)      ! Mega-Parsec [SI]
;;
;; while in a C buffer you would get
;;
;;      double cc=2.99792458e8;           /* Speed of light [SI] */
;;      double k=1.3806503e-23;           /* Boltzmann's constant [SI] */
;;      double Mmoon=7.35e22;             /* Moon mass [SI] */
;;      double Mpc=3.085677582e+22;       /* Mega-Parsec [SI] */
;;
;; While entering the first name, you can use completion. Press `?' to
;; display a detailed list of all available constants (you can scroll
;; the Help window with S-TAB while entering text in the minibuffer).
;;
;; CUSTOMIZATION
;; -------------
;; You can use the following variables to customize this mode:
;;
;; constants-unit-system
;;   The unit system to be used for the constants (cgs or SI).
;;
;; constants-rename
;;   Alist with new new names for some constants.
;;
;; constants-user-defined
;;   User defined constants.
;;
;; constants-default-list
;;   Default constants to insert if none are specified.
;;
;; constants-languages
;;   Format descriptions for different major programming modes.
;;
;; constants-indent-code
;;   Non-nil means, indent the newly inserted code.
;;
;; constants-allow-prefixes
;;   Non-nil means, interpret prefixes like M (mega) etc.
;;
;; constants-prefixes
;;   Allowed prefixes for constants and units.
;;
;;
;; CONTEXT SENSITIVITY
;; -------------------
;; For some languages, it might be usefull to adapt the inserted code
;; to context.  For example, in Emacs Lisp mode, the default settings
;; insert "(VARIABLE VALUE)" with surrounding parenthesis for a `let'
;; form.  However, if you'd like to use this in a `setq' form, the
;; parenthesis are incorrect.  To customize for such a case, create a
;; buffer-local variable `constants-language-function' and set it to a
;; function which returns, after checking the context, the correct
;; entry of the form (MAJOR-MODE FORMAT EXP-STRING).  For the above
;; example, you could do this:
;;
;;   (defun my-constants-elisp-function ()
;;     "Check context for constants insertion."
;;     (save-excursion
;;       (condition-case nil
;;           (progn (up-list -1)
;;                  (if (looking-at "(setq\\>")
;;                      '(emacs-lisp-mode "%n %v%t; %d %u" "e")
;;                    '(emacs-lisp-mode "(%n %v)%t; %d %u" "e")))
;;         (error nil))))     ; return value nil means use default
;;
;;   ;; Create the local variable in Emacs Lisp mode
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'constants-language-function)
;;                    'my-constants-elisp-function)))
;;
;; BUGS
;; ----
;; - Completion should also work for second and subsequent names.
;; - When using cgs units, be very careful with the electric constants
;;   and units.  This library uses E.S.U., not E.M.U.  Note that the
;;   *equations* involving charges, currents and magnetism are all
;;   different for SI, CGS/ESU and CGS/EMU. So when switching to a
;;   different system, you must make sure to use the right equations.
;; - I have tried to implement the cgs units correctly, but I have
;;   some doubt about the electrical and radiation units.
;;   Double-check before blindly using these.
;;
;; AUTHOR
;; ------
;; Carsten Dominik <dominik@science.uva.nl>
;;
;; Let me know if you are missing a constant in the default setup, if
;; you notice that a value of a constant is not correct, or if you
;; would like to see support for another language mode.
;;
;; ACKNOWLEDGEMENTS
;; ----------------
;; Thanks to Kees Dullemond.  Watching him writing programs has
;; inspired this package.
;;
;; Thanks to the following people for reporting bugs and/or suggesting
;; features and additional constants:
;; Dave Pearson
;;
;; TO DO
;; -----
;; - Support more programming languages.
;; - Add calc mode, text mode, any others?
;; - Document aliasing, and support with customize.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defgroup constants nil 
  "Customization group for inserting constants into programs."
  :tag "Constants"
  :prefix "constants-"
  :group 'tools)

(defcustom constants-unit-system 'SI
  "The unit system to be used for the constants.  Legal values cgs and SI."
  :group 'constants
  :type '(choice
	  (const cgs)
	  (const SI)))

(defcustom constants-rename
  '(("kk" . "k") ("bk" . "k")
    ("cc" . "c") ("cl" . "c")
    ("hh" . "h") ("hp" . "h")
    )
  "Alist with new new names for some of the constants.
Sometime it is better in a program to use different names for constants,
for exapmle \"cc\" instead of \"c\" for the speed of light, in order
be able to use single letter variables for other purposes.  Here you
can specify a list of new names.  The cdr of each item must be the name
of a user-defined (variable `constants-user-defined') or default
constant (defined in the constant `constants-defaults')."
  :group 'constants
  :type
  '(repeat
    (cons (string "Variable name")
	  (string "Constant name"))))

(defcustom constants-user-defined nil
  "User defined constants for programs.
For each constant there is a list of 5 items: The long and a short
variable name, a brief description, and the values of the constant
in SI and cgs units, as strings.  For examples, see `constants-defaults'.
The description should be short, because it is placed into a comment
after the variable assignment."
  :group 'constants
  :type '(repeat
	  (list
	   (string :tag "Long name")
	   (string :tag "Short name")
	   (string :tag "Description")
	   (string :tag "SI  value")
	   (string :tag "cgs value"))))

(defcustom constants-default-list "hp,kk,cc,AU,Msun,Lsun,Grav"
  "Default constants to insert if none are specified."
  :group 'constants
  :type 'string)

(defcustom constants-languages
  '((fortran-mode "doubleprecision %n\nparameter(%n=%v)%t! %d %u" "d")
    (c-mode "double %n=%v;%t/* %d %u */" "e")
    (idlwave-mode "%n = %v%t; %d %u" "d")
    (matlab-mode "%n = %v%t% %d %u" "e")
    (octave-mode "%n = %v%t# %d %u" "e")
    (perl-mode  "$%n = %v;%t# %d %u" "e")
    (cperl-mode "$%n = %v;%t# %d %u" "e")
    (emacs-lisp-mode "(%n %v)%t; %d %u" "e") ; for a let form
    (lisp-interaction-mode "(%n %v)%t; %d %u" "e") ; for a let form
    (t "%n = %v%t; %d %u" "e"))
    "Format descriptions for different major programming modes.
This is an alist with major mode symbols as keys.  If a key is `t', that
entry specifies a default format.  The second item is the format string
to insert for each constant.  In the format, several %-escapes
have special meaning:
%n    the variable name
%v    the value of the constant
%d    the descriptive text of the constant
%u    the unit sytem for which this value is applicable
%t    Text after this is indented to `comment-column'

The third element in the list is the string to use for exponents.
By default, \"e\" is used, but you can change it here,
most likely to \"d\"."
  :group 'constants
  :type 
  '(repeat
    (list (symbol :tag "Major mode")
	  (string :tag "Format")
	  (string :tag "Exponent key"))))

(defcustom constants-indent-code t
  "Non-nil means, indent the newly inserted code."
  :group 'constants
  :type 'boolean)

(defconst constants-defaults
  '(
    "Natural constants"

    ;; Note: electrical charge given here is e.s.u., not e.m.u.
    ("echarge"       "e"      "Elementary charge"      "1.602176462e-19" "4.8032e-10")
    ("clight"        "c"      "Speed of light"         "2.99792458e8"    "2.99792458e10")
    ("hplanck"       "h"      "Planck's constant"      "6.62606876e-34"  "6.62606876e-27")
    ("hplanckbar"    "hbar"   "Planck's constant"      "1.054571596e-34" "1.054571596e-27")
    ("Grav"          "GG"     "Gravitational constant" "6.673e-11"       "6.673e-8")
    ("Nav"           "NA"     "Avogadro's constant"    "6.02214199e23"   "6.02214199e23")
    ("melektron"     "me"     "Electron rest mass"     "9.10938188e-31"  "9.10938188e-28")
    ("mproton"       "mp"     "Proton rest mass"       "1.67262158e-27"  "1.67262158e-24")
    ("mneutron"      "mn"     "Neutron rest mass"      "1.67492716e-27"  "1.67492716e-24")
    ("mmuon"         "mu"     "Muon rest mass"         "1.88353109e-28"  "1.88353109e-25")
    ("atomicmass"    "amu"    "Atomic mass unit"       "1.66053873e-27"  "1.66053873e-24")
    ("Rydberg"       "Ryd"    "Rydberg's constant"     "1.09737315685e7" "1.09737315685e5")
    ("finestructure" "fsc"    "Fine structure const"   "7.297352533e-3"  "7.297352533e-3")
    ("kboltzmann"    "k"      "Boltzmann's constant"   "1.3806503e-23"   "1.3806503e-16")
    ("Rgas"          "R0"     "Molar gas constant"     "8.314472"        "8.314472e7")
    ("Vgas"          "V0"     "Ideal gas volume"       "2.2710981e-2"    "2.2710981e4")
    ("sigthompson"   "sth"    "Thompson crosssection"  "6.6524e-29"      "6.6524e-25")
    ("sigma"         "sig"    "Stefan-Boltzman const"  "5.6703e-8"       "5.6703e-5")
    ("arad"          "a"      "Radiation constant"     "7.5657e-15"      "7.5657e-15")

    "Math constants"

    ("pi"            ""       "Pi"                     "3.1415926535897932385e0" "3.1415926535897932385e0") 
    ("exp1"          ""       "e (base of ln)"         "2.7182818284590452354e0" "2.7182818284590452354e0")

    "Lenth units"

    ("meter"         "m"      "Meter"                  "1.0e0"           "1.0e2")
    ("lightyear"     "lyr"    "Lightyear"              "9.460536207e15"  "9.460536207e17")
    ("parsec"        "pc"     "Parsec"                 "3.085677582e16"  "3.085677582e18")
    ("Angstroem"     "Ang"    "Angstroem"              "1e-10"           "1e-8")
    ("micron"        "mum"    "Micrometer"             "1e-6"            "1e-4")

    "Area units"

    ("barn"          "ba"     "Barn"                   "1e-28"           "1e-24")
    
    "Time units"

    ("second"        "s"      "Seconds"                "1.0e0"           "1.0e0")
    ("minute"        "min"    "Minutes"                "60e0"            "60e0")
    ("hour"          "hr"     "Hours"                  "3600e0"          "3600e0")
    ("day"           "d"      "Days"                   "8.64e4"          "8.64e4")
    ("week"          "wk"     "Weeks"                  "6.048e5"         "6.048e5")
    ("year"          "yr"     "Years"                  "3.15576e7"       "3.15576e7")
    ("Hertz"         "Hz"     "Hertz"                  "1.0e0"           "1.0e0")

    "Mass units"

    ("gram"          "g"      "Grams"                  "1.0e-3"           "1.0e0")

    "Force units"

    ("Newton"        "N"      "Newton (force)"         "1e0"             "1e5")
    ("dyne"          "dyn"    "Dyne (force)"           "1e5"             "1e0")

    "Energy units"

    ("Joule"         "J"      "Joule (energy)"         "1e0"             "1e7")
    ("erg"           ""       "Erg (energy)"           "1e-7"            "1e0")
    ("Calories"      "cal"    "Calories (energy)"      "4.1868"          "4.1868e7")
    ("eVolt"         "eV"     "Electron Volt (energy)" "1.602176462e-19" "1.602176462e-12")
    ("Kayser"        "invcm"  "Energy in cm^-1"        "1.986445e-23"    "1.986445e-16")

    "Pressure units"

    ("Pascal"        "Pa"     "Pascal (pressure)"      "1e0"             "10e0")
    ("bar"           ""       "Bar (pressure)"         "1e5"             "1e6")
    ("atmospheres"   "atm"    "Atmospheres (pressure)" "1.01325e5"       "1.01325e6")
    ("torr"          ""       "Torr (pressure)"        "1.333224e2"      "1.333224e3")
    
    "Temperature units"

    ("Kelvin"        "degK"   "Kelvin"                 "1.0e0"           "1.0e0")
    ("Celsius"       "degC"   "Celsius"                "1.0e0"           "1.0e0")
    ("Fahrenheit"    "degF"   "Fahrenheit"             "0.55555555556e0" "0.55555555556e0")

    "Light units"  ;; FIXME: I am not sure if these are right...

    ("Candela"       "cd"     "Candela"                "1e0"             "1e0")
    ("Stilb"         "sb"     "Stilb"                  "1e4"             "1e0")
    ("Lumen"         "lm"     "Lumen"                  "1e0"             "1e0")
    ("Lux"           "lx"     "Lux"                    "1e0"             "1e-4")
    ("Phot"          "ph"     "Phot"                   "1e4"             "1e0")
    ("Lambert"       "lam"    "Lambert"                "3.18309886184e3" "3.18309886184e-1")

    "Radiation units"

    ("Becquerel"     "Bq"     "Becquerel"              "1.0e0"           "1.0e0")
    ("Curie"         "Ci"     "Curie"                  "3.7e10"          "3.7e10")
    ("Gray"          "Gy"     "Gray"                   "1.0e0"           "1.0e4")
    ("Sievert"       "Sv"     "Sievert"                "1.0e0"           "1.0e4")
    ("Roentgen"      "R"      "Roentgen"               "2.58e-4"         "7.7346e2")
    ("Radrad"        "rd"     "Rad (radiation)"        "1.0e-2"          "1.0e2")
    ("Rem"           "rem"    "Rem"                    "1.0e-2"          "1.0e2")

    "Amount of matter units"
    ("Mol"           "Mol"    "Mol (SI base unit)"     "1.0e0"           "1.0e0")

    "Friction units" 

    ("Poise"         "Poi"    "Poise"                  "1.0e-1"          "1.0e0")
    ("Stokes"        "St"     "Stokes"                 "1.0e-4"          "1.0e0")

    "Electrical units" ;; FIXME; I am not sure if the cgs versions are right.

    ;; Note: units refer to esu, not emu units.... 
    ("Ampere"        "Amp"    "Ampere"                 "1.0e0"           "2.99792458e9")
    ("Coulomb"       "C"      "Coulomb"                "1.0e0"           "2.99792458e9")
    ("Faraday"       "Fdy"    "Faraday"                "9.6485341472e4"  "2.892555240e14")
    ("Volt"          ""       "Volt"                   "1.0e0"           "3.335640952e-3")
    ("Ohm"           ""       "Ohm"                    "1.0e0"           "1.112650056e-12")
    ("Mho"           ""       "Mho"                    "1.0e0"           "8.987551787e11")
    ("Siemens"       ""       "Siemens"                "1.0e0"           "8.987551787e11")
    ("Farad"         ""       "Farad"                  "1.0e0"           "8.987551787e11")
    ("Henry"         ""       "Henry"                  "1.0e0"           "1.112650056e-12")
    ("Tesla"         "T"      "Tesla"                  "1.0e0"           "2.99792458e14")
    ("Gauss"         ""       "Gauss"                  "1.0e-4"          "2.99792458e10")
    ("Weber"         "Wb"     "Weber"                  "1.0e0"           "3.335640952e-3")

    "Angular units"
    
    ("Radian"        "rad"    "Radian"                 "1.0e0"           "1.0e0")
    ("Steradian"     "sr"     "Steradian"              "1.0e0"           "1.0e0")
    ("Degrees"       "deg"    "Degrees"                "1.745329252e-2"  "1.745329252e-2")
    ("Grad"          "grad"   "Grad"                   "7.853981634e-3"  "7.853981634e-3")
    ("Arcminute"     "arcmin" "Arcminutes"             "2.908882087e-4"  "2.908882087e-4")
    ("Arcsecond"     "arcsec" "Arcseconds"             "4.848136812e-6"  "4.848136812e-6")
    
    "Astronomical Units"

    ("Lsun"          "LS"     "Solar Luminosity"       "3.82e26"         "3.82e33")
    ("Msun"          "MS"     "Solar Mass"             "1.989e30"        "1.989e33")
    ("Mearth"        "MEa"    "Earth Mass"             "5.976e24"        "5.976e27")
    ("Mmoon"         "Mmn"    "Moon mass"              "7.35e22"         "7.35e25")
    ("Rsun"          "RS"     "Solar radius"           "6.96e8"          "6.96e10")
    ("Rearth"        "RE"     "Earth radius"           "6.378e6"         "6.378e8")
    ("AstronUnit"    "AU"     "Astronomical unit"      "1.49597870691e11" "1.49597870691e13")
    ("Jansky"        "Jy"     "Jansky"                 "1e-26"           "1e-23")
    ("gEarth"        "ga"     "Earth acceleration"     "9.80665"         "9.80665e2")
    )
  "Built-in constants and units")

(defcustom constants-allow-prefixes t
  "Non-nil means, non-matching names are tried again with the first character
interpreted as unit prefix.  See `constants-prefixes' for a list of allowed
prefiexes."
  :group 'constants
  :type 'boolean)

(defcustom constants-prefixes
  '((?E 1e18  "Exa")
    (?P 1e15  "Peta")
    (?T 1e12  "Tera")
    (?G 1e9   "Giga")
    (?M 1e6   "Mega")
    (?k 1e3   "Kilo")
    (?h 1e2   "Hecto")
    (?D 1e1   "Deka")
    (?d 1e-1  "Deci")
    (?c 1e-2  "Centi")
    (?m 1e-3  "Milli")
    (?u 1e-6  "Micro")
    (?n 1e-9  "Nano")
    (?p 1e-12 "Pico")
    (?f 1e-15 "Femto")
    (?a 1e-18 "Atto"))
  "Allowed prefixes for constants and units"
  :group 'constants
  :type  '(repeat
           (list (character :tag "Prefix char")
                 (number    :tag "Numeric value")
                 (string    :tag "Prefix name"))))

(defvar constants-language-function)

;;;###autoload
(defun constants-insert (&optional arg)
  "Insert one or more natural constant definitions in source code.
Prompts for a constant name and inserts a variable definition and
assignment into the code.  The code produced is different for
different programming languages.  The available constants are defined
in `constants-defaults' and `constants-user-defined'.  Also names
specified in `constants-rename' can be given here.
The first name can be entered with completion.  For speed, you can also enter
a comma-separated list of several names, but then completion will not
work for the additional entries.  The variables will be defined
in the upcase-downcase spelling given, but lookup in the table is
case-insensitive."
  (interactive "p")
  (let* ((all-constants (append constants-user-defined constants-defaults))
	 (atable (append constants-rename all-constants))
         (ctable (constants-make-completion-table constants-rename
                                                  all-constants))
	 (req1 (constants-completing-read "Name1[,name2...]: " ctable))
         (req (if (string= "" req1) constants-default-list req1))
	 (clist (split-string req "[ ,]+"))
	 (fentry (or (and (local-variable-p 'constants-language-function)
                          constants-language-function
                          (fboundp constants-language-function)
                          (funcall constants-language-function))
                     (assq major-mode constants-languages)
                     (assq t constants-languages)))
	 format exp-string
         pmatch factor prefix-name rpl
	 const prefix entry entry1 desc value ins beg linelist line)
    ;; Check for fentry aliasing
    (while (and fentry
                (symbolp (nth 1 fentry)))
      (setq fentry (or (assq fentry constants-languages)
                       (assq t constants-languages))))
    (unless fentry
      (error "No format definition for constants in %s" major-mode))
    ;; extract format specifications
    (setq format (nth 1 fentry) exp-string (nth 2 fentry))
    (while (setq const (pop clist))
      (setq prefix nil factor nil prefix-name "")
      (setq entry (constants-assoc const atable 'follow)
	    name (car entry)
            pmatch (assoc (string-to-char const) constants-prefixes))
      (if (and (not entry)
               constants-allow-prefixes
               pmatch
               (setq entry1 (constants-assoc (substring const 1) atable)))
          (progn
            (setq entry entry1
                  name (car entry)
                  factor (nth 1 pmatch)
                  prefix (string-to-char const)
                  prefix-name (nth 2 pmatch))))
      (if (not entry)
          (error "No such constant: %s" const))
      (unless entry
	(error "No such constant: %s" const))
      (setq name (car entry)
	    desc (nth 2 entry)
	    value (cond ((eq constants-unit-system 'SI) (nth 3 entry))
			((eq constants-unit-system 'cgs) (nth 4 entry))
			(t nil)))
      (if (or (not value) (not (stringp value))
	      (not (string-match "\\S-" value)))
	  (error "No value for constant %s (%s)" const desc))
      (if prefix 
          (progn
            ; We need to hack around accuracy problems here.
            (setq value (format "%.12e"
                                (* factor (string-to-number value))))
            (if (string-match "0+[eE]" value)
                (setq value (replace-match "e" t t value)))))
      (if (and (string-match "\\S-" exp-string)
	       (string-match "e" value))
	  (setq value (replace-match exp-string t t value)))
    
      ;; Insert and indent
      (setq ins format)
      (while (string-match "%n" ins)
	(setq ins (replace-match const t t ins)))
      (while (string-match "%v" ins)
	(setq ins (replace-match value t t ins)))
      (while (string-match "%d" ins)
	(setq ins (replace-match 
                   (if prefix (concat prefix-name "-" desc) desc)
                   t t ins)))
      (while (string-match "%u" ins)
	(setq ins (replace-match
                   (concat "[" (symbol-name constants-unit-system) "]")
                   t t ins)))
      ;; First make sure we start a new line
      (setq beg (point))
      (if (not (string-match "\\S-"
			     (buffer-substring (save-excursion
						 (beginning-of-line 1) (point))
					       (point))))
	  (progn
	    (end-of-line 0)))
      (if constants-indent-code
	  (newline-and-indent)
	(newline))
    
      (setq linelist (split-string ins "\n"))
      (while (setq line (pop linelist))
	(if (string-match "\\(.*\\)%t\\(.*\\)" line)
	    (let ((comment-column 42))
	      (insert (match-string 1 line))
	      (indent-to comment-column)
              (insert (match-string 2 line)))
	  (insert line))
	(if constants-indent-code
	    (newline-and-indent)
	  (newline))))))

(defun constants-assoc (key table &optional follow)
  "Case-insensitive assoc on first and second list element.
When FOLLOW is non-nil, check if the match is a rename cell
and follow it up."
  (catch 'exit
    (let ((key1 (downcase key)) entry)
      (while (setq entry (car table))
	(if (and (consp entry)
                 (or (equal key1 (downcase (car entry)))
                     (and (consp (cdr entry))
                          (equal key1 (downcase (nth 1 entry))))))
            (if (stringp (cdr (car table)))
                (throw 'exit (constants-assoc (cdr (car table)) table))
              (throw 'exit (car table))))
        (setq table (cdr table)))
      nil)))

(defun constants-completing-read (&rest args)
  "Completing read, case insensitive."
  (let ((old-value (default-value 'completion-ignore-case))
        (minibuffer-local-completion-map 
         (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map "?" 'constants-help)
    (define-key minibuffer-local-completion-map [(shift tab)] 'constants-scroll-help)
    (unwind-protect
	(progn
	  (setq-default completion-ignore-case t)
	  (apply 'completing-read args))
      (setq-default completion-ignore-case old-value))))

(defun constants-scroll-help ()
  (interactive)
  (let ((cw (selected-window))
        (hw (get-buffer-window "*Help*")))
    (if hw
        (progn
          (select-window hw)
          (condition-case nil
              (scroll-up)
            (error (goto-char (point-min))))
          (select-window cw)))))

(defun constants-make-completion-table (varnames constants)
  "Make completion table containing all allowed names."
  (let ((all 
         (delq nil
               (append
                (mapcar 'car varnames)
                (mapcar (lambda(x) (if (consp x) (car x)))
                        constants)
                (mapcar (lambda(x) (if (consp x) (nth 1 x)))
                        constants))))
        (seen '(""))
        rtn dc)
    (while all
      (setq dc (downcase (car all)))
      (if (not (member dc seen))
          (setq rtn (cons (list (car all)) rtn)
                seen (cons dc seen)))
      (setq all (cdr all)))
    rtn))

;;;###autoload
(defun constants-help (arg)
  "List all available constants.
The values are for the currently selected unit system."
  (interactive "P")
  (with-output-to-temp-buffer "*Help*"
    (let ((all (append constants-user-defined constants-defaults))
          (nv (if (eq constants-unit-system 'SI) 3 4))
          (us (symbol-name constants-unit-system))
          entry)
      (if constants-user-defined (setq all (cons "User defined entries" all)))
      (princ (format
"            List of constants: Use Shift-<TAB> to scroll
Description                    Short      Long name       Value [%s]
-------------------------------------------------------------------------------
" us))
      (while (setq entry (pop all))
        (if (stringp entry)
            (progn
              (princ "\n")
              (princ (make-string (/ (- 79 (length entry)) 2) ?.))
              (princ entry)
              (princ (make-string (/ (- 79 (length entry)) 2) ?.))
              (princ "\n"))
          (princ (format "%-30s %-10s %-15s %s\n"
                         (nth 2 entry) (nth 1 entry) (nth 0 entry)
                         (nth nv entry))))))
    (let ((all constants-rename) entry)
      (princ "\nRenaming\n--------\n")
      (while (setq entry (pop all))
        (princ (format "%-15s refers to `%s'\n" (car entry) (cdr entry)))))
    (let ((all constants-prefixes) entry)
      (princ "\nUnit Prefixes\n-------------\n")
      (while (setq entry (pop all))
        (princ (format "%c  %-6s  %3.0e\n"
                       (nth 0 entry) (nth 2 entry) (nth 1 entry)))))
    (let* ((all-constants (append constants-user-defined constants-defaults))
           (atable (append constants-rename all-constants))
           (ctable (constants-make-completion-table constants-rename
                                                    all-constants))
           const c1ass c1)
      (princ "
The following ambiguities are resolved by ignoring the unit prefix
------------------------------------------------------------------
")
      (while (setq const (car (pop ctable)))
        (if (and (assoc (string-to-char const) constants-prefixes)
                 (> (length const) 1)
                 (setq c1 (downcase (substring const 1)))
                 (setq c1ass (constants-assoc c1 atable 'follow)))
            (princ (format "%-15s refers to %-15s and not to %s-%s\n"
                           const 
                           (car (constants-assoc const atable))
                           (nth 2 (assoc (string-to-char const) constants-prefixes))
                           (if (or t (string= c1 (downcase (car c1ass))))
                               (car c1ass)
                             (nth 1 c1ass))))))))
  (save-window-excursion
    (select-window (get-buffer-window "*Help*"))
    (goto-char (point-min))))

(provide 'constants)

;;; constants.el ends here
