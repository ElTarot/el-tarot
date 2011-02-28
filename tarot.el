;;; tarot.el -- draw a random tarot card

;; Copyright (C) 2007, 2011 Joseph Corneli <holtzermann17@...>
;; Notice: Copyright transfered to public domain wherever applicable.

;; Time-stamp: <2011-02-07 01:03:35 joe>

;;; Commentary:

;; Latest version now includes a simple four-card spread
;; and "alt-text" from Alliete and Crowley (where
;; applicable).

;;; Code:

(defvar tarot-deck [["Fool"               "La Follie ou l'Alchemiste"]
                    ["Magician"           "Le Magicien ou le Bateleur"]
                    ["High Priestess"     "Repos"]
                    ["Empress"            "Les Astres"]
                    ["Emperor"            "Les Ouiseaux et les Poissons"]
                    ["Hierophant"         "Le Grand pretre"]
                    ["Lovers"             "Le Chaos"]
                    ["Chariot"            "Le Despote africain"]
                    ["Strength"           "La Force"]
                    ["Hermit"             "Le Capucin"]
                    ["Wheel of Fortune"   "La Roue de Fortune"]
                    ["Justice"            "La Justice"]
                    ["Hanged Man"         "La Prudence"]
                    ["Death"              "La Mort"]
                    ["Temperance"         "La Temperance"]
                    ["Devil"              "Le Diable"]
                    ["Tower"              "Le Temple Foudroye"]
                    ["Star"               "La Ciel"]
                    ["Moon"               "Les Plantes"]
                    ["Sun"                "La Lumiere"]
                    ["Judgement"          "Le Jugement Dernier"]
                    ["World"              "L'homme et les Quadrupedes"]
                    ["Ace of Wands"       "The Root of the Powers of Fire"]
                    ["2 of Wands"         "Dominion"]
                    ["3 of Wands"         "Virtue"]
                    ["4 of Wands"         "Completion"]
                    ["5 of Wands"         "Strife"]
                    ["6 of Wands"         "Victory"]
                    ["7 of Wands"         "Valour"]
                    ["8 of Wands"         "Swiftness"]
                    ["9 of Wands"         "Strength"]
                    ["10 of Wands"        "Oppression"]
                    ["Page of Wands"]
                    ["Knight of Wands"]
                    ["Queen of Wands"]
                    ["King of Wands"]
                    ["Ace of Cups"        "The Root of the Powers of Water"]
                    ["2 of Cups"          "Love"]
                    ["3 of Cups"          "Abundance"]
                    ["4 of Cups"          "Luxury"]
                    ["5 of Cups"          "Disappointment"]
                    ["6 of Cups"          "Pleasure"]
                    ["7 of Cups"          "Debauch"]
                    ["8 of Cups"          "Indolence"]
                    ["9 of Cups"          "Happiness"]
                    ["10 of Cups"         "Satiety"]
                    ["Page of Cups"]
                    ["Knight of Cups"]
                    ["Queen of Cups"]
                    ["King of Cups"]
                    ["Ace of Swords"      "The Root of the Powers of Air"]
                    ["2 of Swords"        "Peace"]
                    ["3 of Swords"        "Sorrow"]
                    ["4 of Swords"        "Truce"]
                    ["5 of Swords"        "Defeat"]
                    ["6 of Swords"        "Science"]
                    ["7 of Swords"        "Futility"]
                    ["8 of Swords"        "Interference"]
                    ["9 of Swords"        "Cruelty"]
                    ["10 of Swords"       "Ruin"]
                    ["Page of Swords"]
                    ["Knight of Swords"]
                    ["Queen of Swords"]
                    ["King of Swords"]
                    ["Ace of Pentacles"  "The Root of the Powers of Earth"]
                    ["2 of Pentacles"    "Change"]
                    ["3 of Pentacles"    "Work"]
                    ["4 of Pentacles"    "Power"]
                    ["5 of Pentacles"    "Worry"]
                    ["6 of Pentacles"    "Success"]
                    ["7 of Pentacles"    "Failure"]
                    ["8 of Pentacles"    "Prudence"]
                    ["9 of Pentacles"    "Gain"]
                    ["10 of Pentacles"   "Wealth"]
                    ["Page of Pentacles"]
                    ["Knight of Pentacles"]
                    ["Queen of Pentacles"]
                    ["King of Pentacles"]])

(defun random-aref (array)
  (let ((index (random (length array))))
    (values (aref array index) index)))

(defun read-tarot-card (card)
  (let ((len (length card)))
      (if (= len 2)
          (concat (aref card 0) " (" (aref card 1) ")")
        (aref card 0))))

(defun tarot-card ()
  (multiple-value-bind
      (card index) (random-aref tarot-deck)
    (read-tarot-card card)))

(defun tarot-draw-card ()
  (interactive)
  (message "%s" (tarot-card)))

(defun tarot-cards (n)
  (let ((pack tarot-deck)
        stack)
    (dotimes (i n)
        (multiple-value-bind
            (card index) (random-aref pack)
          (setq stack (cons card stack))
          (setq pack (delete (aref pack index) pack))))
    stack))

(defun tarot-spread ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*Spread*"))
  (erase-buffer)
  (let ((stack (tarot-cards 4))
        (import '("Past    :"
                  "Present :"
                  "Future  :"
                  "Outcome :")))
    (dolist (card stack)
      (insert (car import) " ")
      (setq import (cdr import))
      (insert (read-tarot-card card) "\n"))))

;;; tarot.el ends here
