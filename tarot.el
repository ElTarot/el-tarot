;;; tarot.el -- draw a random tarot card

;; Copyright (C) 2007, 2011 Joseph Corneli <holtzermann17@...>
;; Copyright (C) 2011 Jokim Verona <joakim@verona.se>
;; Notice: Copyright transfered to public domain wherever applicable.

;; Time-stamp: <2011-02-07 01:03:35 joe>

;;; Commentary:

;; Latest version now includes a simple four-card spread
;; and "alt-text" from Alliete and Crowley (where
;; applicable).

;;; Code:

(defvar tarot-decks '( ("Tarot" [["Fool"               "La Follie ou l'Alchemiste"]
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

                      ("major arcana"  [["Fool"               "La Follie ou l'Alchemiste" "http://en.wikipedia.org/wiki/The_Fool_(Tarot_card)"]
                                        ["Magician"           "Le Magicien ou le Bateleur" "http://en.wikipedia.org/wiki/The_Magician_(Tarot_card)"]
                                        ["High Priestess"     "Repos" "http://en.wikipedia.org/wiki/The_High_Priestess_(Tarot_card)"]
                                        ["Empress"            "Les Astres" "http://en.wikipedia.org/wiki/The_Empress_(Tarot_card)"]
                                        ["Emperor"            "Les Ouiseaux et les Poissons" "http://en.wikipedia.org/wiki/The_Emperor_(Tarot_card)"]
                                        ["Hierophant"         "Le Grand pretre" "http://en.wikipedia.org/wiki/The_Hierophant_(Tarot_card)"]
                                        ["Lovers"             "Le Chaos" "http://en.wikipedia.org/wiki/The_Lovers_(Tarot_card)"]
                                        ["Chariot"            "Le Despote africain" "http://en.wikipedia.org/wiki/The_Chariot_(Tarot_card)"]
                                        ["Strength"           "La Force" "http://en.wikipedia.org/wiki/The_Chariot_(Tarot_card)"]
                                        ["Hermit"             "Le Capucin" "http://en.wikipedia.org/wiki/The_Hermit_(Tarot_card)"]
                                        ["Wheel of Fortune"   "La Roue de Fortune" "http://en.wikipedia.org/wiki/Wheel_of_Fortune_(Tarot_card)"]
                                        ["Justice"            "La Justice" "http://en.wikipedia.org/wiki/Strength_(Tarot_card)"]
                                        ["Hanged Man"         "La Prudence" "http://en.wikipedia.org/wiki/The_Hanged_Man_(Tarot_card)"]
                                        ["Death"              "La Mort" "http://en.wikipedia.org/wiki/The_Hanged_Man_(Tarot_card)"]
                                        ["Temperance"         "La Temperance" "http://en.wikipedia.org/wiki/Temperance_(Tarot_card)"]
                                        ["Devil"              "Le Diable" "http://en.wikipedia.org/wiki/The_Devil_(Tarot_card)"]
                                        ["Tower"              "Le Temple Foudroye" "http://en.wikipedia.org/wiki/The_Tower_(Tarot_card)"]
                                        ["Star"               "La Ciel" "http://en.wikipedia.org/wiki/The_Star_(Tarot_card)"]
                                        ["Moon"               "Les Plantes" "http://en.wikipedia.org/wiki/The_Moon_(Tarot_card)"]
                                        ["Sun"                "La Lumiere" "http://en.wikipedia.org/wiki/The_Sun_(Tarot_card)"]
                                        ["Judgement"          "Le Jugement Dernier" "http://en.wikipedia.org/wiki/Judgment_(Tarot_card)"]
                                        ["World"              "L'homme et les Quadrupedes" "http://en.wikipedia.org/wiki/The_World_(Tarot_card)"]
                                        ])

                      ;;separate just because the deck is WIP
                      ("javetarot"    [["Fool"               "La Follie ou l'Alchemiste" "http://en.wikipedia.org/wiki/The_Fool_(Tarot_card)"]
                                        ["Magician"           "Le Magicien ou le Bateleur" "http://en.wikipedia.org/wiki/The_Magician_(Tarot_card)"]
                                        ["High Priestess"     "Repos" "http://en.wikipedia.org/wiki/The_High_Priestess_(Tarot_card)"]
                                        ["Empress"            "Les Astres" "http://en.wikipedia.org/wiki/The_Empress_(Tarot_card)"]
                                        ["Emperor"            "Les Ouiseaux et les Poissons" "http://en.wikipedia.org/wiki/The_Emperor_(Tarot_card)"]
                                        ["Hierophant"         "Le Grand pretre" "http://en.wikipedia.org/wiki/The_Hierophant_(Tarot_card)"]
                                        ["Lovers"             "Le Chaos" "http://en.wikipedia.org/wiki/The_Lovers_(Tarot_card)"]
                                        ["Chariot"            "Le Despote africain" "http://en.wikipedia.org/wiki/The_Chariot_(Tarot_card)"]
                                        ["Strength"           "La Force" "http://en.wikipedia.org/wiki/The_Chariot_(Tarot_card)"]
                                        ["Hermit"             "Le Capucin" "http://en.wikipedia.org/wiki/The_Hermit_(Tarot_card)"]
                                        ["Wheel of Fortune"   "La Roue de Fortune" "http://en.wikipedia.org/wiki/Wheel_of_Fortune_(Tarot_card)"]
                                        ["Justice"            "La Justice" "http://en.wikipedia.org/wiki/Strength_(Tarot_card)"]
                                        ["Hanged Man"         "La Prudence" "http://en.wikipedia.org/wiki/The_Hanged_Man_(Tarot_card)"]
                                        ["Death"              "La Mort" "http://en.wikipedia.org/wiki/The_Hanged_Man_(Tarot_card)"]
                                        ["Temperance"         "La Temperance" "http://en.wikipedia.org/wiki/Temperance_(Tarot_card)"]
                                        ["Devil"              "Le Diable" "http://en.wikipedia.org/wiki/The_Devil_(Tarot_card)"]
                                        ["Tower"              "Le Temple Foudroye" "http://en.wikipedia.org/wiki/The_Tower_(Tarot_card)"]
                                        ["Star"               "La Ciel" "http://en.wikipedia.org/wiki/The_Star_(Tarot_card)"]
                                        ["Moon"               "Les Plantes" "http://en.wikipedia.org/wiki/The_Moon_(Tarot_card)"]
                                        ["Sun"                "La Lumiere" "http://en.wikipedia.org/wiki/The_Sun_(Tarot_card)"]
                                        ["Judgement"          "Le Jugement Dernier" "http://en.wikipedia.org/wiki/Judgment_(Tarot_card)"]
                                        ["World"              "L'homme et les Quadrupedes" "http://en.wikipedia.org/wiki/The_World_(Tarot_card)"]
                                        ])
                      ))

(defun random-aref (array)
  (let ((index (random (length array))))
    (values (aref array index) index)))

(defun read-tarot-card (card)
  (let ((len (length card)))
    (if (>= len 2)
        (concat (aref card 0) " (" (aref card 1) ") "
                (if (>= len 3) (org-make-link-string (aref card 2)  "wikipedia") ) )
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

(defun tarot-choose-deck (deck)
  (interactive (list  (completing-read "deck:" (mapcar 'car tarot-decks) )))
  ;;todo
  (setq tarot-deck (cadr (assoc deck tarot-decks))))

(tarot-choose-deck "Tarot") ;;default deck


(defvar tarot-spreads
  '(( "simple"
      ( "Past"
        "Present"
        "Future"
        "Outcome"))

    ( "celtic"
      ("Present"
       "Immediate Challenge"
       "Distant Past"
       "Recent Past"
       "Best Outcome"
       "Immediate Future"
       "Factors"
       "External Influences"
       "Hopes and Fears"
       "Final Outcome"
       )))
  )
(defun tarot-formatted-spread-texts ( spread-name)
  (let* ( (maxlen (apply 'max (mapcar 'length (cadr (assoc spread-name tarot-spreads))))))
    (mapcar (lambda (x) (format (concat  "%-" (number-to-string  maxlen) "s :") x)) (cadr (assoc spread-name tarot-spreads))))
  )


(defun tarot-spread (spread-name)
  (interactive (list  (completing-read "spread:" (mapcar 'car tarot-spreads) ))) ;;TODO completion TODO handle deck selection
  (random t);;shuffling the deck is important TODO
  (pop-to-buffer (get-buffer-create "*Spread*"))
  (org-mode)
  (erase-buffer)
  (let* (
         (import (tarot-formatted-spread-texts spread-name))
         (stack (tarot-cards (length import))) 
         )
    (dolist (card stack)
      (insert (car import) " ")
      (setq import (cdr import))
      (insert (read-tarot-card card) )
      
      ;;  (insert (format  " [[file:tarot_of_marseilles/%s.jpg]]" (aref card 0) ))
        (insert (format  " [[file:jave_tarot/%s.svg]]" (aref card 0) ))
      (insert "\n")
      ))
  )

;;;;


(defun tarot-get-images ()
  (interactive)
  (loop for cardno from 1 to 21 by 1 do
        (with-current-buffer (skip-http-headers
                              (url-retrieve-synchronously  (format "http://upload.wikimedia.org/wikipedia/commons/1/1d/Jean_Dodal_Tarot_trump_%s.jpg" cardno))
                              )
          (write-file (format "/tmp/t%s.jpg" cardno))))
  )

(defun skip-http-headers (buffer)
  "Remove HTTP headers from BUFFER, and return it.
Assumes headers are indeed present!"
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    buffer))
;;; tarot.el ends here
