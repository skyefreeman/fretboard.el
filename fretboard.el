;;; fretboard.el --- Visualize guitar scales and chord shapes on a fretboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Skye Freeman

;; Author: Skye Freeman
;; URL: https://github.com/skyefreeman/fretboard.el
;; Version: 1.0.1
;; Package-Requires: ((emacs "27.1") (s "1.13.0") (dash "2.19.0"))
;; Keywords: music, guitar, tools
(defconst fretboard--version "1.0.1")

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `fretboard` provides an interactive way to visualize guitar scales and chord
;; shapes directly in Emacs.  It displays a text-based representation of a
;; guitar fretboard with highlighted notes for different scales and chords.
;;
;; The package supports various features:
;; - Display scales and chords with highlighted notes on the fretboard
;; - Support for various scale types (major, minor, pentatonic, etc.)
;; - Support for various chord types (major, minor, 7th, etc.)
;; - Multiple tuning options (standard, drop-D, open-G, etc.)
;; - Interactive navigation between notes, scales, and chord types
;;
;; Usage:
;; - M-x fretboard - Display the fretboard, defaulting to the A major scale.
;; - M-x fretboard-display-scale - Display a specific scale on the fretboard
;; - M-x fretboard-display-chord - Display a specific chord on the fretboard
;; - M-x fretboard-set-tuning - Change the guitar tuning
;;
;; Once viewing a fretboard, you can navigate with the following keys:
;; - n/p: Navigate to next/previous note (same scale/chord type)
;; - k/j: Navigate to next/previous scale/chord type (same note)
;; - ,/m: Navigate to next/previous mode (same scale, only for major scale)
;; - d: Toggle between scale and chord display
;; - t: Toggle between different tunings
;; - r: Toggle between note names and relative intervals
;; - s/c: Switch to scale/chord display
;; - q: Close all fretboard buffers

;;; Code:

(require 's)
(require 'dash)

(defvar fretboard-tunings '((:name "standard" :notes ("E" "A" "D" "G" "B" "E"))
                            (:name "half-step-down" :notes ("D#" "G#" "C#" "F#" "A#" "D#"))
                            (:name "drop-d" :notes ("D" "A" "D" "G" "B" "E"))
                            (:name "open-g" :notes ("D" "G" "D" "G" "B" "D"))
                            (:name "dadgad" :notes ("D" "A" "D" "G" "A" "D")))
  "List of guitar tunings.")

(defvar fretboard-tuning-current '("E" "A" "D" "G" "B" "E")
  "Current tuning in use for fretboard display.")

(defvar fretboard-notes-current '()
  "Current scale in use for fretboard display.")

(defvar fretboard-interval-table nil
  "List of intervals.")

(defvar fretboard-display-relative-notes nil
  "Show notes either in notes, or relative to the root.")

(defvar fretboard-notes '("A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#")
  "All available notes in western music.")

(defvar fretboard-modes '("Ionian" "Dorian" "Phrygian" "Lydian" "Mixolydian" "Aeolian" "Locrian")
  "All available modes in western music.")

(defvar fretboard-mode-counter 0)

(defcustom fretboard-fret-count 12
  "Number of frets to display on the fretboard."
  :type 'integer
  :group 'fretboard)

(defface fretboard-display-relative-face
  '((t :foreground "#0096FF"))
  "Face for the root note of a scale or chord.")

(defface fretboard-root-face
  '((t :foreground "#FF0000" :weight bold))
  "Face for the root note of a scale or chord.")

(defvar fretboard-current-display nil
  "Holds information about the current fretboard display for navigation.
Format is a plist with :type, :root, and :subtype keys.")

(defvar fretboard-scale-types
  '("major" "minor" "pentatonic-major" "pentatonic-minor"
    "blues" "harmonic-minor" "melodic-minor")
  "List of available scale types.")

(defvar fretboard-chord-types
  '("major" "minor" "7" "maj7" "m7" "dim" "aug"
    "sus2" "sus4" "add9" "6" "m6" "9" "m9")
  "List of available chord types.")

(defun fretboard-construct-interval-table ()
  "Construct a map of interval names to semitone values."
  (let ((interval-names '("1" "m2" "2" "m3" "3" "4" "m5" "5" "m6" "6" "m7" "7"))
        (interval-table '()))
    (dotimes (interval 12)
      (push (list :name (nth interval interval-names)
                  :value interval)
            interval-table))
    (setq fretboard-interval-table interval-table)))

(defun fretboard-modal-shift-highlighted-notes (lst n)
  "Rotate the list LST by N positions.
This is used to shift the highlighted notes when displaying different modes."
  (let ((rotations (mod n (length lst))))
    (dotimes (_ rotations lst)
      (setq lst (append (cdr lst) (list (car lst)))))))

(defun fretboard-rotate-notes (root-index modal-shift)
  "Create a new list of notes starting at ROOT-INDEX with MODAL-SHIFT applied.
This is used for generating modal scales relative to the root note."
  (let* ((len (length fretboard-notes))
         (index (mod (+ modal-shift (mod root-index len)) len)))
    (append (nthcdr index fretboard-notes)
            (-take index fretboard-notes))))

(defun fretboard-note-to-interval (note rotated-notes)
  "Convert a NOTE to its interval name relative to the first note in ROTATED-NOTES.
Returns the interval name (e.g., '1', 'm3', '5') for the given note."
  (let ((index (-find-index (lambda (n) (string= n note))
                            rotated-notes)))
    (when index
      (plist-get
       (-find (lambda (mapping)
                (= (plist-get mapping :value) index))
              fretboard-interval-table) :name))))


(defun fretboard-get-notes-for-tuning (name)
  "Get the open string notes for the giving tuning NAME."
  (plist-get (-first (lambda (tuning)
                       (string= name (plist-get tuning :name)))
                     fretboard-tunings)
             :notes))

(defun fretboard-get-name-for-tuning-notes (notes)
  "Get the open string NOTES for the giving tuning NAME."
  (plist-get (-first (lambda (tuning)
                       (equal notes (plist-get tuning :notes)))
                     fretboard-tunings)
             :name))

(defun fretboard-tuning-names ()
  "Get the name of all supported tunings."
  (-map (lambda (tuning)
          (plist-get tuning :name))
        fretboard-tunings))

(defun fretboard-get-note-at-position (string fret)
  "Get the note at the given STRING and FRET position."
  (let* ((open-note (nth (- 6 string) fretboard-tuning-current))
         (open-note-index (-elem-index open-note fretboard-notes))
         (note-index (mod (+ open-note-index fret) (length fretboard-notes))))
    (nth note-index fretboard-notes)))

(defun fretboard-render (intervals root-index &optional frets)
  "Render a guitar fretboard with the note INTERVALS and ROOT-INDEX marked.
Optionally, determine the number of FRETS to display."
  (if (equal fretboard-interval-table nil) (fretboard-construct-interval-table))

  (let* ((fret-count (or frets fretboard-fret-count))
         (highlight-notes
          (mapcar (lambda (interval)
                    (nth (mod (+ root-index interval)
                              (length fretboard-notes))
                         fretboard-notes))
                  intervals))
         (modal-shift (nth fretboard-mode-counter intervals))
         (rotated-notes (fretboard-rotate-notes root-index modal-shift))
         (root-note (car rotated-notes)))

    (setq fretboard-notes-current (fretboard-modal-shift-highlighted-notes highlight-notes fretboard-mode-counter))

    (with-temp-buffer
      ;; Header
      (insert "  ")
      (dotimes (fret (1+ fret-count))
        (if (< fret 10)
            (insert (format " %d  " fret))
          (insert (format "%2d  " fret))))
      (insert "\n")

      ;; Fretboard
      (dotimes (string 6)
        (let ((string-num (1+ string)))
          ;; String number
          (insert (format "%d " string-num))

          ;; Each fret
          (dotimes (fret (1+ fret-count))
            (let* ((note (fretboard-get-note-at-position string-num fret))
                   (highlighted (member note highlight-notes))
                   (formatted-note (if (= (length note) 2)
                                       (format " %s " note)
                                     (format " %s  " note)))

                   (relative-interval (fretboard-note-to-interval note rotated-notes))
                   (formatted-relative-interval (if (= (length relative-interval) 2)
                                                    (format " %s " relative-interval)
                                                  (format " %s  " relative-interval)))
                   (is-root (string= note root-note)))

              (cond
               ((not highlighted) (insert " ·  "))
               (is-root
                (insert (propertize formatted-note 'face 'fretboard-root-face)))
               (fretboard-display-relative-notes
                (insert (propertize formatted-relative-interval 'face 'fretboard-display-relative-face)))
               (t (insert formatted-note)))))
          (insert "\n")))
      (buffer-string))))

(defun fretboard-display-scale (root scale-type)
  "Display the fretboard highlighting the ROOT note and SCALE-TYPE."
  (interactive
   (list (completing-read "Root note: " fretboard-notes nil t)
         (completing-read "Scale type: " fretboard-scale-types nil t)))

  (let* ((scale-intervals (cond
                           ((string= scale-type "major") '(0 2 4 5 7 9 11))
                           ((string= scale-type "minor") '(0 2 3 5 7 8 10))
                           ((string= scale-type "pentatonic-major") '(0 2 4 7 9))
                           ((string= scale-type "pentatonic-minor") '(0 3 5 7 10))
                           ((string= scale-type "blues") '(0 3 5 6 7 10))
                           ((string= scale-type "harmonic-minor") '(0 2 3 5 7 8 11))
                           ((string= scale-type "melodic-minor") '(0 2 3 5 7 9 11))
                           (t '(0 2 4 5 7 9 11)))) ; default to major
         (root-index (-elem-index root fretboard-notes))
         (buffer-name (format "*Fretboard: %s %s*" root scale-type))
         (existing-buffer (get-buffer buffer-name))
         (fretboard (fretboard-render scale-intervals root-index)))

    (if existing-buffer (kill-buffer existing-buffer))

    ;; Store current display info for navigation
    (setq fretboard-current-display (list :type 'scale :root root :subtype scale-type))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "Fretboard - %s %s Scale\n" root scale-type))
      (insert (format "Tuning - %s %s\n\n"
                      (fretboard-get-name-for-tuning-notes fretboard-tuning-current)
                      fretboard-tuning-current))
      (insert (format "Mode - %s \n" (nth fretboard-mode-counter fretboard-modes)))
      (insert (format "Notes: %s\n\n" (s-join ", " fretboard-notes-current)))
      (insert fretboard)
      (insert (if (string= scale-type "major")
                  "\nNavigate:\n\nn=next\np=previous\nk=next-type\nj=previous-type\n,=next-mode\nm=previous-mode\ns=scale\nc=chord\nt=tuning\nr=relative\nq=quit"
                "\nNavigate:\n\nn=next\np=previous\nk=next-type\nj=previous-type\ns=scale\nc=chord\nt=tuning\nr=relative\nq=quit"))
      (fretboard-mode)
      (switch-to-buffer buffer-name))))

(defun fretboard-display-chord (root chord-type)
  "Display the fretboard highlighting the ROOT note and CHORD-TYPE."
  (interactive
   (setq fretboard-mode-counter 0)
   (list (completing-read "Root note: " fretboard-notes nil t)
         (completing-read "Chord type: " fretboard-chord-types nil t)))

  (let* ((chord-intervals (cond
                           ((string= chord-type "major") '(0 4 7))
                           ((string= chord-type "minor") '(0 3 7))
                           ((string= chord-type "7") '(0 4 7 10))
                           ((string= chord-type "maj7") '(0 4 7 11))
                           ((string= chord-type "m7") '(0 3 7 10))
                           ((string= chord-type "dim") '(0 3 6))
                           ((string= chord-type "aug") '(0 4 8))
                           ((string= chord-type "sus2") '(0 2 7))
                           ((string= chord-type "sus4") '(0 5 7))
                           ((string= chord-type "add9") '(0 4 7 14))
                           ((string= chord-type "6") '(0 4 7 9))
                           ((string= chord-type "m6") '(0 3 7 9))
                           ((string= chord-type "9") '(0 4 7 10 14))
                           ((string= chord-type "m9") '(0 3 7 10 14))
                           (t '(0 4 7)))) ; default to major
         (root-index (-elem-index root fretboard-notes))
         (buffer-name (format "*Fretboard: %s%s Chord*" root chord-type))
         (existing-buffer (get-buffer buffer-name))
         (fretboard (fretboard-render chord-intervals root-index)))

    (if existing-buffer (kill-buffer existing-buffer))

    ;; Store current display info for navigation
    (setq fretboard-current-display (list :type 'chord :root root :subtype chord-type))

    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "Fretboard - %s %s Chord\n" root chord-type))
      (insert (format "Tuning - %s %s\n\n"
                      (fretboard-get-name-for-tuning-notes fretboard-tuning-current)
                      fretboard-tuning-current))
      (insert (format "Notes: %s\n\n" (s-join ", " fretboard-notes-current)))
      (insert fretboard)
      (insert "\nNavigate:\n\nn=next\np=previous\nk=next-type\nj=previous-type\ns=scale\nc=chord\nt=tuning\nr=relative\nq=quit")
      (fretboard-mode)
      (switch-to-buffer buffer-name))))

(defun fretboard ()
  "Display the fretboard using the current values of FRETBOARD-CURRENT-DISPLAY.
Defaults to the notes of the A major scale."
  (interactive)
  (let ((type (or (plist-get fretboard-current-display :type) 'scale))
        (root (or (plist-get fretboard-current-display :root) "A"))
        (subtype (or (plist-get fretboard-current-display :subtype) "major")))
    (cond
     ((eq type 'scale)
      (fretboard-display-scale root subtype))
     ((eq type 'chord)
      (fretboard-display-chord root subtype)))))

(defun fretboard-toggle-relative-notes ()
  "Toggle between displaying note names and intervals relative to the root.
When in relative mode, notes are shown as intervals instead of note names."
  (interactive)
  (setq fretboard-display-relative-notes (not fretboard-display-relative-notes))
  (fretboard-refresh-display))

(defun fretboard-set-tuning (tuning)
  "Set the fretboard TUNING to one of the predefined tunings."
  (interactive
   (list (completing-read "Select tuning: "
                          (fretboard-tuning-names)
                          nil t)))
  (let ((notes (fretboard-get-notes-for-tuning tuning)))
    (setq fretboard-tuning-current notes)
    (message "Tuning set to: %s" (s-join " " notes))))

(defun fretboard-next ()
  "Navigate to the next option based on current display."
  (interactive)
  (when fretboard-current-display
    (let* ((type (plist-get fretboard-current-display :type))
           (root (plist-get fretboard-current-display :root))
           (subtype (plist-get fretboard-current-display :subtype))
           (root-idx (-elem-index root fretboard-notes))
           (next-root-idx (mod (1+ root-idx) (length fretboard-notes)))
           (next-root (nth next-root-idx fretboard-notes)))
      (cond
       ((eq type 'scale)
        (fretboard-display-scale next-root subtype))
       ((eq type 'chord)
        (fretboard-display-chord next-root subtype))))))

(defun fretboard-previous ()
  "Navigate to the previous option based on current display."
  (interactive)
  (when fretboard-current-display
    (let* ((type (plist-get fretboard-current-display :type))
           (root (plist-get fretboard-current-display :root))
           (subtype (plist-get fretboard-current-display :subtype))
           (root-idx (-elem-index root fretboard-notes))
           (prev-root-idx (mod (1- root-idx) (length fretboard-notes)))
           (prev-root (nth prev-root-idx fretboard-notes)))
      (cond
       ((eq type 'scale)
        (fretboard-display-scale prev-root subtype))
       ((eq type 'chord)
        (fretboard-display-chord prev-root subtype))))))

(defun fretboard-next-type ()
  "Cycle through the types (scales or chords) for the current root."
  (interactive)
  (setq fretboard-mode-counter 0)
  (when fretboard-current-display
    (let* ((type (plist-get fretboard-current-display :type))
           (root (plist-get fretboard-current-display :root))
           (subtype (plist-get fretboard-current-display :subtype))
           (types-list (if (eq type 'scale)
                           fretboard-scale-types
                         fretboard-chord-types))
           (type-idx (-elem-index subtype types-list))
           (next-type-idx (mod (1+ type-idx) (length types-list)))
           (next-type (nth next-type-idx types-list)))
      (cond
       ((eq type 'scale)
        (fretboard-display-scale root next-type))
       ((eq type 'chord)
        (fretboard-display-chord root next-type))))))

(defun fretboard-previous-type ()
  "Cycle through the types (scales or chords) for the current root."
  (interactive)
  (setq fretboard-mode-counter 0)
  (when fretboard-current-display
    (let* ((type (plist-get fretboard-current-display :type))
           (root (plist-get fretboard-current-display :root))
           (subtype (plist-get fretboard-current-display :subtype))
           (types-list (if (eq type 'scale)
                           fretboard-scale-types
                         fretboard-chord-types))
           (type-idx (-elem-index subtype types-list))
           (prev-type-idx (mod (1- type-idx) (length types-list)))
           (prev-type (nth prev-type-idx types-list)))
      (cond
       ((eq type 'scale)
        (fretboard-display-scale root prev-type))
       ((eq type 'chord)
        (fretboard-display-chord root prev-type))))))

(defun fretboard-next-mode ()
  "Cycle to the next mode of the current scale.
Only applies to major scales, cycling through Ionian, Dorian, Phrygian, etc.
For other scale types, this resets to the default mode (mode counter to 0)."
  (interactive)
  (let* ((type (plist-get fretboard-current-display :type))
         (subtype (plist-get fretboard-current-display :subtype)))
    (cond
     ((eq type 'scale)
      (if (string= subtype "major")
          (progn
            (setq fretboard-mode-counter (mod (1+ fretboard-mode-counter) (length fretboard-modes))))
        (setq fretboard-mode-counter 0))
      (fretboard-refresh-display)))))

(defun fretboard-previous-mode ()
  "Cycle to the previous mode of the current scale.
Only applies to major scales, cycling through Ionian, Dorian, Phrygian, etc.
For other scale types, this resets to the default mode (mode counter to 0)."
  (interactive)
  (let* ((type (plist-get fretboard-current-display :type))
         (subtype (plist-get fretboard-current-display :subtype)))
    (cond
     ((eq type 'scale)
      (if (string= subtype "major")
          (progn
            (setq fretboard-mode-counter (mod (1- fretboard-mode-counter) (length fretboard-modes))))
        (setq fretboard-mode-counter 0))
      (fretboard-refresh-display)))))

(defun fretboard-refresh-display ()
  "Relayout the current fretboard."
  (interactive)
  (when fretboard-current-display
    (let* ((type (plist-get fretboard-current-display :type))
           (root (plist-get fretboard-current-display :root))
           (subtype (or (plist-get fretboard-current-display :subtype) "major")))
      (cond
       ((eq type 'scale)
        (fretboard-display-scale root subtype))
       ((eq type 'chord)
        (fretboard-display-chord root subtype))))))

(defun fretboard-toggle-display-type ()
  "Toggle between scale and chord display."
  (interactive)
  (when fretboard-current-display
    (let* ((type (plist-get fretboard-current-display :type))
           (root (plist-get fretboard-current-display :root)))
      (cond
       ((eq type 'scale)
        (fretboard-display-chord root "major"))
       ((eq type 'chord)
        (fretboard-display-scale root "major"))))))

(defun fretboard-toggle-tuning-type ()
  "Toggle display between fretboard tunings."
  (interactive)
  (when fretboard-tuning-current
    (let* ((note-list (-map (lambda (tuning)
                              (plist-get tuning :notes))
                            fretboard-tunings))
           (index (-elem-index fretboard-tuning-current note-list))
           (next-index (if (< index (- (length note-list) 1))
                           (+ index 1)
                         0))
           (next-tuning (nth next-index fretboard-tunings)))
      (fretboard-set-tuning (plist-get next-tuning :name))
      (fretboard-refresh-display))))

(defun fretboard-switch-to-scale ()
  "Switch to scale display for the current root."
  (interactive)
  (when fretboard-current-display
    (let* ((root (plist-get fretboard-current-display :root)))
      (fretboard-display-scale root "major"))))

(defun fretboard-switch-to-chord ()
  "Switch to chord display for the current root."
  (interactive)
  (setq fretboard-mode-counter 0)
  (when fretboard-current-display
    (let* ((root (plist-get fretboard-current-display :root)))
      (fretboard-display-chord root "major"))))

(defun fretboard-quit-all ()
  "Close all fretboard buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match "^\\*Fretboard:" (buffer-name buffer))
      (kill-buffer buffer))))

(define-derived-mode fretboard-mode special-mode "Fretboard"
  "Major mode for displaying guitar fretboard visualizations."
  ;; Hide cursor
  (setq cursor-type nil)
  ;; Set up keybindings
  (define-key fretboard-mode-map (kbd "n") 'fretboard-next)
  (define-key fretboard-mode-map (kbd "p") 'fretboard-previous)
  (define-key fretboard-mode-map (kbd "k") 'fretboard-next-type)
  (define-key fretboard-mode-map (kbd "j") 'fretboard-previous-type)
  (define-key fretboard-mode-map (kbd ",") 'fretboard-next-mode)
  (define-key fretboard-mode-map (kbd "m") 'fretboard-previous-mode)
  (define-key fretboard-mode-map (kbd "d") 'fretboard-toggle-display-type)
  (define-key fretboard-mode-map (kbd "t") 'fretboard-toggle-tuning-type)
  (define-key fretboard-mode-map (kbd "r") 'fretboard-toggle-relative-notes)
  (define-key fretboard-mode-map (kbd "s") 'fretboard-switch-to-scale)
  (define-key fretboard-mode-map (kbd "c") 'fretboard-switch-to-chord)
  (define-key fretboard-mode-map (kbd "q") 'fretboard-quit-all))

(provide 'fretboard)
;;; fretboard.el ends here
