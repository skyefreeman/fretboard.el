;;; fretboard.el --- Summary: Guitar fretboard visualization
;;; Commentary:
;;; Code:

(require 's nil t)
(require 'dash nil t)

(defvar fretboard-tuning-standard '("E" "A" "D" "G" "B" "E")
  "Standard guitar tuning from low to high.")

(defvar fretboard-tuning-current fretboard-tuning-standard
  "Current tuning in use for fretboard display.")

(defvar fretboard-notes '("A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#")
  "All available notes in western music.")

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

(defun fretboard-get-note-at-position (string fret)
  "Get the note at the given STRING and FRET position."
  (let* ((open-note (nth (- 6 string) fretboard-tuning-current))
         (open-note-index (-elem-index open-note fretboard-notes))
         (note-index (mod (+ open-note-index fret) (length fretboard-notes))))
    (nth note-index fretboard-notes)))

(defun fretboard-render (highlight-notes &optional frets)
  "Render a guitar fretboard with HIGHLIGHT-NOTES marked.
Optional FRETS parameter determines number of frets to display (default 12)."
  (let ((fret-count (or frets 12)))
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
                   (display (if highlighted 
				(if (= (length note) 2)
				    (format " %s " note)
				  (format " %s  " note))
			      " ·  ")))
              (insert display)))
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
         (scale-notes (mapcar (lambda (interval) 
                                (nth (mod (+ root-index interval) 
                                          (length fretboard-notes)) 
                                     fretboard-notes))
                              scale-intervals))
         (buffer-name (format "*Fretboard: %s %s*" root scale-type))
	 (existing-buffer (get-buffer buffer-name))
         (fretboard (fretboard-render scale-notes)))
    
    (if existing-buffer (kill-buffer existing-buffer))
    
    ;; Store current display info for navigation
    (setq fretboard-current-display (list :type 'scale :root root :subtype scale-type))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "Fretboard - %s %s Scale\n\n" root scale-type))
      (insert (format "Notes: %s\n\n" (s-join ", " scale-notes)))
      (insert fretboard)
      (insert "\nNavigate: n=next, p=previous, s=scale, c=chord, t=toggle, j=previous-type, k=next-type")
      (fretboard-mode)
      (switch-to-buffer buffer-name))))

(defun fretboard-display-chord (root chord-type)
  "Display the fretboard highlighting the ROOT note and CHORD-TYPE."
  (interactive 
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
         (chord-notes (mapcar (lambda (interval) 
				(nth (mod (+ root-index interval) 
                                          (length fretboard-notes)) 
                                     fretboard-notes))
                              chord-intervals))
         (buffer-name (format "*Fretboard: %s%s Chord*" root chord-type))
	 (existing-buffer (get-buffer buffer-name))
         (fretboard (fretboard-render chord-notes)))

    (if existing-buffer (kill-buffer existing-buffer))
    
    ;; Store current display info for navigation
    (setq fretboard-current-display (list :type 'chord :root root :subtype chord-type))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "Fretboard - %s %s Chord\n\n" root chord-type))
      (insert (format "Notes: %s\n\n" (s-join ", " chord-notes)))
      (insert fretboard)
      (insert "\nNavigate: n=next, p=previous, s=scale, c=chord, t=toggle, j=previous-type, k=next-type")
      (fretboard-mode)
      (switch-to-buffer buffer-name))))

(defun fretboard-set-tuning (tuning)
  "Set the fretboard TUNING to one of the predefined tunings."
  (interactive
   (list (completing-read "Select tuning: "
                        '(("Standard (E A D G B E)" . standard)
                          ("Drop D (D A D G B E)" . drop-d)
                          ("Open G (D G D G B D)" . open-g)
                          ("DADGAD (D A D G A D)" . dadgad)
                          ("Half step down (Eb Ab Db Gb Bb Eb)" . half-down))
                        nil t)))
  
  (let ((tuning-notes
         (cond
          ((string= tuning "standard") '("E" "A" "D" "G" "B" "E"))
          ((string= tuning "drop-d") '("D" "A" "D" "G" "B" "E"))
          ((string= tuning "open-g") '("D" "G" "D" "G" "B" "D"))
          ((string= tuning "dadgad") '("D" "A" "D" "G" "A" "D"))
          ((string= tuning "half-down") '("Eb" "Ab" "Db" "Gb" "Bb" "Eb"))
          (t fretboard-tuning-standard))))
    
    (setq fretboard-tuning-current tuning-notes)
    (message "Tuning set to: %s" (s-join " " tuning-notes))))

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

(defun fretboard-switch-to-scale ()
  "Switch to scale display for the current root."
  (interactive)
  (when fretboard-current-display
    (let* ((root (plist-get fretboard-current-display :root)))
      (fretboard-display-scale root "major"))))

(defun fretboard-switch-to-chord ()
  "Switch to chord display for the current root."
  (interactive)
  (when fretboard-current-display
    (let* ((root (plist-get fretboard-current-display :root)))
      (fretboard-display-chord root "major"))))

(define-derived-mode fretboard-mode special-mode "Fretboard"
  "Major mode for displaying guitar fretboard visualizations."
  (define-key fretboard-mode-map (kbd "n") 'fretboard-next)
  (define-key fretboard-mode-map (kbd "p") 'fretboard-previous)
  (define-key fretboard-mode-map (kbd "k") 'fretboard-next-type)
  (define-key fretboard-mode-map (kbd "j") 'fretboard-previous-type)
  (define-key fretboard-mode-map (kbd "t") 'fretboard-toggle-display-type)
  (define-key fretboard-mode-map (kbd "s") 'fretboard-switch-to-scale)
  (define-key fretboard-mode-map (kbd "c") 'fretboard-switch-to-chord))

(provide 'fretboard)
;;; fretboard.el ends here
