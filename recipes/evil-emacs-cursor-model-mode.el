;;; evil-emacs-cursor-model-mode.el --- Emacs' cursor model in evil-mode -*- lexical-binding: t; -*-

;; ============================================================================
;;; License:
;; ============================================================================
;; Creative Commons Attribution-ShareAlike 4.0 International License
;; [[https://creativecommons.org/licenses/by-sa/4.0/]]

;; ----------------------------------------------------------------------------
;; Thanks.
;; A special thanks to Toby Cubitt who coded the motions in the cursor model.
;; Peter Friis Jensen made it a mode and swapped some keybindings.

;; Author: Toby Cubitt
;; Maintainer: Peter Friis Jensen <maxfriis@gmail.com>
;; URL: https://github.com/maxfriis/evil-emacs-cursor-model-mode
;; Created: 2025-11-15
;; Version: 0.1.3
;; Keywords: convenience, files
;; Package-Requires: ((emacs "29.1") (evil "1.15.0"))

;; ============================================================================
;;; Commentary:
;; ============================================================================
;; Emacs' cursor between characters model for cursor positioning in
;; `evil-mode' instead of Vim's normal-state cursor on top of character model.
;; ============================================================================
;;; TODO:
;; ============================================================================
;; Does erlier release versions of evil work?  Probably.
;; 1.15.0 is the current but fairly old release version of evil.
;;
;; Work on `evil-visual-block' which still use `evil-mode's cursor model.
;;
;; Fix `user-error' in `evil-emacs-cursor-model-repeat-find-char'.
;; Don't know how to make the signal come from the function when inside `let'.
;; For now the function `if' reports the `user-error'.

;; ============================================================================
;;; Code:
;; ============================================================================
(require 'evil)

;; ----------------------------------------------------------------------------
;; Remember init defaults.
(defvar evil-emacs-cursor-model-move-cursor-back-init evil-move-cursor-back
  "For toggling the variable with `evil-emacs-cursor-model-mode'.")
(defvar evil-emacs-cursor-model-move-beyond-eol-init evil-move-beyond-eol
  "For toggling the variable with `evil-emacs-cursor-model-mode'.")
(defvar evil-emacs-cursor-model-highlight-closing-paren-at-point-states-init evil-highlight-closing-paren-at-point-states
  "For toggling the variable with `evil-emacs-cursor-model-mode'.")

;; ============================================================================
;;; The minor mode
;; ============================================================================
(define-minor-mode evil-emacs-cursor-model-mode
  "Mode for using Emacs' cursor model in `evil-mode's normal state.
\nThe mode swap \"a\"/\"A\", \"o\"/\"O\" and \"p\"/\"P\" compared to Vim's normal state keys.
The idea is to avoid the <shift> layer when dealing with the current line.
Layers can then be replaced with a motion with equivalent efficiency.
\nEmbrace the mindset of Emacs' cursor model and motions among line nuggets.
Maybe fewer layers are better for your Emacs pinky?"
  :lighter nil
  :global t
  :require 'evil-emacs-cursor-model-mode
  :group 'evil
  (cond
   (evil-emacs-cursor-model-mode
    (unless evil-mode
      (evil-mode 1))
    ;; ----------------------------------------------------------------------------
    ;; Cursor related `evil-mode' settings.
    (setq
     evil-move-cursor-back nil
     evil-move-beyond-eol t
     evil-highlight-closing-paren-at-point-states nil)
    ;; ----------------------------------------------------------------------------
    ;; Rebinding relevant `evil-org-mode' commands.
    (evil-define-minor-mode-key 'normal 'evil-org-mode
      "a"  #'evil-org-append-line
      "A"  nil
      "o"  #'evil-org-open-above
      "O"  #'evil-org-open-below))
   (t ; else
    ;; ----------------------------------------------------------------------------
    ;; Back to `evil-mode' defaults when `evil-emacs-cursor-model-mode' is disabled.
    (setq
     evil-move-cursor-back evil-emacs-cursor-model-move-cursor-back-init
     evil-move-beyond-eol evil-emacs-cursor-model-move-beyond-eol-init
     evil-highlight-closing-paren-at-point-states evil-emacs-cursor-model-highlight-closing-paren-at-point-states-init)
    ;; ----------------------------------------------------------------------------
    ;; `evil-org-mode' defaults.
    (evil-define-minor-mode-key 'normal 'evil-org-mode
      "a"  nil
      "A"  #'evil-org-append-line
      "o"  #'evil-org-open-below
      "O"  #'evil-org-open-above))))

;; ============================================================================
;;; Remappings that implement Emacs' cursor model
;; ============================================================================
(defvar evil-emacs-cursor-model-mode-map (make-sparse-keymap)
  "Keymap for `evil-emacs-cursor-model-mode'.")
;; ----------------------------------------------------------------------------
;; Motions.
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-find-char-to>"
            #'evil-emacs-cursor-model-find-before-char)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-find-char>"
            #'evil-emacs-cursor-model-find-after-char)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-repeat-find-char>"
            #'evil-emacs-cursor-model-repeat-find-char)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-repeat-find-char-reverse>"
            #'evil-emacs-cursor-model-repeat-find-char-reverse)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-forward-word-end>"
            #'evil-emacs-cursor-model-forward-after-word-end)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-forward-WORD-end>"
            #'evil-emacs-cursor-model-forward-after-WORD-end)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-backward-word-end>"
            #'evil-emacs-cursor-model-backward-after-word-end)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-backward-WORD-end>"
            #'evil-emacs-cursor-model-backward-after-WORD-end)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-jump-item>"
            #'evil-emacs-cursor-model-jump-after-item)
;; ----------------------------------------------------------------------------
;; Commands.
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-append>"
            #'evil-append-line)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-append-line>"
            #'evil-append)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-open-above>"
            #'evil-open-below)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-open-below>"
            #'evil-open-above)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-paste-before>"
            #'evil-paste-after)
(keymap-set evil-emacs-cursor-model-mode-map
            "<remap> <evil-paste-after>"
            #'evil-paste-before)
;; ----------------------------------------------------------------------------
(add-to-list 'minor-mode-map-alist
             (cons 'evil-emacs-cursor-model-mode
                   evil-emacs-cursor-model-mode-map) t)

;; ============================================================================
;;; Evil commands implementing Emacs' cursor model
;; ============================================================================
(evil-define-motion evil-emacs-cursor-model-find-after-char (count char)
  "Move point immediately after the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (unless count (setq count 1))
  (if (and (= char (char-after))
           (> count 0))
      (evil-find-char (1- count) char)
    (evil-find-char count char))
  (when (> count 0) (forward-char))
  (setq evil-last-find (list #'evil-find-char char (> count 0))))

(evil-define-motion evil-emacs-cursor-model-find-before-char (count char)
  "Move point immediately before the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (unless count (setq count 1))
  (if (and (= char (char-after))
           (> count 0))
      (evil-find-char (1- count) char)
    (evil-find-char count char))
  (setq evil-last-find (list #'evil-find-char-to char (> count 0))))

(evil-define-motion evil-emacs-cursor-model-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (interactive "<c>")
  (unless count (setq count 1))
  (unless (nth 2 evil-last-find) (setq count (- count))) ; Backwards search.
  (let ((find (eq (car evil-last-find) #'evil-find-char))
        (char (nth 1 evil-last-find)))
    (unless char (user-error "No previous search"))
    (unless find
      (cond ; Vim does this when find is nil.
       ((and (= count  1) (= char (char-after)))  (setq count (1+ count)))
       ((and (= count -1) (= char (char-before))) (setq count (1- count)))))
    (if (search-forward
         (char-to-string char)
         (cond (evil-cross-lines nil)
               ((and evil-respect-visual-line-mode
                     visual-line-mode)
                (save-excursion
                  (if (> count 0) (end-of-visual-line) (beginning-of-visual-line))
                  (point)))
               ((> count 0) (line-end-position))
               (t (line-beginning-position)))
         t count)
        (unless (or find (= count 0))
          (if (> count 0) (backward-char) (forward-char)))
      (user-error "Can't find `%c'" char))))

(evil-define-motion evil-emacs-cursor-model-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (interactive "<c>")
  (evil-emacs-cursor-model-repeat-find-char (- (or count 1))))

(defun evil-emacs-cursor-model-forward-after-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (forward-thing thing count))
   (t
    (unless (bobp) (backward-char))
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when bnd
        (cond
         ((< (point) (cdr bnd)) (goto-char (car bnd)))
         ((= (point) (cdr bnd)) (setq count (1+ count)))))
      (condition-case nil
          (when (zerop (setq rest (forward-thing thing count)))
            (end-of-thing thing))
        (error))
      rest))))

(defun evil-emacs-cursor-model-backward-after-end (thing &optional count)
  "Move backward to end of THING.
The motion is repeated COUNT times.  This is the same as calling
`evil-emacs-cursor-model-forward-after-word-end' with -COUNT."
  (evil-emacs-cursor-model-forward-after-end thing (- (or count 1))))

(evil-define-motion evil-emacs-cursor-model-forward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT'th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (evil-emacs-cursor-model-forward-after-end thing count)))

(evil-define-motion evil-emacs-cursor-model-forward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT'th next WORD."
  :type inclusive
  (evil-emacs-cursor-model-forward-after-word-end count t))

(evil-define-motion evil-emacs-cursor-model-backward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT'th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-emacs-cursor-model-backward-after-end thing count)))

(evil-define-motion evil-emacs-cursor-model-backward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT'th previous WORD."
  :type inclusive
  (evil-emacs-cursor-model-backward-after-word-end count t))

;; ----------------------------------------------------------------------------
;;;; Redefine inclusive motion type to not include character after point.
(evil-define-type inclusive
  "Return the positions unchanged, with some exceptions.
If the end position is at the beginning of a line, then:

* If the beginning position is at or before the first non-blank
  character on the line, return `line' (expanded)."
  :expand (lambda (beg end) (evil-range beg end))
  :contract (lambda (beg end) (evil-range beg end))
  :normalize (lambda (beg end)
               (cond
                ((progn
                   (goto-char end)
                   (and (/= beg end) (bolp)))
                 (setq end (max beg (1- end)))
                 (cond
                  ((progn
                     (goto-char beg)
                     (looking-back "^[\f\s\t\v]*" (line-beginning-position)))
                   (evil-expand beg end 'line))
                  (t
                   (unless evil-cross-lines
                     (setq end (max beg (1- end))))
                   (evil-expand beg end 'inclusive))))
                (t
                 (evil-range beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

;; ----------------------------------------------------------------------------
;;;; Make "e" search offset put point after last character.
(defun evil-emacs-cursor-model-ad-evil-ex-search-adjust-offset (offset)
  "Make `evil-mode's \"e\" search OFFSET put point after last character."
  (unless (zerop (length offset))
    (save-match-data
      (string-match
       "^\\([esb]\\)?\\(\\([+-]\\)?\\([0-9]*\\)\\)$"
       offset)
      (when (and (= (aref offset (match-beginning 1)) ?e)
                 (not (bobp)))
        (forward-char)))))

(advice-add
 'evil-ex-search-goto-offset
 :after #'evil-emacs-cursor-model-ad-evil-ex-search-adjust-offset)

;; ----------------------------------------------------------------------------
;;;; `evil-jump-item' move point after matching delimeter if it jumps forward.
(evil-define-motion evil-emacs-cursor-model-jump-after-item (count)
  "Find the next item in this line immediately before
or somewhere after the cursor and jump to the corresponding one."
  :jump t
  :type inclusive
  (let ((pos (point)))
    (unless (or (bolp) (bobp)) (backward-char))
    (condition-case nil
        (evil-jump-item count)
      (user-error (goto-char pos)))
    (unless (< (point) pos)
      (goto-char pos)
      (evil-jump-item count)
      (when (> (point) pos) (forward-char)))))

(provide 'evil-emacs-cursor-model-mode)
;;; evil-emacs-cursor-model-mode.el ends here
