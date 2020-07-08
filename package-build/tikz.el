;;; tikz.el --- A minor mode to edit TikZ pictures -*- lexical-binding:t -*-

;; Copyright (C) 2020  Emilio Torres-Manzanera <torres@uniovi.es>

;; Author: Emilio Torres-Manzanera <torres@uniovi.es>
;; Version: 0.11
;; Keywords: tex
;; URL: https://github.com/emiliotorres/tikz
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; * ~tikz~ A minor mode to edit TikZ pictures with Emacs

;; ~tikz~ is a minor mode for creating TikZ diagrams with Emcas as an
;; alternative to QtikZ.

;; ** Installation

;; - You should have a running environment of LaTeX.
;; - You should install the [[https://pwmt.org/projects/zathura/][Zathura]]
;; viewer, or change the default configuration.
;; - Copy this file into your =init.el= or load it
;; or use =M-x package-install RET tikz=.

;; ** Using it

;; - Create a Tex file with your favourite preamble.
;; - Once you have fixed the preamble in your document, launch the TikZ mode.
;; #+begin_example
;; M-x tikz-mode
;; #+end_example
;; - Now, when you are in idle time, the pdf will be refreshed
;;   automatically with the content of the current buffer.
;; - Every time you modify your preamble, you must turn off/on the TikZ mode.

;; ** Screenshot

;; On the left side, Emacs with a LaTeX buffer with minor mode TikZ. On
;; the right side, Zathura viewing the pdf.

;; [[file:graphics/tikzscreenshot.png]]

;; ** Configuration

;; - You can select another pdf viewer. Modify =tikz-zathura= with your chosen viewer.
;; - You can modify the idle timer to update the pdf. Modify =tikz-resume-timer=.


;; ** How it works

;; - It copies the current TeX buffer into a temp file.
;; - [[https://ctan.org/pkg/mylatexformat][Pre-compile the preamble]].
;; - Pdftex the temp file.
;; - It uses Zathura to view the pdf.
;; - In idle time, copy the current buffer to the temp file and compile
;;   it. Zathura automatically refreshes the view.

;; ** Alternatives
;; - QtikZ

;; ** ChangeLog

;; | date             |  ver | Change                 |
;; |------------------+------+------------------------|
;; | <2020-07-08 lun> | 0.11 | Time to launch Zathura |
;; | <2020-06-22 lun> |  0.1 |                        |

;;; Code:



(defvar tikz-resume-timer 1
  "Timer for refreshing pdf.")

(defvar tikz-zathura "zathura"
  "External pdf viewer.")




(defvar tikz-file-temp-tex-prefix "tikz"
  "Temporal file to use.")

(defvar tikz-preamble-precompiled "myformat"
  "Name of the precompiled preamble.")

(defvar tikz-buffer-compilation "*tikzing output*"
  "Output of the pdflatex with current buffer.")

(defvar tikz-resume-timer-internal nil
  "Timer for `tikz-run-pdflatex' to reschedule itself, or nil.")



(defun tikz-copy-current-buffer-to-temp-tex-file (buf filename preamble)
  "Create a file FILENAME with the current buffer BUF and `%&PREAMBLE' string.
%&preamble
content of BUF"
  (save-excursion
    ;; Insert
    (with-temp-buffer
      (insert (concat "%&" preamble "\n"))
      (write-region nil nil filename nil 'quiet))
    (set-buffer buf)
    (write-region nil nil filename t 'quiet)))



(defun tikz-run-pdflatex (input-buffer-tex
                          file-temp-tex
                          file-temp-preamble)
  "Copy INPUT-BUFFER-TEX to FILE-TEMP-TEX with preamble FILE-TEMP-PREAMBLE.

Run pdflatex in FILE-TEMP-TEX."
  (let* ((buffcompilation (get-buffer-create tikz-buffer-compilation))
         (dir-temp-tex (file-name-directory file-temp-tex)))
    ;; (message "%s hola %s" (current-time-string) file-temp-tex)
    ;; Paso I.. Guardar el actual buffer en un fichero temporal
    (tikz-copy-current-buffer-to-temp-tex-file input-buffer-tex
                                               file-temp-tex
                                               file-temp-preamble)
    ;; Paso II. Compilamos
    (with-current-buffer buffcompilation
      (erase-buffer))
    ;; Y lanzamos el proceso asíncrono
    (start-process "pdfing"  buffcompilation "pdflatex"
                   "-halt-on-error" "-file-line-error"
                   (concat "-output-directory=" dir-temp-tex) "-synctex=0" file-temp-tex)))


(defun tikz-run-current-buffer ()
  "Pdflatex this tex buffer several times (TikZing).

1. Kill previous process.
2. Create a temporal file.
3. Create a pre-compiled preamble.
4. Launch visor of pdf
5. Launch pdflatex in idle timer."
  (when (derived-mode-p 'latex-mode)
    (message "TikZing. (Do not modify preamble!) Pre-compiling...")
    (let* ((input-buffer-tex (current-buffer))
           (buffcompilation (get-buffer-create tikz-buffer-compilation))
           (file-temp-tex (concat (make-temp-file tikz-file-temp-tex-prefix) ".tex"))
           (file-temp-pdf (concat (file-name-sans-extension file-temp-tex) ".pdf"))
           (dir-temp-tex (file-name-directory file-temp-tex))
           (file-temp-preamble (concat "\"" file-temp-tex tikz-preamble-precompiled "\""))
           (secs 0))
      (when tikz-resume-timer-internal
        (tikz-kill)) ; Remove other/previous tikzing
      ;; Step I. Save the current buffer in a temp file
      (set-buffer input-buffer-tex)
      (write-region nil nil file-temp-tex nil 'quiet)
      ;;      (message "fichero temp: %s" file-temp-tex)
      ;; Step II. Pre-compile the preamble in a synchronous way.
      ;; How to pre-compile preamble: See https://ctan.org/pkg/mylatexformat
      ;; This pre-compilation is done only one time.
      (set-buffer buffcompilation)
      (erase-buffer)
      (call-process  "pdflatex" nil buffcompilation nil
                     "-ini" (concat "-output-directory=" dir-temp-tex)
                     (concat "-jobname=" file-temp-preamble )
                     "\"&pdflatex\""
                     "mylatexformat.ltx"
                     (concat "\"" file-temp-tex "\""))
      (message "TikZing. (Do not modify preamble!) Pre-compiling...done")
      ;;
      ;; Activamos zathura, el visor externo de pdf
      ;; necesita un pdf.
      ;;

      ;; Step III. Create a pdf version of the file-temp-tex.
      ;; We need a pdf file to launch Zathura
      (tikz-run-pdflatex input-buffer-tex
                         file-temp-tex
                         file-temp-preamble)
      ;;
      ;; Step IV. Open asynchronously the pdf with Zathura
      ;; But first we have to wait to pdftex finishes.
      ;; Zathura updates it automatically.
      ;; (message "pdf %s" file-temp-pdf)
      ;; Wait until the pdf exists.
      (while (and (not (file-exists-p file-temp-pdf))
                  (< secs 20))
        (message "TikZing waiting for the first compilation (%s seconds)..." (- 20 secs))
        (sit-for 1)
        (setq secs (+ 1 secs)))
      ;; Otherwise, Zathura fails.
      (start-process (concat "tikz" tikz-zathura)  nil  tikz-zathura file-temp-pdf)
      (message "TikZing waiting for the first compilation...done")
      ;;
      ;; Step V. Pdflatex when in idle time
      (setq tikz-resume-timer-internal
            (run-with-idle-timer tikz-resume-timer-internal
                                 t
                                 'tikz-run-pdflatex
                                 input-buffer-tex
                                 file-temp-tex
                                 file-temp-preamble)))))



(defun tikz-kill ()
  "Kill the process of pdflatexing."
  (interactive)
  ;; Remove run-idle-timer
  (unless tikz-resume-timer-internal
    (cancel-timer tikz-resume-timer-internal))
  (cancel-function-timers 'tikz-run-pdflatex)
  ;; Kill process
  (when (get-process (concat "tikz" tikz-zathura))
    (delete-process (get-process (concat "tikz" tikz-zathura))))
  (when (get-process tikz-buffer-compilation)
    (delete-process (get-process tikz-buffer-compilation))))

;;;###autoload
(define-minor-mode tikz-mode
  "Drawing interface for TikZ pictures.

Compile your (short) one page TeX file with TikZ pictures.

Do not modify your preamble. If you change it, turn off this mode
and turn on it again.

Similar to the program QtikZ."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " TikZ"
  ;; The minor mode bindings. '(([C-backspace] . hungry-electric-delete))
  nil
  (if tikz-mode
      (progn
        (setq tikz-resume-timer-internal tikz-resume-timer)
        (tikz-run-current-buffer))
    (tikz-kill))
  :group 'tikz)




(provide 'tikz)
;;; tikz.el ends here
