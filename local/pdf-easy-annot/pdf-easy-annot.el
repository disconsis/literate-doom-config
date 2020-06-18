;;; pdf-easy-annot.el -- Easier annotations for PDF files -*- lexical-binding: t; -*-
;;
;; Commentary:
;; `pdf-tools' is one of the best pdf editors on Linux for annotating PDFs.
;; The most common type of annotations is highlighting, but the default workflow
;; for it is pretty cumbersome - you select the text, right-click, hover over
;; "Add markup annotation", and then choose "highlight". This really takes me
;; out of the flow of the reading. Introducing `pdf-easy-annot', a minor mode
;; which highlights everything you select.
;;
;; I thought about adding an option to modify the type of annotation
;; (underline, strikethrough.) but I never use those so I couldn't find the
;;
;; TODO Color the mode line indicator with the highlighting color
;; TODO Maybe color the bg of the indicator in the color of the pdf
;; TODO Add color buttons to mode line to quickly switch highlight colors
;; TODO Auto-add text annots on click (as opposed to selection - which highlights)
;;
;; Code:

(require 'pdf-annot)

(defun pdf-easy-annot/add-hl-advice (&rest _)
  (call-interactively #'pdf-annot-add-highlight-markup-annotation))


;; Possible indicators for other modes:
;;   ﲋ
(defvar pdf-easy-annot-hl-mode-line-indicator "ﭑ"
  "Mode line indicator for `pdf-easy-annot-hl-minor-mode'.
Must be set *before* turning on `pdf-easy-annot-hl-minor-mode'.
Set to `nil' to disable.")


(defun pdf-easy-annot/hl-mode-line-color ()
  "Foreground color to use for mode line indicator.
TODO This *should* be the color that will be used for the next highlighting.
Right now it's just a hack, since 'yellow' is the default highlight color."

  "yellow")


(defun pdf-easy-annot/hl-mode-line-info ()
  "Mode line sexp built from `pdf-easy-annot-hl-mode-line-indicator'.
Uses the foreground color which will be used for the next highlighting.
TODO check if the indicator is displayable, otherwise use a default ('HL' ?)"

  (let ((fg-color (pdf-easy-annot/hl-mode-line-color)))
    (propertize pdf-easy-annot-hl-mode-line-indicator 'font-lock-face
                `(:foreground ,fg-color))))


;;;###autoload
(define-minor-mode pdf-easy-annot-hl-minor-mode
  "A minor mode to automatically highlight text on selection."
  :init-value nil
  :lighter "ezHL"
  :keymap nil
  (if pdf-easy-annot-hl-minor-mode
      (turn-on-pdf-easy-annot-hl-minor-mode)
    (turn-off-pdf-easy-annot-hl-minor-mode)))


;;;###autoload
(defun turn-on-pdf-easy-annot-hl-minor-mode ()
  (interactive)
  (advice-add #'pdf-view-mouse-set-region :after #'pdf-easy-annot/add-hl-advice)
  (when pdf-easy-annot-hl-mode-line-indicator
    ;; Don't need to remove from mode-line-misc-info since we use `pushnew' to add it.
    (pushnew '(pdf-easy-annot-hl-minor-mode
               (:eval (pdf-easy-annot/hl-mode-line-info)))
             mode-line-misc-info)))


(defun turn-off-pdf-easy-annot-hl-minor-mode ()
  (interactive)
  (advice-remove #'pdf-view-mouse-set-region #'pdf-easy-annot/add-hl-advice))


(provide 'pdf-easy-annot)
;; pdf-easy-annot.el ends here
