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
;; TODO Add color buttons to mode line to quickly switch highlight colors
;;
;; Code:

(require 'pdf-annot)

(defun pdf-easy-annot/add-hl-advice (&rest _)
  (call-interactively #'pdf-annot-add-highlight-markup-annotation))

(defvar pdf-easy-annot-hl-mode-line-indicator "ﭑ"
  "Mode line indicator for `pdf-easy-annot-hl-minor-mode'.
Must be set *before* turning on `pdf-easy-annot-hl-minor-mode'.
Set to `nil' to disable.")


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
    (push `(pdf-easy-annot-hl-minor-mode
            (" " ,pdf-easy-annot-hl-mode-line-indicator " "))
          mode-line-misc-info)))

(defun turn-off-pdf-easy-annot-hl-minor-mode ()
  (interactive)
  (advice-remove #'pdf-view-mouse-set-region #'pdf-easy-annot/add-hl-advice))


(provide 'pdf-easy-annot)
;; pdf-easy-annot.el ends here
