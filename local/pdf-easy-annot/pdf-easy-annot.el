;;; pdf-easy-annot.el -- Easier annotations for PDF files -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; `pdf-tools' is one of the best pdf editors on Linux for annotating PDFs, but it
;; lacks has some really cumbersome workflows. This package aims to fix those.
;;
;; 1. Easier highlighting The most common type of annotations is highlighting,
;; but the default workflow requires a lot of steps - you select the text,
;; right-click, hover over "Add markup annotation", and then choose "highlight".
;; You have to do this *every* time you highlight something, which for me is on
;; the order of once every 5 seconds. This really takes me out of the flow of
;; the reading. We provide a minor-mode which highlights everything you select.
;;
;; I thought about adding an option to modify the type of annotation (underline,
;; strikethrough.) but I never use those so I couldn't find the motivation.
;;
;;
;; 2. Distinguishing markers for content-carrying markup annotations By default,
;; the only way to tell that a markup annotation has any contents is to hover
;; over it an read the echo area. This makes it very easy to not see some
;; comments, which in turn makes me fearful enough to add text annotations
;; beside the markup to carry the contents instead. This is beyond tedious and
;; deserves a fix.
;;
;; There aren't a lot of ways to mark annotations - there's no way to put a star
;; on the annotations or something. We can only use the tools we do have -
;; highlight, underline, squiggly, strike-out, and icons. Getting icons to
;; demarcate which annotation they're referring to seems nigh-impossible since
;; we don't have easy access to line drawings. Changing the highlight color also
;; seems brittle since I might choose to use that color for actual highlights
;; later. Since I primarily use highlights and almost never use squigglies, I
;; can add a squiggly annotation to the highlight annotation to mark it as
;; containing contents.
;;
;; TODO It would be nice to have a flag which allows creating these markers on a
;;      file which has been marked up without this mode.
;; FIXME marker association map must be saved for each file, in case an annotation
;;        with contents marked by this mode is deleted.
;;
;; Planned features:
;;
;; TODO Color the mode line indicator with the highlighting color
;; TODO Maybe color the bg of the indicator in the color of the pdf
;; TODO Add color buttons to mode line to quickly switch highlight colors
;; TODO Auto-add text annots on click (as opposed to selection - which highlights)
;; TODO Enable mouse passthrough when clicking on an annotation to activate it
;; TODO Change `pdf-view-region' face's background to active highlight color
;;
;;; Code:

(require 'pdf-annot)
(require 'dash)
(require 's)
(require 'cl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Easier highlighting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pdf-easy-annot/add-hl-advice (&rest _)
  "Auto-highlight selected region."
  (when-let ((region (pdf-view-active-region t)))
    (pdf-annot-add-highlight-markup-annotation
     region pdf-easy-annot--auto-hl-active-color)))


;; Possible indicators for other modes:
;;   ﲋ
(defvar pdf-easy-annot-auto-hl-mode-line-indicator "ﭑ"
  "Mode line indicator for `pdf-easy-annot-auto-hl-minor-mode'.
Must be set *before* turning on `pdf-easy-annot-auto-hl-minor-mode'.
Set to `nil' to disable.")


(defvar pdf-easy-annot-auto-hl-button-colors '("yellow")
  "List of colors to make buttons for, irrespective of history.")


(defvar pdf-easy-annot--auto-hl-active-color
  (cdr (assq 'color (assq 'highlight pdf-annot-default-annotation-properties)))
  "Color to use for future highlights.
Use the defaults from `pdf-annot-default-annotation-properties' at the start.")

(defvar pdf-easy-annot-color-button-indicator " "
  "String used for the color buttons on the modeline.")

(defun pdf-easy-annot--auto-hl-color-make-button (color)
  (propertize
   pdf-easy-annot-color-button-indicator
   'help-echo (format "mouse-1: change highlight color to %s
mouse-3: remove this button" color)
   'font-lock-face `(:foreground ,color)
   'mouse-face `(:foreground ,color :background ,color)
   'local-map
   (let ((map (make-sparse-keymap)))
     (define-key map [mode-line mouse-1]
       (lambda (&rest _)
         (interactive)
         (when (not (equal color pdf-easy-annot--auto-hl-active-color))
           (setq pdf-easy-annot--auto-hl-active-color color)
           (force-mode-line-update) ;; to update indicator color
           )))
     (define-key map [mode-line mouse-3]
       (lambda (&rest _)
         (interactive)
         (setq pdf-easy-annot-auto-hl-button-colors
               (remove color pdf-easy-annot-auto-hl-button-colors))
         (setq pdf-annot-color-history
               (remove color pdf-annot-color-history))
         (message (format "color button '%s' removed" color))
         (force-mode-line-update)))
     map)))


(defun pdf-easy-annot/auto-hl-mode-line-info ()
  "Mode line sexp built from `pdf-easy-annot-auto-hl-mode-line-indicator'.
Uses the foreground color which will be used for the next highlighting.
TODO check if the indicator is displayable, otherwise use a default ('HL' ?)
TODO Add a button for the default color"
  (s-join ""
   (-snoc
    (-map #'pdf-easy-annot--auto-hl-color-make-button
          (-union pdf-easy-annot-auto-hl-button-colors pdf-annot-color-history))
    " | "
    (propertize pdf-easy-annot-auto-hl-mode-line-indicator
                'font-lock-face `(:foreground ,pdf-easy-annot--auto-hl-active-color)
                'help-echo "mouse-1: disable auto-hl minor mode"
                'local-map
                (make-mode-line-mouse-map
                 'mouse-1
                 (lambda (&rest _)
                   (interactive)
                   (pdf-easy-annot-auto-hl-minor-mode -1)
                   (force-mode-line-update)))))))


;;;###autoload
(define-minor-mode pdf-easy-annot-auto-hl-minor-mode
  "A minor mode to automatically highlight text on selection."
  :init-value nil
  :lighter "ezHL"
  :keymap nil
  (if pdf-easy-annot-auto-hl-minor-mode
      (turn-on-pdf-easy-annot-auto-hl-minor-mode)
    (turn-off-pdf-easy-annot-auto-hl-minor-mode)))


;;;###autoload
(defun turn-on-pdf-easy-annot-auto-hl-minor-mode ()
  (interactive)
  (advice-add #'pdf-view-mouse-set-region :after #'pdf-easy-annot/add-hl-advice)
  (when pdf-easy-annot-auto-hl-mode-line-indicator
    ;; Don't need to remove from mode-line-misc-info since we use `cl-pushnew' to add it.
    (cl-pushnew '(pdf-easy-annot-auto-hl-minor-mode
               (:eval (pdf-easy-annot/auto-hl-mode-line-info)))
             mode-line-misc-info)))


(defun turn-off-pdf-easy-annot-auto-hl-minor-mode ()
  (interactive)
  (advice-remove #'pdf-view-mouse-set-region #'pdf-easy-annot/add-hl-advice))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Distinguishing markers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar-local pdf-easy-annot-content-markers (make-hash-table)
  "Association of IDs of content-carrying markup annotations with their markers.")

(defun pdf-easy-annot/add-squiggly-content-marker-to-annot (annot)
  "Add a squiggly annotation to the same area used by another markup annotation.
Does not do anything to text annotations and squiggly annotations.

Modifies `pdf-easy-annot-content-markers' to associate annot with its new marker.

Since I primarily use highlights and almost never use squigglies, I can add
a squiggly annotation to the highlight annotation to mark it as containing
contents."

  (when (memq (pdf-annot-get-type annot) '(highlight underline strike-out))
    (let* ((annot-id (pdf-annot-get-id annot))
           (marker-annot
            (pdf-annot-add-annotation 'squiggly
                                      (pdf-annot-get-display-edges annot)
                                      nil
                                      (pdf-annot-get annot 'page))))
      (puthash annot-id (pdf-annot-get-id marker-annot)
               pdf-easy-annot-content-markers))))

(defun pdf-easy-annot/remove-content-marker (annot)
  "Remove content marker for an annotation.

We must make sure to delete this squiggly if the original annotation is deleted
or its contents removed."

  (when-let* ((annot-id (pdf-annot-get-id annot))
              (marker-id (gethash annot-id pdf-easy-annot-content-markers)))
    (pdf-info-delannot marker-id)
    (remhash annot-id pdf-easy-annot-content-markers)))


(defun pdf-easy-annot/manage-marker-for-changed-annot (annot)
  "Manage (create/delete) markers for changed annotations."
  (require 's)
  (when (pdf-annot-markup-annotation-p annot)
    (let ((has-contents (s-present? (pdf-annot-get annot 'contents)))
          (has-marker (pdf-annot-get annot 'content-marker)))
      (cond
       ((and has-contents (not has-marker))
        (pdf-easy-annot/add-squiggly-content-marker-to-annot annot))

       ((and (not has-contents) has-marker)
        (pdf-easy-annot/remove-content-marker annot))))))


(defun pdf-easy-annot/manage-markers-on-modifications (modified-annots)
  "Manage markers for *all* modified annotations.

TODO Make sure this doesn't start a recursive modification chain."
  (let ((pdf-annot-inhibit-modification-hooks t))
    (dolist (modification '(:inserted :changed))
      (mapc #'pdf-easy-annot/manage-marker-for-changed-annot
            (funcall modified-annots modification)))
    (mapc #'pdf-easy-annot/remove-content-marker
          (funcall modified-annots :deleted))))


(defun pdf-easy-annot/enable-manage-markers ()
  (cl-pushnew #'pdf-easy-annot/manage-markers-on-modifications
              pdf-annot-modified-functions))


;;;###autoload
(define-minor-mode pdf-easy-annot-content-markers-minor-mode
  "A minor mode which automatically adds squigglies to markup annotations which
have contents."
  :init-value nil
  :lighter "ezMark"
  :keymap nil
  (if pdf-easy-annot-auto-hl-minor-mode
      (turn-on-pdf-easy-annot-content-markers-minor-mode)
    (turn-off-pdf-easy-annot-content-markers-minor-mode)))

;;;###autoload
(defun turn-on-pdf-easy-annot-content-markers-minor-mode ()
  (interactive)
  (add-hook 'pdf-view-mode-hook #'pdf-easy-annot/enable-manage-markers))

(defun turn-off-pdf-easy-annot-content-markers-minor-mode ()
  (interactive)
  (remove-hook 'pdf-view-mode-hook #'pdf-easy-annot/enable-manage-markers))

(provide 'pdf-easy-annot)
;;; pdf-easy-annot.el ends here
