;;; pokemacs-layout.el --- Pokemacs-layout -*- lexical-binding: t -*-
;;

;; Copyright (c) 2022 mattiasdrp and contributors.

;; Author: mattiasdrp
;; Maintainer: mattiasdrp <https://github.com/mattiasdrp>
;; Created: 09 November 2024
;; Version: 0.1.0
;; Licence: MIT
;; Keywords: emacs, layout, convenience
;; URL: https://github.com/mattiasdrp/pokemacs-layout
;; Package-Requires: ((emacs "27"))
;;; Commentary:

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defgroup pokemacs-layout nil
  "Customisation group for pokemacs-layout."
  :group 'emacs
  :tag "Pokemacs layout customisation")

(defun pokemacs-layout--get-split-function (split-type)
  "Returns the split function according to SPLIT-TYPE."
  (pcase split-type
    ('column   'split-window-right)
    ('row      'split-window-below)
    ('none     'selected-window)))

(define-minor-mode pokemacs-layout-locked-window-buffer-mode
  "Make the current window always display this buffer."
  :lighter "locked"
  (set-window-dedicated-p (selected-window) pokemacs-layout-locked-window-buffer-mode))

(defun pokemacs-layout--lock-window (lock-window)
  "Lock the window if LOCK-WINDOW is t"
  (when lock-window
    (with-selected-window (selected-window)
      (pokemacs-layout-locked-window-buffer-mode))))

(defun pokemacs-layout--get-number (number)
  "Returns NUMBER as an integer.
NUMBER can be:
nil: 1
an integer i: i
a variable: the value stored in this variable"
  (let ((number (eval number)))
    (cond
     ((and (symbolp number) (numberp number)) (symbol-value number))
     ((numberp number) number)
     ((functionp number) (funcall number))
     (t 1))))

(defun pokemacs-layout--apply-recursive-layout-alist (layout-alist)
  "Apply one level of LAYOUT-ALIST and calls itself recursively."
  (cl-loop for (split-type . properties) in layout-alist do
           (let (;; Get the split function
                 (split-function (pokemacs-layout--get-split-function split-type))
                 ;; save the currently selected window
                 (current-window (selected-window)))
             ;; Destruct the properties list
             (seq-let [action lock-window number] properties
               (let ((number (pokemacs-layout--get-number number)))
                 ;; If number is nil then we split only once
                 (cond
                  ((functionp action)
                   ;; If the action is a function we apply it in the current window
                   (funcall action))
                  ((stringp action)
                   ;; If the action is a string, create a buffer with this string as a name
                   (set-window-buffer (selected-window) (get-buffer-create action)))
                  ((listp action)
                   ;; If the action is a list it means the current window will be
                   ;; split again in sub-windows, call the function recursively
                   (dolist (layout-alist action)
                     (pokemacs-layout--apply-recursive-layout-alist layout-alist)))
                  (t (message "unknown action '%S'" action)))
                 (select-window current-window)
                 (dotimes (_ number)
                   (pokemacs-layout--lock-window lock-window)
                   (select-window (funcall split-function))
                   (balance-windows)))))))

;;;###autoload
(defun pokemacs-layout--apply-layout (layout-alist)
  "Creates a layout of windows according to the provided LAYOUT-ALIST."
  ;; Disable visual-fill-column-mode if enabled as it doesn't work well with
  ;; programatically creating windows
  (when (and (boundp 'visual-fill-column-mode) visual-fill-column-mode)
    (visual-fill-column-mode -1))
  ;; Make sure that there's only one window
  (delete-other-windows)
  ;; Make sure that magit will create its buffer in the current window
  (defvar magit-display-buffer-function)
  (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
    (pokemacs-layout--apply-recursive-layout-alist layout-alist)))

(defvar pokemacs-layout-prog-default
  '((column . (nil nil 2))
    (none . ('((row . (magit-status-quick t))
               (row . ("*compilation*" t))
               (none . ("*lsp-help*" t))) nil nil))))

(defvar pokemacs-layout-prog-default-custom-number
  '((column . (nil nil pokemacs-layout-columns))
    (none . ('((row . (magit-status-quick t))
               (row . ("*compilation*" t))
               (none . ("*lsp-help*" t))) nil nil))))

(defvar pokemacs-layout-elisp-default
  '((column . (nil nil 2))
    (none . ('((row . (magit-status-quick t))
               (row . (ielm  t))
               (none . ("*Messages*" t))) nil nil))))

(defun pokemacs-layout--create-layout (name layout-alist description)
  "Creates a property list containing the NAME of the layout, its content (LAYOUT-ALIST) and its DESCRIPTION."
  `(:name ,name :layout ,layout-alist :description ,description))


(defcustom pokemacs-layout-columns 3
  "Number of columns for the default layouts.
The n-1 first columns are unlocked vertical columns
The last one is split in three locked horizontal windows:
- magit
- compilation or ielm
- lsp-help or Messages"
  :group 'pokemacs-layout
  :type 'int
  :tag " Layout columns")

(defcustom pokemacs-layout-layouts
  `(,(pokemacs-layout--create-layout
      "prog default layout"
      pokemacs-layout-prog-default
      "3 vertical columns with last one being magit | compilation | lsp-help")
    ,(pokemacs-layout--create-layout
      "prog custom layout"
      pokemacs-layout-prog-default-custom-number
      "vertical custom number of columns with last one being magit | compilation | lsp-help")
    ,(pokemacs-layout--create-layout
      "elisp default layout"
      pokemacs-layout-elisp-default
      "vertical columns with last one being magit | ielm | messages"))
  "List of layouts.
A LAYOUT can be declare with `pokemacs-layout--create-layout'.
It is a list of alists of (SPLIT-TYPE . LAYOUT)
SPLIT-TYPE is `column', `row', `none'.
  `none' should always be used for the last alist of each LAYOUT since it dictates the kind of the next sibling."
  :group 'pokemacs-layout
  :type 'list
  :tag " Layouts")

(defun pokemacs-layout--extract-layout-property (property)
  "Extract the PROPERTY from a layout."
  (mapcar (lambda (layout) (plist-get layout property)) pokemacs-layout-layouts))

(defun pokemacs-layout--extract-layout-with-name (name)
  "Extracts the layout which :name is NAME."
  (cl-find-if (lambda (layout) (string= (plist-get layout :name) name)) pokemacs-layout-layouts))

(defun pokemacs-layout--annotate-layout (name)
  "Adds an annotation to NAME."
  (let* ((layout (pokemacs-layout--extract-layout-with-name name))
         (description (plist-get layout :description)))
    (format "  %s" description)))

(defun pokemacs-layout--apply (name)
  "Apply the layout linked to NAME."
  (let* ((layout (pokemacs-layout--extract-layout-with-name name))
         (layout-layout (plist-get layout :layout)))
    (pokemacs-layout--apply-layout layout-layout)))

;;;###autoload
(defun pokemacs-layout-apply (layout-name)
  "Prompt the user for a LAYOUT to apply and applies it."
  (interactive
   (let* (
          (layout-names (pokemacs-layout--extract-layout-property :name))
          (completion-extra-properties `(:annotation-function ,#'pokemacs-layout--annotate-layout)))
     (list (completing-read "Layout: " layout-names))))
  (pokemacs-layout--apply layout-name))

;; End:
(provide 'pokemacs-layout)

;;; pokemacs-layout.el ends here
