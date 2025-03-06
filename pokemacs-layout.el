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
    (_         'selected-window)))

(defun pokemacs-layout--get-buffer-name (buffer-action)
  "Returns the name associated to BUFFER-ACTION."
  (buffer-name (pokemacs-layout--get-buffer buffer-action)))

(defun pokemacs-layout--get-number (number)
  "Returns NUMBER as an integer.
NUMBER can be:
- nil: 1
- an integer i: i
- a variable: the value stored in this variable"
  (let ((number (eval number)))
    (cond
     ((and (symbolp number) (numberp number)) (symbol-value number))
     ((numberp number) number)
     ((functionp number) (funcall number))
     (t 1))))

(defun pokemacs-layout--lock-window (lock-window)
  "Lock the window if LOCK-WINDOW is t"
  (when lock-window
    (with-selected-window (selected-window)
      (set-window-dedicated-p (selected-window) t))))

(defun pokemacs-layout--apply-recursive-layout-windows-alist (layout-windows-alist)
  "Apply one level of LAYOUT-WINDOWS-ALIST and calls itself recursively."
  (cl-loop for (split-type . properties) in layout-windows-alist do
           (let (;; Get the split function
                 (split-function (pokemacs-layout--get-split-function split-type))
                 ;; save the currently selected window
                 (current-window (selected-window)))
             ;; Destruct the properties list
             (seq-let [action lock-window number] properties
               (let ((number (pokemacs-layout--get-number number)))
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
                   (dolist (layout-windows-alist action)
                     (pokemacs-layout--apply-recursive-layout-windows-alist layout-windows-alist)))
                  (t (message "unknown action '%S'" action)))
                 (select-window current-window)
                 (dotimes (_ (- number 1))
                   (pokemacs-layout--lock-window lock-window)
                   (select-window (funcall split-function))
                   (balance-windows)))))))

(defun pokemacs-layout--side-place (side)
  "Returns the place of SIDE in the window-side-slots list."
  (pcase side
    ('left    0)
    ('top     1)
    ('right   2)
    ('bottom  3)))

(defun pokemacs-layout--get-max-slots (side-properties)
  "Find max slot in slot properties."
  (cl-loop for (slot _ _) in side-properties maximize slot))

(defun pokemacs-layout--get-buffer (action)
  "Returns the buffer associated to ACTION."
  (cond
   ((functionp action)
    ;; If the action is a function we apply it in the current window
    (let ((previous-buffer (window-buffer)))
      (funcall action)
      (let ((new-buffer (current-buffer)))
        (set-window-buffer (selected-window) previous-buffer)
        new-buffer)))
   ((stringp action)
    ;; If the action is a string, create a buffer with this string as a name
    (get-buffer-create action))
   ((listp action)
    ;; If the action is a list, create a buffer with the first action
    (get-buffer-create (pokemacs-layout--get-buffer (car action))))
   (t (message "unknown action '%S'" action))))

(defun pokemacs-layout--apply-layout-sides-alist (layout-sides-alist)
  "For each side in LAYOUT-SIDES-ALIST, create its layout."
  (cl-loop
   for (side . properties) in layout-sides-alist do
   (let ((max_slots (pokemacs-layout--get-max-slots properties))
         (side-place (pokemacs-layout--side-place side)))
     ;; Replace the max number of slots for SIDE in window-side-slots
     (setf (nth side-place window-sides-slots) max_slots)
     (cl-loop
      for (slot buffer-action _) in properties do
      (let ((buffer (pokemacs-layout--get-buffer buffer-action)))
        (if (listp buffer-action)
            (add-to-list 'display-buffer-alist
                         `(,(mapconcat #'pokemacs-layout--get-buffer-name buffer-action "\\|")
                           display-buffer-in-side-window
                           (side . ,side)
                           (slot . ,slot)
                           (dedicated . t)
                           (window-width . ,pokemacs-layout-sidebar-width)))
          (add-to-list 'display-buffer-alist
                       `(,(pokemacs-layout--get-buffer-name buffer-action)
                         display-buffer-in-side-window
                         (side . ,side)
                         (slot . ,slot)
                         (dedicated . t)
                         (window-width . ,pokemacs-layout-sidebar-width))))
        (display-buffer-in-side-window
         buffer
         `((side . ,side)
           (slot . ,slot)
           (dedicated . t)
           (window-width . ,pokemacs-layout-sidebar-width))))))))

;;;###autoload
(defun pokemacs-layout--apply-layout (layout-windows-alist layout-sides-alist)
  "Creates a layout.
The layout is split in two parts:
- a layout of windows according to the provided LAYOUT-WINDOWS-ALIST
- a layout of sides according to the provided LAYOUT-SIDES-ALIST."
  ;; Disable visual-fill-column-mode if enabled as it doesn't work well with
  ;; programatically creating windows
  (when (and (boundp 'visual-fill-column-mode) visual-fill-column-mode)
    (visual-fill-column-mode -1))
  ;; Make sure that there's only one window
  (delete-other-windows)
  ;; Make sure that magit will create its buffer in the current window
  (defvar magit-display-buffer-function)
  (let ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
    (pokemacs-layout--apply-recursive-layout-windows-alist layout-windows-alist)
    (pokemacs-layout--apply-layout-sides-alist layout-sides-alist)))

(defvar pokemacs-layout-prog-default
  '(:windows
    ((column . (nil nil 2)))
    :sides
    ((right . ((1 magit-status-quick t)
               (2 ("*compilation*" "*lsp-help*") t))))))

(defvar pokemacs-layout-prog-default-nomagit
  '(:windows
    ((column . (nil nil 2)))
    :sides
    ((right . ((1 "*compilation*" t)
               (2 "*lsp-help*" t))))))

(defvar pokemacs-layout-prog-default-custom-number
  '(:windows
    ((column . (nil nil pokemacs-layout-columns)))
    :sides
    ((right . ((1 magit-status-quick t)
               (2 ("*compilation*" "*lsp-help*") t))))))

(defvar pokemacs-layout-elisp-default
  '(:windows
    ((column . (nil nil 2)))
    :sides
    ((right . ((1 magit-status-quick t)
               (2 ielm t)
               (3 "*Messages*" t))))))

(defun pokemacs-layout--create-layout (name layout-alist description)
  "Creates a property list containing the NAME of the layout, its content (LAYOUT-ALIST) and its DESCRIPTION."
  `(:name ,name :layout ,layout-alist :description ,description))

(defcustom pokemacs-layout-sidebar-width 70
  "Width of sidebars."
  :group 'pokemacs-layout
  :type 'int
  :tag " Sidebar width")

(defcustom pokemacs-layout-columns 3
  "Number of columns for the default layouts.
The n-1 first columns are unlocked vertical columns
The last one is a sidebar split in three locked horizontal windows:
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
      "prog default layout no magit"
      pokemacs-layout-prog-default-nomagit
      "3 vertical columns with last one being compilation | lsp-help")
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
         (layout-layout (plist-get layout :layout))
         (layout-windows (plist-get layout-layout :windows))
         (layout-sides (plist-get layout-layout :sides)))
    (pokemacs-layout--apply-layout layout-windows layout-sides)))

(defvar pokemacs-layout--toggled-side-windows nil)

(defun pokemacs-layout--disable-side-windows (&rest _)
  "Disable side-windows."
  (let* ((frame (window-normalize-frame nil))
         (window--sides-inhibit-check t))
    (when (window-with-parameter 'window-side nil frame)
      (setq pokemacs-layout--toggled-side-windows t)
      (window-toggle-side-windows))))

(defun pokemacs-layout--reenable-side-windows (&rest _)
  "Reenable side windows if it was disabled previously."
  (when pokemacs-layout--toggled-side-windows
    (setq pokemacs-layout--toggled-side-windows nil)
    (window-toggle-side-windows)))

(advice-add #'balance-windows :before #'pokemacs-layout--disable-side-windows)
(advice-add #'balance-windows :after #'pokemacs-layout--reenable-side-windows)

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
