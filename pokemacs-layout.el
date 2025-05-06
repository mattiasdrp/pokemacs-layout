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
;; Package-Requires: ((emacs "27.1"))
;;; Commentary:

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defgroup pokemacs-layout nil
  "Customisation group for pokemacs-layout."
  :group 'emacs
  :tag "pokemacs layout customisation")

(defcustom pokemacs-layout-main-monitor "DP-3"
  "Main monitor for Emacs."
  :group 'pokemacs-layout
  :type 'string
  :tag "pokemacs layout main monitor")

(defcustom pokemacs-layout-second-monitor "DP-1"
  "Second monitor for Emacs."
  :group 'pokemacs-layout
  :type 'string
  :tag "pokemacs layout second monitor")

(defun pokemacs-layout--get-split-function (split-type)
  "Return the split function according to SPLIT-TYPE."
  (pcase split-type
    ('column   'split-window-right)
    ('row      'split-window-below)
    (_         'selected-window)))

(defun pokemacs-layout--get-buffer-name (buffer-action)
  "Return the name associated to BUFFER-ACTION."
  (buffer-name (pokemacs-layout--get-buffer buffer-action)))

(defun pokemacs-layout--get-number (number)
  "Return NUMBER as an integer.

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
  "Lock the window if LOCK-WINDOW is t."
  (when lock-window
    (with-selected-window (selected-window)
      (set-window-dedicated-p (selected-window) t))))

(defun pokemacs-layout--apply-recursive-layout-windows-alist (main-buffer layout-windows-alist)
  "Apply one level of LAYOUT-WINDOWS-ALIST and call itself recursively."
  (with-current-buffer main-buffer
    (cl-loop for ((split-type . properties) . rest) on layout-windows-alist do
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
                       (pokemacs-layout--apply-recursive-layout-windows-alist main-buffer layout-windows-alist)))
                    (t (message "unknown action '%S'" action)))
                   (select-window current-window)
                   ;; If this is the last element and it's split-type is not none, split one less time
                   (let ((number (if (and (null rest) (not (eq split-type 'none)))
                                     (- number 1)
                                   number)))
                     (dotimes (_ number)
                       (pokemacs-layout--lock-window lock-window)
                       (select-window (funcall split-function))
                       (balance-windows)))))))))

(defun pokemacs-layout--side-place (side)
  "Return the place of SIDE in the window-side-slots list."
  (pcase side
    ('left    0)
    ('top     1)
    ('right   2)
    ('bottom  3)))

(defun pokemacs-layout--get-max-slots (side-properties)
  "Find max slot in SIDE-PROPERTIES."
  (cl-loop for (slot _ _) in side-properties maximize slot))

(defun pokemacs-layout--get-buffer (action)
  "Return the buffer associated to ACTION."
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

(defvar pokemacs-layout--side-windows '())

(defun pokemacs-layout--display-buffer-in-tagged-side-window (buffer alist)
  "Reuse a side window on any frame tagged with the appropriate type.
  Falls back to `display-buffer-in-side-window` if none found."
  (message "buffer: %S, buffer name: %S" buffer (buffer-name buffer))
  (let* ((buffer-name (buffer-name buffer))
         (reused-window
          (or
           ;; Search in the saved side windows even if they're hidden
           (car (cl-find-if (lambda (cell)
                              (cl-destructuring-bind (_ . regex) cell
                                (string-match-p regex buffer-name))) pokemacs-layout--side-windows))
           ;; Otherwise, search in all the live windows in every frame
           (catch 'found
             (dolist (frame (frame-list))
               (when (frame-visible-p frame)
                 (dolist (window (window-list frame 'nomini))
                   (message "window %S, parameters %S" window (window-parameters window))
                   (when-let* ((window-parameter (window-parameter window 'pokemacs-layout-buffer-type))
                               (_ (string-match-p window-parameter buffer-name)))
                     (throw 'found window)))))))))
    (if reused-window
        ;; Use it
        (progn
          (unless (window-live-p reused-window)
            ;; if the window is not live, this means it's a side-window that was hidden
            ;; toggle on all side windows (TODO: just toggle on the necessary side window)
            (dolist (frame (frame-list))
              (unless (eq (frame-parameter frame 'minibuffer) 'only)
                (ignore-error (window-toggle-side-windows frame)))))
          (window--display-buffer buffer reused-window 'reuse alist))
      ;; Create a new side window and tag it
      (let ((new-window (display-buffer-in-side-window buffer alist)))
        (set-window-parameter new-window 'pokemacs-layout-buffer-type buffer-name)
        new-window))))

(defun pokemacs-layout--sidebar-width ()
  "Return `pokemacs-layout-sidebar-width' as an absolute value.

If `pokemacs-layout-sidebar-width' is an absolute value, return it.
If it's a percentage, return the width corresponding to the percentage of the value
returned by `frame-width'."
  (cond
   ((integerp pokemacs-layout-sidebar-width) pokemacs-layout-sidebar-width)
   ((and (stringp pokemacs-layout-sidebar-width)
         (string-match "^\\([0-9]+\\)%?$" pokemacs-layout-sidebar-width))
    (round (* (/ (string-to-number (match-string 1 pokemacs-layout-sidebar-width)) 100.0) (frame-width))))
   (t (error "Invalid size format: %S" value))))

(defun pokemacs-layout--apply-layout-sides-alist (layout-sides-alist &optional main-buffer)
  "For each side in LAYOUT-SIDES-ALIST, create its layout."
  (with-current-buffer (if main-buffer main-buffer (current-buffer))
    (cl-loop
     for (side . properties) in layout-sides-alist do
     (let ((max_slots (pokemacs-layout--get-max-slots properties))
           (side-place (pokemacs-layout--side-place side)))
       ;; Replace the max number of slots for SIDE in window-side-slots
       (setf (nth side-place window-sides-slots) max_slots)
       (cl-loop
        for (slot buffer-action _) in properties do
        (let ((buffer (pokemacs-layout--get-buffer buffer-action))
              (buffer-regex (if (listp buffer-action)
                                (mapconcat #'pokemacs-layout--get-buffer-name buffer-action "\\|")
                              (pokemacs-layout--get-buffer-name buffer-action))))
          (add-to-list 'display-buffer-alist
                       `(,buffer-regex
                         pokemacs-layout--display-buffer-in-tagged-side-window
                         (side . ,side)
                         (slot . ,slot)
                         (dedicated . t)
                         (inhibit-switch-frame nil)
                         (reusable-frames . t)
                         (window-width . ,(pokemacs-layout--sidebar-width))))
          (let ((new-window (display-buffer-in-side-window
                             buffer
                             `((side . ,side)
                               (slot . ,slot)
                               (dedicated . t)
                               (inhibit-switch-frame nil)
                               (reusable-frames . t)
                               (window-width . ,(pokemacs-layout--sidebar-width))))))
            (push (cons new-window buffer-regex) pokemacs-layout--side-windows)
            (set-window-parameter new-window 'pokemacs-layout-buffer-type buffer-regex))))))))

(defvar pokemacs-layout--default-main-frame nil "Main frame on which other frames depend")

(defun pokemacs-layout--apply-layout (layout-windows-alist &optional layout-sides-alist monitor)
  "Create a layout.

  The layout is split in three parts:
  - the (optional) MONITOR where it should be created. The main frame should not specify a MONITOR
  - a layout of windows according to the provided LAYOUT-WINDOWS-ALIST
  - a (optional) layout of sides according to the provided LAYOUT-SIDES-ALIST."
  ;; Disable visual-fill-column-mode if enabled as it doesn't work well with
  ;; programatically creating windows
  (when (and (boundp 'visual-fill-column-mode) visual-fill-column-mode)
    (visual-fill-column-mode -1))
  (let ((frame (if monitor (make-frame-on-monitor monitor) (selected-frame))))
    (select-frame frame)
    ;; Make sure that there's only one window
    (delete-other-windows)
    ;; Make sure that magit will create its buffer in the current window
    (defvar magit-display-buffer-function)
    (let* ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
           (buffer (current-buffer)))
      (pokemacs-layout--apply-recursive-layout-windows-alist buffer layout-windows-alist)
      (pokemacs-layout--apply-layout-sides-alist layout-sides-alist buffer))))

(defun pokemacs-layout--extract-layout-property (property)
  "Extract the PROPERTY from a layout."
  (mapcar (lambda (layout) (plist-get layout property)) pokemacs-layout-layouts))

(defun pokemacs-layout--extract-layout-with-name (name)
  "Extract the layout which :name is NAME."
  (cl-find-if (lambda (layout) (string= (plist-get layout :name) name)) pokemacs-layout-layouts))

(defun pokemacs-layout--annotate-layout (name)
  "Add an annotation to NAME."
  (let* ((layout (pokemacs-layout--extract-layout-with-name name))
         (description (plist-get layout :description)))
    (format "  %s" description)))

(defun pokemacs-layout--apply-monitor-layout (monitor-layout)
  "Apply MONITOR-LAYOUT."
  (let* ((monitor (plist-get monitor-layout :monitor))
         (layout-windows (plist-get monitor-layout :windows))
         (layout-sides (plist-get monitor-layout :sides))
         (children (plist-get monitor-layout :children)))
    (pokemacs-layout--apply-layout layout-windows layout-sides monitor)
    (when children
      (dolist (child-monitor-layout children)
        (pokemacs-layout--apply-monitor-layout child-monitor-layout)))))

(defun pokemacs-layout--apply (name)
  "Apply the layout linked to NAME."
  (let* ((layout (pokemacs-layout--extract-layout-with-name name))
         (monitor-layouts (plist-get layout :layout)))
    (dolist (monitor-layout monitor-layouts) (pokemacs-layout--apply-monitor-layout monitor-layout))))

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

(defconst pokemacs-layout-prog-default
  '((:windows
     ((column . ("*Messages*" nil 1))
      (none . ("*scratch*" nil 1)))
     :sides
     ((right . ((1 magit-status-quick t)
                (2 ("*compilation*" "*lsp-help*") t)))))))

(defconst pokemacs-layout-prog-default-nomagit
  '((:windows
     ((column . (nil nil pokemacs-layout-columns)))
     :sides
     ((right . ((1 "*compilation*" t)
                (2 "*lsp-help*" t)))))))

(defconst pokemacs-layout-prog-default-custom-number
  '((:windows
     ((column . (nil nil pokemacs-layout-columns)))
     :sides
     ((right . ((1 magit-status-quick t)
                (2 ("*compilation*" "*lsp-help*") t)))))))

(defconst pokemacs-layout-prog-default-frames-custom-number
  `((:windows
     ((column . (nil nil pokemacs-layout-columns)))
     :sides
     ((right . ((1 magit-status-quick t)
                (2 ("*compilation*" "*lsp-help*") t))))
     :children
     ((:monitor ,pokemacs-layout-second-monitor
                :windows
                ((column . ("*Messages*" nil 1))
                 (none . ("*scratch*" nil 1))))))))

(defconst pokemacs-layout-elisp-default
  '((:windows
     ((column . (nil nil pokemacs-layout-columns)))
     :sides
     ((right . ((1 magit-status-quick t)
                (2 ielm t)
                (3 "*Messages*" t)))))))

(defun pokemacs-layout--create-layout (name layout-alist description)
  "Create a property list containing the NAME of the layout, its content (LAYOUT-ALIST) and its DESCRIPTION."
  `(:name ,name :layout ,layout-alist :description ,description))

(defcustom pokemacs-layout-sidebar-width "50%"
  "Width of sidebars."
  :group 'pokemacs-layout
  :type '(choice (integer :tag "Absolute")
                 (strinng :tag "Percentage"))
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
      "prog custom monitors layout"
      pokemacs-layout-prog-default-frames-custom-number
      "vertical custom number of columns with last one being magit | compilation | lsp-help")
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

(defun pokemacs-layout-current-monitor ()
  "Return the current monitor."
  (interactive)
  (let ((monitor (cdr (assq 'name (frame-monitor-attributes)))))
    (message "Current monitor %S" monitor)
    monitor))

;;;###autoload
(defun pokemacs-layout-monitor-names ()
  "Return the names of all the plugged monitors."
  (interactive)
  (let ((monitors (mapcar (lambda (a)
                            (cdr (assq 'name a)))
                          (display-monitor-attributes-list))))
    (message "Monitors: %S" monitors)
    monitors))

;;;###autoload
(defun pokemacs-layout-apply (layout-name &optional columns)
  "Prompt the user for a LAYOUT-NAME to apply and apply it.

  COLUMNS can be set with `C-u', the `universal-argument'
  or are equal to `pokemacs-layout-columns' by default"
  (interactive
   (let* (
          (layout-names (pokemacs-layout--extract-layout-property :name))
          (completion-extra-properties `(:annotation-function ,#'pokemacs-layout--annotate-layout)))
     (list (completing-read "Layout: " layout-names) current-prefix-arg)))
  (setq pokemacs-layout--default-main-frame (selected-frame))
  (let* ((pokemacs-layout-columns (or columns pokemacs-layout-columns))
         (current-prefix-arg nil))
    (call-interactively #'pokemacs-layout-reset)
    ;; (add-to-list 'display-buffer-alist
    ;;              '(t (display-buffer-reuse-window
    ;;                   display-buffer-reuse-mode-window
    ;;                   display-buffer-pop-up-window)
    ;;                  (reusable-frames . t)))
    (pokemacs-layout--apply layout-name)))

;;;###autoload
(defun pokemacs-layout-reset ()
  "Reset the Emacs frame to an empty state."
  (interactive)
  (setq display-buffer-alist nil)
  (delete-other-windows))

;; End:
(provide 'pokemacs-layout)

;;; pokemacs-layout.el ends here
