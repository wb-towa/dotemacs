;;; java-color-mode.el --- show java.awt.Color colors live in the buffer
;; This is free and unencumbered software released into the public domain.

;;; Commentary:
;; http://i.imgur.com/BVa0jkb.gif
;; Requires Emacs 24.3 are higher.
;;; Code:

(require 'cl-lib)

(defvar java-color-regexp
  (let* ((num "\\(\\(?:0x\\)?[0-9a-f]+\\)")
         (args (format "(\\s *%s\\s *,\\s *%s\\s *,\\s *%s\\s *)" num num num)))
    (concat "new\\s \\(?:java\\.awt\\.\\)?Color\\s *" args))
  "Regexp matching java.awt.Color expressions.")

(defvar-local java-color-overlays ()
  "List of java-color-mode overlays in the current buffer.")

(defun java-color-make-overlays ()
  "Put a color overlay over every java.awt.Color."
  (save-excursion
    (setf (point) (point-min))
    (while (search-forward-regexp java-color-regexp nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (overlay (make-overlay beg end)))
        (push overlay java-color-overlays)
        (cl-loop for i from 1 to 3
                 for match = (match-string i)
                 for value = (read (replace-regexp-in-string "0x" "#x" match))
                 collect (format "%02x" value) into values
                 finally (let ((color (apply #'concat "#" values)))
                           (overlay-put overlay
                                        'face `((:background ,color)))))))))

(defun java-color-clear-overlays ()
  "Remove all java-color-mode overlays in the buffer."
  (while java-color-overlays
    (delete-overlay (pop java-color-overlays))))

(defun java-color-function (&optional _pos _beg _end)
  "Function to run after every change."
  (java-color-clear-overlays)
  (java-color-make-overlays))

(define-minor-mode java-color-mode
  "Show java.awt.Color colors right in the buffer."
  :lighter " jcolor"
  (if java-color-mode
      (progn
        (add-hook 'after-change-functions #'java-color-function nil t)
        (java-color-function))
    (java-color-clear-overlays)
    (remove-hook 'after-change-functions #'java-color-function t)))

(provide 'java-color-mode)

;;; java-color-mode.el ends here
