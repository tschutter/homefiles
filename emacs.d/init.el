;;; Some packages installed in ~/.emacs.d/lisp/ are single files while others
;;; are placed inside their own sub-directories.
;;; Prepend ~/.emacs.d/lisp/ and all of it's subdirectories to load-path.
;;; See http://www.emacswiki.org/emacs/LoadPath#AddSubDirectories
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;;;; Editor behavior

;;; We don't need to see the startup message.
(setq inhibit-startup-message t)

;;; Turn off the toolbar.
(tool-bar-mode -1)

;;; Turn off blinking cursor.
(blink-cursor-mode 0)

;;; Uniquely indentify buffers
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; Enable menu of recently opened files.
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(recentf-mode 1)

;;; Display line numbers.
(require 'linum)
(setq linum-format "% 5d")  ;Always 5 columns
(global-linum-mode)         ;All buffers

;;; Put column number in mode line.
(column-number-mode 1)

;;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;;; Vertical motion starting at EOL line keeps to EOL.
(setq track-eol t)

;;; Indentation should insert spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; If we do see tabs, they are 4 chars wide.
(setq default-tab-width 4)

;;; Scroll one line at a time instead of paging.
;;; Paging is what PgUp and PgDn are for.
(setq scroll-conservatively 100)

;;; Remember and restore point location after PgUp,PgDn
(setq scroll-preserve-screen-position t)

;;; Default to filename at point for C-x C-f.
(require 'ffap)
(ffap-bindings)
(setq ffap-machine-p-known 'accept)  ;No pinging
(setq ffap-c-path
      (list
       (getenv "SRC_TREE")
       "~/src/pxpoint"
       "~/src/webservices"
       "/usr/include"
       "/usr/local/include"
       "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
       "C:/Program Files/Java/jdk1.6.0_20/include"))

;;; Advanced highlighting of matching parenthesis.
(require 'mic-paren)
(paren-activate)
(setq paren-sexp-mode t)  ; Always highlight the whole s-expression.
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (paren-toggle-matching-quoted-paren 1)
                      (paren-toggle-matching-paired-delimiter 1))))
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (paren-toggle-open-paren-context 1))))

;;; Enable switching between buffers using substrings.
(iswitchb-mode 1)

;;; Put all backups in one directory.
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
(defun make-backup-file-name (file)
  (concat "~/.emacs.d/backups/" (file-name-nondirectory file) "~"))
(setq auto-save-list-file-prefix "~/.emacs.d/backups/")

;;; Provide an easy goto-line (^c-g).
(global-set-key "\C-cg" 'goto-line)

;;; Save and restore point (F3, F4).
(define-key global-map [\C-f3] '(lambda () (interactive) (point-to-register 33)))  ;^F3 Save
(define-key global-map [f3] '(lambda () (interactive) (jump-to-register 33)))      ;F3 Restore
(define-key global-map [\C-f4] '(lambda () (interactive) (point-to-register 34)))  ;^F4 Save
(define-key global-map [f4] '(lambda () (interactive) (jump-to-register 34)))      ;F4 Restore

;;; Insert datetime into current buffer (^c-d).
(defun insert-date ()
  "Inserts date time string into current buffer."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d %H:%M:%S")))
(global-set-key (kbd "C-c d") 'insert-date)


;;;; Printing
(require 'ps-print)
(setq ps-number-of-columns 2)
(setq ps-landscape-mode t)
(setq ps-line-number t)
(setq ps-print-color-p nil)
(setq ps-print-header nil)
(setq lpr-page-header-switches '("-F" "--length=61" "--indent=4"))


;;;; Source code manipulation

;;; Display and cleanup bogus whitespace.
(require 'whitespace)
(setq whitespace-style '(face tabs trailing indentation empty))
(global-whitespace-mode 1)
(setq whitespace-action '(auto-cleanup))

;;; Line wrap regions, function definitions, and function calls.
(defun region-line-wrap ()
  "Line wrap region, breaking at commas"
  (let ((newline (if (eq major-mode (quote vbnet-mode)) " _\n" "\n")))
    (save-excursion
      (save-restriction
        (narrow-to-region (mark) (point))
        (goto-char (point-min))
        (forward-char)
        (if (not (looking-at newline))
            (insert newline))
        (while (re-search-forward "," (point-max) t)
          (if (not (looking-at newline))
              (insert newline)))
        (goto-char (point-max))
        (backward-char)
        (beginning-of-line)
        (if (not (looking-at " *)$"))
            (progn
              (goto-char (point-max))
              (backward-char)
              (insert newline)))))
    (indent-region (mark) (point) nil)))
(defun function-line-wrap ()
  "Line wrap function call or function definition"
  (interactive)
  (let ((original-point (point)))
    (save-excursion
      (mark-defun)
      (let ((defun-begin (point)) (defun-end (mark)))
        ;; Try the sexp that we are inside of.
        (goto-char original-point)
        (backward-up-list)
        (if (looking-at "(")
            (progn
              (set-mark (point))
              (forward-list)
              (region-line-wrap))
          ;; Try the sexp before original-point.
          (goto-char original-point)
          (re-search-backward ")" defun-begin)
          (backward-up-list)
          (set-mark (point))
          (forward-list)
          (region-line-wrap))))))
(define-key global-map [f2] '(lambda () (interactive) (function-line-wrap)))


;;;; Python

;;; Use ^c-^w or ^c-^v
(setq py-pychecker-command "pep8")
(setq py-pychecker-command-args (quote ("--repeat")))
(setq python-check-command "pep8")

;;;; Desktop (must be last)
(desktop-save-mode 1)
