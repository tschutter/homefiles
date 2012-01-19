;;;; Snippets from old .emacs files.

;todo; ;;;; Live mode (similar to tail -f) which is specified by dump() output
;todo; (load-library "live-mode")
;todo;

;;;   ;; find-function setup
;;;   (autoload 'find-function "find-function" nil t)
;;;

;todo; ;;; Editor behavior: turn on font-lock for all modes
;todo; (global-font-lock-mode t)
;todo;

;;;   (load-library "mydiary")
;;;
;;;   ;; crypt++
;;;   ;(setq crypt-encryption-type 'gpg)
;;;   ;(crypt-rebuild-tables)
;;;
;;;   ;(quietly-read-abbrev-file)
;;;   ;(add-hook 'c-mode-hook '(lambda () (abbrev-mode 1)))
;;;

;;;   ;; tell the default compile buffer to be n lines high
;;;   ;(setq compilation-window-height 11)
;;;
;;;   ;;(win32-select-font) ;C-x C-e this to select a font
;;;   ;; italic fonts follow
;;;   ;(set-default-font "-*-Courier New-normal-r-*-*-15-90-*-*-c-*-*-ansi-")
;;;   ;(set-face-font 'italic "-*-Courier New-normal-i-*-*-15-90-*-*-c-*-*-ansi-")
;;;   ;(set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-15-90-*-*-c-*-*-ansi-")
;;;
;;;   ;(set-default-font "-*-Andale Mono-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
;;;
;;;   ;;; CSharp mode
;;;   ;;; http://www.emacswiki.org/emacs/CSharpMode
;;;   ; Currently broken as of 2010-06-28, search for "23" in wiki.
;;;   ;(require 'cc-mode)
;;;   ;(add-to-list 'load-path (expand-file-name "~/.emacs.d/CsharpToolsForEmacs"))
;;;   ;(require 'csharp-mode)
;;;   ;(setq auto-mode-alist
;;;   ;   (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;todo; ;;; Editor behavior: turn on word wrap in text-mode
;todo; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;todo;

;todo; ;;; Editor behavior: Use MS-Windows cut and paste style
;todo; (pc-selection-mode)
;todo;
;todo; ;;; Editor behavior: highlight QWER keyword
;todo; (font-lock-add-keywords
;todo;  'c-mode
;todo;  '(("\\<\\(QWER\\)" 1 font-lock-warning-face t)))
;todo; (font-lock-add-keywords
;todo;  'c++-mode
;todo;  '(("\\<\\(QWER\\)" 1 font-lock-warning-face t)))
;todo;

;todo; ;;; Editor behavior: provide index in C mode
;todo; (add-hook 'c-mode-common-hook '(lambda () (imenu-add-to-menubar "Index")))
;todo;
;todo; ;;;; Editor tools
;todo;
;todo; ;;; Editor tools: GUD enable tooltips in GNU Emacs 21.
;todo; (when (featurep 'tooltip)
;todo;   (defvar tooltip-gud-tips-p)
;todo;   (setq tooltip-gud-tips-p t))
;todo;
;todo; ;;; Editor tools: describe PC-lint message
;todo; (defun describe-lint-message (num)
;todo;   "Display documentation of a PC-lint message."
;todo;   (interactive "nPC-lint message number: ")
;todo;   (find-file-read-only-other-window
;todo;    (concat (getenv "SRC_TREE") "/common/pclint/msg.txt"))
;todo;   (widen)
;todo;   (goto-char (point-min))
;todo;   (re-search-forward (concat "^" (int-to-string num) " "))
;todo;   (beginning-of-line)
;todo;   (set-mark (point))
;todo;   (forward-line)
;todo;   (re-search-forward "^[1-9]" (point-max) 1)
;todo;   (beginning-of-line)
;todo;   (forward-line -1)
;todo;   (narrow-to-region (mark) (point))
;todo;   (exchange-point-and-mark))
;todo; (global-set-key "\C-he" 'describe-lint-message)
;todo; (define-key menu-bar-help-menu [describe-lint-message]
;todo;   '("Describe PC-lint Message" . describe-lint-message))
;todo;
;todo; ;;; Editor tools: find all header files included by a source file
;todo; (require 'c-includes)
;todo; (setq c-includes-path
;todo;       (list
;todo;        (getenv "SRC_TREE")
;todo;        "/usr/include"
;todo;        "/usr/include/sys"
;todo;        "C:/Program Files/Microsoft Visual Studio .NET/Vc7/include"
;todo;        "C:/Program Files/Microsoft Visual Studio .NET/Vc7/FrameworkSDK/include"
;todo;        "C:/j2sdk1.4.0/include"))
;todo; (define-key menu-bar-tools-menu [c-includes-current-file]
;todo;   '("Show C Includes" . c-includes-current-file))
;todo;
;todo; ;;;; PRA source code style
;todo;
;todo; ;;; PRA source code style: C and C++
;todo; (require 'cc-vars)
;todo; (setq c-basic-offset 2)
;todo; (defun adjust-indentation-style ()
;todo;   ;; use C-c C-s to determine the syntactic symbol
;todo;   ;; use C-h v c-offsets-alist to see current setting for the syntactic symbol
;todo;   ;; change it here
;todo;   (c-set-offset 'arglist-intro '+)
;todo;   (c-set-offset 'case-label '+)
;todo;   (c-set-offset 'arglist-close '0)
;todo; )
;todo; (add-hook 'c-mode-hook 'adjust-indentation-style)
;todo; (add-hook 'c++-mode-hook 'adjust-indentation-style)
;todo; (add-hook 'java-mode-hook 'adjust-indentation-style)
;todo;
;todo; ;;; PRA source code style: IDL
;todo; (setq auto-mode-alist (cons '("\\.idl\\'" . c-mode) auto-mode-alist))
;todo;
;todo; ;;; PRA source code style: DOC++
;todo; (setq auto-mode-alist (cons '("\\.dxx\\'" . c-mode) auto-mode-alist))
;todo;
;todo; ;;; PRA source code style: code fixup
;todo; (require 'pra-code-fixup)
;todo;
;todo; ;;;; MS Windows
;todo;
;todo; (if (eq system-type 'windows-nt)
;todo;     ;; define findstr command for Windows
;todo;     (defun findstr ()
;todo;       "Search for files using the Windows findstr command."
;todo;       (interactive)
;todo;       (let ((grep-command "findstr /n /p /s "))
;todo;         (call-interactively 'grep))))
;todo;
;todo; ;;;; VB .NET
;todo; (autoload 'vbnet-mode "vbnet-mode" "Visual Basic mode." t)
;todo; (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
;todo;                                  vbnet-mode)) auto-mode-alist))
;todo; (setq vbnet-mode-indent 2)
;todo; (setq vbnet-wild-files "*.frm *.bas *.cls *.vb")
;todo;
;;;   ;; windows printing from
;;;   ;; http://www.emacswiki.org/cgi-bin/wiki/PrintingFromEmacs
;;;   ;(setenv "GS_LIB"
;;;   ;    "c:/Program Files/gs/gs8.61/lib;c:/Program Files/gs/gs8.61/fonts"
;;;   ;    )
;;;   ;(setq ps-lpr-command "c:/Program Files/gs/gs8.61/bin/gswin32c.exe")
;;;   ;(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
;;;   ;(setq ps-printer-name t) ; t = pass empty printer name to ps-lpr-command
;;;

;todo; ;;;; HTML
;todo;
;todo; ;;; HTML: editing (html-helper-mode.el)
;todo; (require 'html-helper-mode)
;todo; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;todo; (add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))
;todo; (add-to-list 'auto-mode-alist '("\\.htm$" . html-helper-mode))
;todo; (setq html-helper-use-expert-menu t)
;todo; (setq html-helper-do-write-file-hooks nil)
;todo; (add-hook 'html-mode-hook
;todo;           '(lambda ()
;todo;              (turn-on-auto-fill)
;todo;              (turn-on-font-lock)))
;todo;
;;;   ;; Add to expert menu (only effects html-helper-mode beta >= 02-May-1995).
;;;   ;; `compile' bound to key in ~/.emacs_dickow.
;;;   (global-set-key "\C-cc" 'compile)
;;;   ;(setq html-helper-user-menu '(["Check HTML of this buffer" compile t]))
;;;   (add-hook 'html-helper-mode-hook
;;;             '(lambda ()
;;;                (make-local-variable 'compile-command)
;;;                (make-local-variable 'ispell-skip-sgml-tags)
;;;                (setq compile-command
;;;                      (concat "weblint -x Netscape " buffer-file-name))
;;;                (setq ispell-skip-sgml-tags t)))
;;;
;todo; ;;;; LaTeX
;todo;
;todo; ;;; LaTeX: enable RefTeX for all LaTeX files
;todo; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;todo; (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
;todo;
;todo; ;;;; JDEE
;todo;
;todo; ;;;; EIEIO
;todo; ;(add-to-list 'load-path (expand-file-name "~/emacs_pra/eieio-0.17"))
;todo; ;
;todo; ;;;; speedbar
;todo; ;(add-to-list 'load-path (expand-file-name "~/emacs_pra/speedbar-0.14beta4"))
;todo; ;
;todo; ;;;; Semantic
;todo; ;(add-to-list 'load-path (expand-file-name "~/emacs_pra/semantic-1.4.4"))
;todo; ;(setq semantic-load-turn-everything-on t)
;todo; ;(require 'semantic-load)
;todo; ;
;todo; ;;;; elib
;todo; ;(add-to-list 'load-path (expand-file-name "~/emacs_pra/elib-1.0"))
;todo; ;
;todo; ;;;; JDEE
;todo; ;(add-to-list 'load-path (expand-file-name "~/emacs_pra/jde-2.3.2/lisp"))
;todo; ;(require 'jde)
;todo;

;todo; (defun include-macro-wrap ()
;todo;   "Macro wrap include in if/endif"
;todo;   (interactive)
;todo;   (save-excursion
;todo;     (beginning-of-line)
;todo;     (if (not (looking-at "# *include <"))
;todo;         (ding)
;todo;       ; Indent the "include" if needed.
;todo;       (if (looking-at "#include <")
;todo;           (progn
;todo;             (forward-char)
;todo;             (insert " ")))
;todo;       ; Get the filename
;todo;       (end-of-line)
;todo;       (backward-char)
;todo;       (re-search-backward "[/<]")
;todo;       (re-search-forward "\\([a-zA-Z_]+\\)\.\\([a-zA-Z_]+\\)")
;todo;       (let ((wrapper (concat "#if !defined(_"
;todo;                              (upcase (match-string 1))
;todo;                              "_"
;todo;                              (upcase (match-string 2)) "_)")))
;todo;         (beginning-of-line 0)
;todo;         (if (not (looking-at wrapper))
;todo;             (progn
;todo;               (beginning-of-line 2)
;todo;               (insert (concat wrapper) "\n")
;todo;               (beginning-of-line 2)
;todo;               (insert "#endif\n")))))))
;todo; (define-key global-map [f6] '(lambda () (interactive) (include-macro-wrap)))

;todo; (add-to-list 'ffap-alist '("\\.idl\\'" . ffap-c-mode))
;todo; (add-to-list 'ffap-alist '("\\.xpm\\'" . ffap-c-mode))
