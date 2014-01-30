; mg (emacs-like text editor) config file

auto-fill-mode
set-fill-column 72
line-number-mode ; display in modeline

; Prevent UTF-8 left quotes pasted from gcc error messages from
; screwing everything up.  With meta-key-mode disabled they are pasted
; as \342\200\230 but that is better than translating them as cursor
; move commands.
meta-key-mode 0

; Use "M-x describe-bindings" to see current bindings.
global-set-key "\e[1;5C" forward-word
global-set-key "\e[1;5D" backward-word
global-set-key "\e[5C" forward-word
global-set-key "\e[5D" backward-word
global-set-key "\e\e[C" forward-word
global-set-key "\e\e[D" backward-word
global-set-key "\^[[3~" delete-char
global-set-key "\^cg" goto-line
global-set-key "\^hk" describe-key-briefly
global-set-key "\^z" undo
