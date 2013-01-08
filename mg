; mg (emacs-like text editor) config file

auto-fill-mode
set-fill-column 72
line-number-mode ; display in modeline

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
