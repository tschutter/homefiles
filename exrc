" vi config file (not for vim or gvim)

" Use indents of 4 spaces, and have them copied down lines.
set tabstop=4
set shiftwidth=4

" Although autoindent sounds nice, it totally screws up text pasted
" from other apps.  And in vi, I paste more often than creating
" original text.
set noautoindent

" Print the line number in front of each line.
set number

" Show matching brackets.
set showmatch

" Search for text as you enter it.  Only available if the extra_search
" feature was specified at build time.  As of 2013-005-13, Cygwin
" builds vi with extra_search disabled.
if has("searchincr")
    set searchincr
endif

" Highlight text that matches the current search.
if has("hlsearch")
    set hlsearch
endif

" Enable syntax highlighting.
if has("syntax")
    syntax on
endif
