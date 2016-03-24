# Executed by ksh(1) for non-login shells.

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

# Source alias, function, and prompt definitions for Bourne-derived shells.
if [ -f ~/.bournerc ]; then
    . ~/.bournerc
fi

# Save history files in ~/.cache/ksh/history.
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/ksh/history"

# Display more than the default 16 last commands.
alias history="fc -l -r -1 -1024"

# Emacs history editing.
set -o emacs

# Subset of csh(1)-style history editing using the `!' character.
set -o csh-history

# History navigation.
bind '^XH'=beginning-of-line # home
bind '^XF'=end-of-line # end
bind '^[[3'=prefix-2 2>/dev/null # del prep on OpenBSD < 5.2
bind '^[[3~'=delete-char-forward # del

# Use fasd(1) to maintain a jump-list of directories.
# https://github.com/clvv/fasd
_FASD_DATA="${XDG_CACHE_HOME:-$HOME/.cache}/fasd"
_FASD_AWK=awk
MANPATH=${MANPATH}:${HOME}/.homefiles/man
. ${HOME}/.homefiles/bin/fasd
eval "$(fasd --init auto)"
_PS1_FASD='$(_fasd_ps1_func)'
# Re-select the default prompt string to pickup _PS1_FASD.
ps1-default

# Source local kshrc.
if [ -f ~/.kshrc-local ]; then
    . ~/.kshrc-local
fi

# Local Variables:
# mode:sh
# End:
