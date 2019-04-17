# Executed by ksh(1) for non-login shells.

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

# Source alias, function, and prompt definitions for Bourne-derived shells.
if [ -f ~/.bournerc ]; then
    . ~/.bournerc
fi

# Don't save duplicate lines in the history list.
# Do save lines that begin with a space by not including ignorespace;
# many times there are leading spaces when pasting a group of
# commands.  OpenBSD 6.2 and newer.
HISTCONTROL=ignoredups

# Save history files in ~/.cache/ksh/history.
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/ksh/history"

# Disable XON/XOFF (ctrl-s/ctrl-q) flow control.
stty -ixon

# Display more than the default 16 last commands.
unalias history
function history {
    if [ "$1" = "" ]; then
        fc -l -r -1 -1028
    else
        fc -l -r -1 -"$1"
    fi
}

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

# KSH support for git-prompt.
# https://gist.github.com/qbit/5483415
. ~/.homefiles/ksh-git-prompt.sh

# The __git_ps1() function defined by git-completion.bash has a number
# of configuration settings.
# Show unstaged (*) and staged (+) changes next to the branch name.
GIT_PS1_SHOWDIRTYSTATE=t
# Show stashed objects exist ($) next to the branch name.
GIT_PS1_SHOWSTASHSTATE=t
# Show untracked files exist (%) next to the branch name.
GIT_PS1_SHOWUNTRACKEDFILES=t
# Show the difference between HEAD and its upstream.
GIT_PS1_SHOWUPSTREAM="auto"

# Source local kshrc.
if [ -f ~/.kshrc-local ]; then
    . ~/.kshrc-local
fi

# Local Variables:
# mode:sh
# End:
