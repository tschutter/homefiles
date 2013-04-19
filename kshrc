# Executed by ksh(1) for non-login shells.

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

# Source alias, function, and prompt definitions for Bourne-derived shells.
if [ -f ~/.bournerc ]; then
    . ~/.bournerc
fi

# Save history files in ~/.var/.
HISTFILE=${HOME}/.var/ksh_history

# Emacs history editing.
set -o emacs

# Subset of csh(1)-style history editing using the `!' character.
set -o csh-history

# History navigation.
bind '^XH'=beginning-of-line # home
bind '^XF'=end-of-line # end
bind '^[[3'=prefix-2 2>/dev/null # del prep on OpenBSD < 5.2
bind '^[[3~'=delete-char-forward # del

# Source local kshrc.
if [ -f ~/.kshrc-local ]; then
    . ~/.kshrc-local
fi

# Local Variables:
# mode:sh
# End:
