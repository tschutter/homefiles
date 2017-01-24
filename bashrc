# Executed by bash(1) for non-login shells.

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

# Source alias, function, and prompt definitions for Bourne-derived shells.
if [ -f ~/.bournerc ]; then
    . ~/.bournerc
fi

# Number of trailing directory components to retain when expanding \w in PS1.
# Note that PROMPT_DIRTRIM is a bashism.
PROMPT_DIRTRIM=4

# Don't save duplicate lines in the history list.
# Do save lines that begin with a space by not including ignorespace;
# many times there are leading spaces when pasting a group of
# commands.
HISTCONTROL=ignoredups

# Add a timestamp to history output.
export HISTTIMEFORMAT="%Y-%m-%d %H:%M:%S  "

# Save history files in ~/.cache/bash/history.
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/bash/history"

# Append to the history file, don't overwrite it.
shopt -s histappend

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Enable cut-n-paste between shell and X clipboard using Emacs key
# bindings.  Note that M-w needs to be handled by the terminal
# instead, because M-w should copy a region, and only the terminal
# knows about selected regions.
# http://unix.stackexchange.com/questions/18701
# http://stackoverflow.com/questions/994563
# http://stackoverflow.com/questions/8366450
if [[ -n ${DISPLAY} ]]; then
    # These bindings cannot be put in .inputrc because they use the
    # bash-specific $READLINE environment variables.  With these
    # bindings we lose the kill ring concept in bash, but I never use
    # that anyways.  It is more important to have key binding
    # consistency between applications.

    # Kill text from point to end of line and copy to X clipboard.
    _x-kill-line() {
        echo -n "${READLINE_LINE:$READLINE_POINT}" | xsel -b -i
        READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
    }
    bind -m emacs -x '"\C-k": _x-kill-line'

    # Yank X clipboard into bash's line buffer, placing point at end of yanked text.
    _x-yank() {
        CLIP=$(xsel -b -o)
        COUNT=$(echo "$CLIP" | wc -c)
        READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}${CLIP}${READLINE_LINE:$READLINE_POINT}"
        READLINE_POINT=$(($READLINE_POINT + $COUNT - 1))
    }
    bind -m emacs -x '"\C-y": _x-yank'
fi

# Give a bit more information when tmuxinator completes.
function mux {
    command mux $*;
    echo "tmux list-sessions:";
    tmux list-sessions;
}

# Platform specific definitions.
if [ "${_UNAME}" = "Cygwin" ]; then
    # Ignore case while completing
    set completion-ignore-case on
fi

# Add an "alert" alias for long running commands.  Use like so:
#   long-running-command; alert
# "history" is bash-specific.
[ -x /usr/bin/notify-send ] && alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Enable programmable completion features.
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Define completion for directory navigation functions.
_basecd() {
    local cur=${COMP_WORDS[COMP_CWORD]}
    local base=$1
    local len=$((${#base}+1))
    COMPREPLY=( $( compgen -S/ -d ${base}${cur} | cut -b ${len}- ) )
}
_home() { _basecd ${HOME}/; }
complete -o nospace -F _home home
_src() { _basecd ${SRC:-${HOME}/src}/; }
complete -o nospace -F _src src
_u() { _basecd ../; }
complete -o nospace -F _u u
_uu() { _basecd ../../; }
complete -o nospace -F _uu uu
_uuu() { _basecd ../../../; }
complete -o nospace -F _uuu uuu
_uuuu() { _basecd ../../../../; }
complete -o nospace -F _uuuu uuuu

# Use fasd(1) to maintain a jump-list of directories.
# https://github.com/clvv/fasd
_FASD_DATA="${XDG_CACHE_HOME:-$HOME/.cache}/fasd"
MANPATH=${MANPATH}:${HOME}/.homefiles/man
. ${HOME}/.homefiles/bin/fasd
eval "$(fasd --init auto)"

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

# Source local bashrc.
if [ -f ~/.bashrc-local ]; then
    . ~/.bashrc-local
fi

# Local Variables:
# mode:sh
# End:
