# Executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source alias, function, and prompt definitions for Bourne-derived shells.
if [ -f ~/.bournerc ]; then
    . ~/.bournerc
fi

# Number of trailing directory components to retain when expanding \w in PS1.
# Note that PROMPT_DIRTRIM is a bashism.
PROMPT_DIRTRIM=4

# Don't save duplicate lines in the history list.
# Do save lines that begin with a space; I sometimes accidently do that.
HISTCONTROL=ignoredups

# Append to the history file, don't overwrite it.
shopt -s histappend

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Platform specific definitions.
if [ "${_UNAME}" = "CYGWIN_NT" ]; then
    # Ignore case while completing
    set completion-ignore-case on

    # Run a batch file.
    launch() {
        ${SYSTEMROOT}/system32/cmd.exe /c start `/usr/bin/cygpath.exe --windows $1`
    }
fi

# Add an "alert" alias for long running commands.  Use like so:
#   long-running-command; alert
# "history" is bash-specific.
[ -x /usr/bin/notify-send ] && alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Enable programmable completion features.
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Source local bashrc.
if [ -f ~/.bashrc-local ]; then
    . ~/.bashrc-local
fi

# Local Variables:
# mode:sh
# End:
