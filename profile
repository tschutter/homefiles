# Executed by sh/bash/ksh for login shells.
# This file should only contain commands understood by sh.
# Aliases are not inherited by subshells and therefore should not be
# defined here.
# Environment variables that begin with an underscore are intended
# only for use by .profile and .${SHELL}rc

# Determine the OS.  [Linux, Cygwin, Darwin, OpenBSD, SunOS]
_UNAME=$(uname)
if [ "${_UNAME%%-*}" = "CYGWIN_NT" ]; then
    _WINVER=${_UNAME#*-}  # Strip prefix
    _WINVER=${_WINVER%-*}  # Strip suffix
    _WINVER_MAJOR=${_WINVER%.*}
    _WINVER_MINOR=${_WINVER#*.}
    _UNAME="Cygwin"
fi
export _UNAME

path_append() {
    # Append a dir to PATH if dir exists and it is not in PATH.
    INPATH=`echo ${PATH} | grep -E "(^|:)$1(:|\$)"`
    if [ -d "$1" -a -z "${INPATH}" ]; then
        PATH="${PATH}:$1"
    fi
}

path_prepend() {
    # Prepend a dir to PATH if dir exists and it is not in PATH.
    INPATH=`echo ${PATH} | grep -E "(^|:)$1(:|\$)"`
    if [ -d "$1" -a -z "${INPATH}" ]; then
        PATH="$1:${PATH}"
    fi
}

# Set initial path.
if [ "${_UNAME}" = "OpenBSD" ]; then
    # From /etc/skel/.profile
    PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.
    export PATH HOME TERM
fi

# Add sysadm dirs to path.
if [ "${_UNAME}" != "Cygwin" ]; then
    path_prepend /sbin
    path_prepend /usr/sbin
fi

# Prepend user's bin directory to the path.
path_prepend ${HOME}/bin

# Prepend user's .local/bin directory to the path.
# See https://www.python.org/dev/peps/pep-0370/.
path_prepend ${HOME}/.local/bin

# MySQL history.
MYSQL_HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/mysql/history"; export MYSQL_HISTFILE

# Config for Python interactive sessions.
PYTHONSTARTUP=${HOME}/.homefiles/pythonstartup.py; export PYTHONSTARTUP

# Directory for Python utility functions.
PYTHONPATH=${HOME}/.homefiles/lib/python; export PYTHONPATH

if [ "${_UNAME}" = "OpenBSD" ]; then
    # Source directory for pkg_add.
    MIRROR=http://mirror.esc7.net/pub/OpenBSD
    PKG_PATH=${MIRROR}/%v/packages/%a/; export PKG_PATH
fi

# Use less(1) for a pager.
# Do not set the LESS environment variable, because that also applies to PAGER.
if command -v less >/dev/null 2>&1; then
    export PAGER="less"
fi

# Make less more friendly for non-text input files, see lesspipe(1).
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Tell gpg-agent(1) which tty to use.
export GPG_TTY=$(tty)

# Enable multi-select using tab and shift-tab for fzf(1).
export FZF_DEFAULT_OPTS='--multi'

# When logged in via SSH and running a gpg operation which asks for a
# key's passphrase, request that the password be entered directly in
# the terminal rather than opening an external passphrase window
# (pinentry).
# if [[ -n "$SSH_CONNECTION" ]] ;then
#     export PINENTRY_USER_DATA="USE_CURSES=1"
# fi
# Which does not work.  For now, added this to ~/.gnupg/gpg-agent.conf:
#   pinentry-program /usr/bin/pinentry-curses

# Prevent "Couldn't connect to accessibility bus" warnings when
# starting some apps such as evince.  See:
# http://askubuntu.com/questions/227515/terminal-warning-when-opening-a-file-in-gedit
# https://bugzilla.redhat.com/show_bug.cgi?id=889690
# https://mail.gnome.org/archives/gnome-accessibility-devel/2011-June/msg00006.html
export NO_AT_BRIDGE=1

# Include rust package manager environment if it exists.
if [ -r "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
fi

# Include .profile-local if it exists.
if [ -r "$HOME/.profile-local" ]; then
    . "$HOME/.profile-local"
fi

# Include shell rc file.
#
# Note that using ${SHELL} here doesn't seem to work. homefiles rev
# 4a64d5 broke logins on XUbuntu.
if [ -n "$BASH_VERSION" ]; then
    if [ -r "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
elif [ -n "$KSH_VERSION" ]; then
    export ENV="$HOME/.kshrc"
fi
