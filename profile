# Executed by sh/bash/ksh for login shells.
# This file should only contain commands understood by sh.
# Aliases are not inherited by subshells and therefore should not be defined here.
# Environment variables are intended only for use by .profile and .${SHELL}rc

# Determine the hostname.
_HOSTNAME=`hostname | /usr/bin/tr '[A-Z]' '[a-z]'`  # Convert to lowercase.
_HOSTNAME=${_HOSTNAME%%.*}  # Strip any domain suffix.
export _HOSTNAME

# Determine the OS.  [Linux, CYGWIN_NT, OpenBSD, SunOS]
_UNAME=$(uname)
_UNAME=${_UNAME%%-*} # Strip any version info
export _UNAME

function path_append() {
    # Append a dir to PATH if dir exists and it is not in PATH.
    INPATH=`echo ${PATH} | grep -E "(^|:)$1(:|\$)"`
    if [ -d "$1" -a -z "${INPATH}" ]; then
        PATH="${PATH}:$1"
    fi
}

function path_prepend() {
    # Prepend a dir to PATH if dir exists and it is not in PATH.
    INPATH=`echo ${PATH} | grep -E "(^|:)$1(:|\$)"`
    if [ -d "$1" -a -z "${INPATH}" ]; then
        PATH="$1:${PATH}"
    fi
}

# Set initial path.
if [ "${_UNAME}" = "OpenBSD" ]; then
    PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.
    export PATH HOME TERM
fi

# Add sysadm dirs to path.
if [ "${_UNAME}" != "CYGWIN_NT" ]; then
    path_prepend /sbin
    path_prepend /usr/sbin
fi

# Prepend user's bin directory to the path.
path_prepend ${HOME}/bin

# Set the prompt.
case "$TERM" in
    xterm*|rxvt*)
        # If this is an xterm set the title to host:dir
        PS1='\e]0;${_HOSTNAME}:${PWD#${HOME}/}\a${_HOSTNAME}:\w\$ '
        ;;
    *)
        PS1='${_HOSTNAME}:\w\$ '
        ;;
esac

# Define environment variables.
if [ "${_UNAME}" = "OpenBSD" ]; then
    PKG_PATH=ftp://ftp3.usa.openbsd.org/pub/OpenBSD/5.0/packages/amd64/; export PKG_PATH
fi
[ -x "/usr/bin/less" ] && LESS="--quit-at-eof --long-prompt"; export LESS

# Make less more friendly for non-text input files, see lesspipe(1).
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Include .profile-local if it exists.
if [ -r "$HOME/.profile-local" ]; then
    . "$HOME/.profile-local"
fi

# If running bash, include .bashrc if it exists.
if [ -n "$BASH_VERSION" ]; then
    if [ -r "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# If running ksh, include .kshrc if it exists.
if [ -n "$KSH_VERSION" ]; then
    export ENV="$HOME/.kshrc"
fi
