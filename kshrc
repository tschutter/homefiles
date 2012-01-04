# Executed by ksh for interactive shells.
# This file should contain ksh-only commands.

# Emacs history editing.
set -o emacs

# Function definitions.
function _cdhelper() {
    CMD="cd $1"
    shift
    for ARG in $*; do
        CMD=${CMD}/${ARG}
    done
    eval ${CMD}
}
function home() {
    _cdhelper ${HOME} $*
}
function src() {
    _cdhelper ${SRC} $*
}
function u() {
    _cdhelper .. $*
}
function uu() {
    _cdhelper ../.. $*
}
function uuu() {
    _cdhelper ../../.. $*
}
function uuuu() {
    _cdhelper ../../../.. $*
}

# Alias definitions.
alias dir="ls -la"
