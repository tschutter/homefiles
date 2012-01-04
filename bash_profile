# Executed by bash for login shells.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# Include .bashrc if it exists.
if [ -r "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# Set PATH so it includes user's private bin if it exists.
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
