Homefiles
=========

Initial System Preparation
--------------------------

OpenBSD::

    MIRROR=http://mirror.esc7.net/pub/OpenBSD/
    PKG_PATH=${MIRROR}`uname -r`/packages/`uname -m`/; export PKG_PATH
    doas pkg_add -i -z git python-3*

Debian, Ubuntu::

    sudo apt-get install git python3

Usage
-----

Initial checkout::

    git clone --recursive git://github.com/tschutter/homefiles.git $HOME/.homefiles
    cd $HOME/.homefiles
    ./install.py
    bin/install-essentials
    ./install.py

Update::

    cd $HOME/.homefiles
    git pull
    ./install.py
    install-essentials

----------------------------------------------------------------------

Manual Configuration
--------------------

Some configuration is difficult to do via scripts.

XUbuntu / xfce
~~~~~~~~~~~~~~

#. Settings Manager -> Other -> Keyboard Input Methods

   * In the "Next input method" row, click on the "..." button and select "Delete".

     Alt-space is reserved for set-mark-command in Emacs.

   * Uncheck "Show icon on system tray".

     Why show an icon that only lets you select English?

#. Settings Manager -> Personal -> Window Manager Tweaks -> Accessibility

   * Uncheck "Automatically tile windows when moving toward the screen
     edge".

     Why should a window resize when you move it?  Especially to some
     odd half-screen size?

#. Right click on the clock in the top panel and select Properties.

   * Set the Format to "Custom Format" and set the custom format
     string to "%a, %Y-%m-%d %H:%M".

     See `ISO 8601 <http://xkcd.com/1179/>`_ for the reason why.

Linux Mint
~~~~~~~~~~

#. Control Center -> Keyboard Shortcuts -> Window Management

   * In the "Activate the window menu" row, select "Alt+Space" and press backspace.

     Alt-space is reserved for just-one-space in Emacs.

Firefox
~~~~~~~

#. Install KeySnail to get Emacs keybindings in Firefox::

    wget https://github.com/mooz/keysnail/raw/master/keysnail.xpi
    firefox keysnail.xpi

----------------------------------------------------------------------

Troubleshooting
---------------

#. Pressing Ctrl-Shift in a terminal window displays a yellow box
   containing "ISO 14755 mode".

   This a "feature" of xterm.  Use xfce4-terminal instead.  Check
   Settings Manager -> Preferred Applications -> Utilities -> Terminal
   Emulator.
