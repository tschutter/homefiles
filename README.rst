Homefiles
=========

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
    git submodule update --init --recursive
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

   * Uncheck "Show icon on system tray".  Why show an icon that only lets you select English?

#. Right click on the clock in the top panel and select Properties.

   * Set the Format to "Custom Format".

   * Set the custom format string to "%a, %Y-%m-%d %H:%M".

Linux Mint
~~~~~~~~~~

#. Control Center -> Keyboard Shortcuts -> Window Management

   * In the "Activate the window menu" row, select "Alt+Space" and press backspace.

     Alt-space is reserved for just-one-space in Emacs.

TO DO
-----

* Win7 do not merge toolbar icons.

* Windows do not hide unused tooltray icons.
