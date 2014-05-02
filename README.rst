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

#. Settings Manager -> Window Manager -> Keyboard

   * In the "Window operations menu" row, double click on "<Alt>space" and then click on the "Clear" button.

     Alt-space is reserved for just-one-space in Emacs.

Linux Mint
~~~~~~~~~~

#. Control Center -> Keyboard Shortcuts -> Window Management

   * In the "Activate the window menu" row, select "Alt+Space" and press backspace.

     Alt-space is reserved for just-one-space in Emacs.
