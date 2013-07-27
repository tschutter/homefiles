Homefiles
=========

Usage
-----

Initial checkout::

    git clone --recursive git://github.com/tschutter/homefiles.git $HOME/.homefiles
    $HOME/.homefiles/install.py
    $HOME/bin/install-essentials
    $HOME/.homefiles/install.py

Update::

    cd $HOME/.homefiles
    git pull
    git submodule update --init --recursive
    ./install.sh
    install-essentials

----------------------------------------------------------------------

Manual Configuration
--------------------

Some configuration is difficult to do via scripts.

XUbuntu / xfce
~~~~~~~~~~~~~~

#. Settings -> Settings Manager -> Window Manager -> Keyboard

   * In the "Window operations menu" row, select "<Alt>space" and click on the "Clear" button.

     Alt-space is reserved for just-one-space in Emacs.

Linux Mint
~~~~~~~~~~

#. Control Center -> Keyboard Shortcuts -> Window Management

   * In the "Activate the window menu" row, select "Alt+Space" and press backspace.

     Alt-space is reserved for just-one-space in Emacs.

TO DO
-----

* Win7 do not merge toolbar icons.

* Windows do not hide unused tooltray icons.
