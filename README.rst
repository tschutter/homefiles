Homefiles
=========

Initial System Preparation
--------------------------

OpenBSD::

    MIRROR=http://mirror.esc7.net/pub/OpenBSD/
    PKG_PATH=${MIRROR}`uname -r`/packages/`uname -m`/; export PKG_PATH
    doas pkg_add -i -z git python-3*
    echo "XTerm*loginShell: true" >> ~/.Xdefaults

Debian, Ubuntu::

    sudo apt-get install git python3

Usage
-----

Initial checkout::

    git clone git://github.com/tschutter/homefiles.git $HOME/.homefiles
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

Vivaldi
~~~~~~~

#. Vivaldi Menu -> Settings -> Sync

   * Enter username and password.

Firefox
~~~~~~~

#. Install Bluhell Firewall, the lightweight Ad-Blocker and
   Tracking/Privacy Protector.  Use the Firefox Add-ons menu.

----------------------------------------------------------------------

Other Optional Setup
--------------------

#. Generate ssh private/public key pair::

    ssh-keygen

#. Add ssh public key to Bitbucket and GitHub.

#. Install pcloud app.

   * Download `pCloud Drive
     <https://www.pcloud.com/download-free-online-cloud-file-storage.html>`_
     and save it to ~/bin/pcloud.  Make it executable.

   * Run ``pcloud`` and login.

   * Add a sync between ``~/org`` and ``pcloudDrive/org``.

#. Copy GPG secret keys.  Run on source machine::

    gpg --export-secret-keys > gpg-secrets.txt

   Copy ``gpg-secrets.txt`` to new machine and then import::

    gpg --import gpg-secrets.txt

#. Setup pass database.  Read instructions in
   ``.password-store/0-README.txt``.

----------------------------------------------------------------------

Troubleshooting
---------------

#. Pressing Ctrl-Shift in a terminal window displays a yellow box
   containing "ISO 14755 mode".

   This a "feature" of xterm.  Use xfce4-terminal instead.  Check
   Settings Manager -> System -> Default Applications -> Utilities ->
   Terminal Emulator.
