homefiles Design
================

The requirements include:

1. Multiplatform support.  Ubuntu, OpenBSD, Cygwin, etc.

2. Easy to deploy to new hosts.

3. Easy to determine which files are part of homefiles, and which are
   not.

Considered but rejected:

1. Branches for different platforms.  This makes it difficult to
   compare how files are the same or different between platforms.

.profile, .bashrc, .kshrc Design
--------------------------------

The requirements include:

1. Multiplatform support.  Ubuntu, OpenBSD, Cygwin, etc.

2. Multishell support.  Bourne (sh), Bourne-again (bash), and Korn
   (ksh).

Restrictions imposed by tools:

1. For shells in the Bourne family (sh, bash, ksh), the .profile file
   is only read by login shells.

2. For shells in the Bourne family, the .${SHELL}rc files are only
   read by non-login shells.

3. Environment variables are inherited by subshells (if exported).

4. Functions and aliases cannot be inherited by or exported to
   subshells.

5. Function and alias definitions can be defined in a POSIX standard
   way so that they can be shared by shells in the Bourne family.

6. The PS1 environment variable is overwritten by /etc/bash.bashrc on
   Ubuntu.  This means that we cannot define it in .profile.

Therefore the implementation uses these guidelines:

1. Most environment variables are set in .profile.  They are then
   exported to the non-login subshells.  The exceptions are
   environment variables that only apply to one kind of shell.  For
   example, PROMPT_DIRTRIM is only supported by bash so we put it's
   definition in .bashrc.

2. The .profile sources the .${SHELL}rc files.  That way both login
   and non-login shells will define functions and aliases.

3. The .${SHELL}rc files source .bournerc which is where aliases,
   functions, and PS1 are defined.

4. Environment variables that are intended only for .profile and
   .${SHELL}rc use start with a single underscore.

5. The .signature file is selected at install.py time, not at app
   runtime.  Too many apps use .signature, and each would need to be
   configured to select the correct one.  It is much easier to put the
   selection just in install.py.  That means that my role is selected
   by which machine that I am on.
