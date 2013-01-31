========================
Bourne Shell Programming
========================

To remain portable, all of my shell scripting is done with POSIX
``sh`` (Bourne shell) constructs, even when running under ``bash`` or
``ksh``.

Determining if a Program Exists
-------------------------------

Avoid ``which``. Not only is it an external process you're launching
for doing very little (meaning builtins like ``hash`` or ``type`` are
way cheaper), you can also rely on the builtins to actually do what
you want, while the effects of external commands can easily vary from
system to system.

Why care?

Many operating systems have a ``which`` that doesn't even set an exit
status, meaning the ``if which foo >/dev/null`` won't even work there
and will always report that foo exists, even if it doesn't.  Many
operating systems make which do custom and evil stuff like change the
output or even hook into the package manager.  So, don't use
``which``. Instead use ``hash``: ::

    if ! hash foo 2>/dev/null; then
        echo "ERROR: foo not installed.  Run:" 1>&2
        echo "       sudo apt-get install foo" 1>&2
        exit 1
    fi

The ``type -P`` command is available in ``bash``, but not in ``sh``.

Boolean Variables
-----------------

Use the ``true`` and ``false`` programs::

    IS_DOG=true
    if $IS_DOG; then
        # is a dog
    else
        # is not a dog
    fi

Absolute Pathnames For Commands
-------------------------------

Hardcoding paths, like ``/bin/true`` or ``/usr/bin/python``, makes any
shell-script much less portable, as the exact paths vary from Unix
variant to Unix variant.  If you are attempting to provide security,
setting ``PATH`` to a system-only path at the beginning of the script
is a much better idea.

See `openbsd-misc 2013-01: OT using absolute paths in scripts
<http://archives.neohapsis.com/archives/openbsd/2013-01/0726.html>`_.

[ vs. [[ Tests
--------------

The ``[`` ("test" command) and ``[[`` ("new test" command) are used to
evaluate expressions. ``[[`` works only in Bash and Korn shell, and is
more powerful; ``[`` and test are available in POSIX shells.

When should the new test command ``[[`` be used, and when the old one
``[``?

* If at all possible, use the old ``[`` for portability.

* Both Bash and Korn shells implement ``[`` and ``[[`` as a built-in, so
  performance is not a reason to choose ``[[``.

* The ``[[`` test replaces the -a (AND) and -o (OR) operators with
  ``&&`` and ``||``, respectively.  This is not a sufficient reason to
  choose ``[[``.

* When using ``[[``, the second operand of the ``!=`` and ``=``
  expressions are patterns (e.g. the comparison ``[[ foobar = f*r ]]``
  succeeds).

Determining if the Shell is Interactive
---------------------------------------

If a Bourne-compliant shell is interactive, then ``PS1`` is set and
``$-`` includes the character "i".  But nothing prevents a
non-interactive shell from setting ``PS1``, so the ``$-`` test is
preferred: ::

    [[ $- != *i* ]] && return

The current set of ``set -o`` options (with single letter names) can
be found in the parameter ``$-``.

Determining if a Command is Part of a Pipe
------------------------------------------

Use the ``[ -t 1 ]`` test to determine if stdout is connected to a
terminal.  If it is not, then the command is part of a pipe.

For example, let's say that the window title is being set via the
``PS1`` environment variable.  If you ssh to a host where it's ``PS1``
does not set the window title, then the window title will be
incorrect.  So we would like to create an alias for ssh that will set
the window title to a default value before calling ssh.  If the target
machine is configured correctly, it will immediately set the window
title back to something useful.  But if the ssh is part of a pipe like
``REMOTE_FILES=`ssh remote ls```, then you do not want to inject
control characters into the output stream.  That can be handled like
this: ::

    alias ssh="[ -t 1 ] && echo -ne '\033]0;ssh\007' ; ssh"

The echo is only executed if ssh is not called from within a pipe.
The final ssh is always executed.

$* vs. $@
---------

``$*`` (star) and ``$@`` (at) expand to a string containing all of the
command-line arguments, as if you had used ``$1 $2 $3...``

The difference between ``$*`` and ``$@`` lies in the way they behave
when they occur inside double quotes: ``$*`` behaves in the normal
way, whereas ``$@`` creates a separate double-quoted string for each
command-line argument. That is, ``"$*"`` behaves as if you had written
``"$1 $2 $3"``, whereas ``"$@"`` behaves as if you had written ``"$1"
"$2" "$3"``.  In most cases ``"$@"`` should be used because it
preserves spaces in arguments.
