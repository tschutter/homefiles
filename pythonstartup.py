"""
Loaded by interactive Python sessions.

Referenced by the PYTHONSTARTUP environment variable.

For occasional use.  If you are looking in here, you probably should
be using ipython instead.

See http://docs.python.org/tutorial/interactive.html
"""

from __future__ import print_function
import atexit
import os.path
import readline
import rlcompleter
try:
    from xdg.BaseDirectory import xdg_cache_home
except ImportError:
    # xdg not available on all platforms
    # pylint: disable=C0103
    print("xdg module not available, see ~/.homefiles/pythonstartup.py")
    xdg_cache_home = os.path.expanduser("~/.cache")

# Including rlcompleter is required for auto-completion.  The assert
# exists to prevent unused import warnings.
assert rlcompleter

# Enable auto-completion with the tab key.
readline.parse_and_bind('tab: complete')
print("Use TAB for auto-completion.")


def _history_pathname():
    """
    Determine pathname of where history is stored.

    Default is ~/.python_history, but we are trying to cleanup the
    user's home directory.
    """
    return os.path.join(xdg_cache_home, "python", "history")


def _save_history():
    """Save history to file."""
    pathname = _history_pathname()
    directory = os.path.dirname(pathname)
    if not os.path.exists(directory):
        os.mkdir(directory)
    readline.write_history_file(pathname)


atexit.register(_save_history)


def _load_history():
    """Load history from file."""
    history_pathname = _history_pathname()
    if os.path.exists(history_pathname):
        readline.read_history_file(history_pathname)
        print("History loaded from {}.".format(history_pathname))


_load_history()
