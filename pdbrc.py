# Loaded by ~/.pdbrc, because .pdbrc can only contain pdb commands.

# Command line history.
import atexit
import os
import readline
if "XDG_CACHE_DIR" in os.environ:
    cache_dir = os.environ["XDG_CACHE_DIR"]
elif "HOME" in os.environ:
    cache_dir = os.path.join(os.environ["HOME"], ".cache")
else:
    cache_dir = os.path.expanduser(os.path.join("~", ".cache"))
histfile = os.path.join(cache_dir, "pdb", "history")
try:
    readline.read_history_file(histfile)
except IOError:
    pass
atexit.register(readline.write_history_file, histfile)
del histfile
del cache_dir
readline.set_history_length(200)

# Cleanup any variables that could otherwise clutter up the namespace.
del atexit, os, readline
