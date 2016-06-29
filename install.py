#!/usr/bin/env python

"""
Installs files in tschutter/homefiles using symbolic links.
"""

# TODO:
# Connect mailto-mutt:
#   modify ~/.config/xfce4/helpers.rc
#   create ~/.local/share/xfce4/helpers/custom-MailReader.desktop

from __future__ import print_function
import argparse
import glob
import os
import shutil
import stat
import subprocess
import sys
import tempfile
import time
try:
    # pylint: disable=F0401
    from xdg.BaseDirectory import xdg_cache_home
except ImportError:
    # xdg not available on all platforms
    # pylint: disable=C0103
    xdg_cache_home = os.path.expanduser("~/.cache")


def force_run_command(cmdargs, stdinstr=None):
    """Run an external command, returning stdout and stderr as a string."""
    if stdinstr is None:
        stdinstr = ""
    process = subprocess.Popen(
        cmdargs,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    if stdinstr is not None:
        stdinstr = stdinstr.encode("ascii")
    stdoutdata, stderrdata = process.communicate(stdinstr)
    return (stdoutdata + stderrdata).decode()


def run_command(args, cmdargs, stdinstr=None):
    """Run an external command, printing stdout and stderr."""
    if args.verbose:
        print("Running '{0}'".format(" ".join(cmdargs)))
    if not args.dryrun:
        output = force_run_command(cmdargs, stdinstr)
        output = output.rstrip()
        if len(output) > 0:
            print(output)


def cygpath_w(pathname):
    """Converts a pathname to Windows style X:\foo\bar."""
    if sys.platform == "cygwin":
        pipe = subprocess.Popen(
            ["cygpath", "--windows", pathname],
            stdout=subprocess.PIPE
        ).stdout
        for line in pipe:
            pathname = line.strip().decode("ascii")
        pipe.close()

    return pathname


def cygpath_u(pathname):
    """Converts a pathname to Cygwin style /cygpath/X/foo/bar."""
    if sys.platform == "cygwin":
        pipe = subprocess.Popen(
            ["cygpath", "--unix", pathname],
            stdout=subprocess.PIPE
        ).stdout
        for line in pipe:
            pathname = line.strip().decode("ascii")
        pipe.close()

    return pathname


def simplify_path(path):
    """Perform the inverse of os.path.expanduser()."""
    homedir = os.path.expanduser("~")
    path = os.path.abspath(path)
    if path.startswith(homedir):
        path = os.path.join("~", path[len(homedir) + 1:])
    return path


def mkdir(args, enabled, directory, mode):
    """Create directory."""
    if enabled:
        if not os.path.isdir(directory):
            print("Creating '{0}' directory.".format(simplify_path(directory)))
            if not args.dryrun:
                os.mkdir(directory, mode)
    else:
        clean_link(args, directory, False)


def file_in_path(filename):
    """Returns True if filename is in PATH."""
    # Get the PATH.
    path = os.getenv("PATH", os.defpath)

    # Add ~/bin in case this is not a login shell.
    path = os.pathsep.join((path, os.path.expanduser("~/bin")))

    # Loop through all of the path components searching for filename.
    for pathdir in path.split(os.pathsep):
        pathname = os.path.join(pathdir, filename)
        if os.path.exists(pathname):
            return True
    return False


def clean_link(args, linkname, backup=True):
    """Delete link or backup files and dirs."""
    link_pathname = os.path.join(args.homedir, linkname)

    if os.path.islink(link_pathname):
        # The destination exists as a symbolic link.
        print("Deleting symbolic link '{0}'.".format(link_pathname))
        if not args.dryrun:
            os.unlink(link_pathname)

    elif os.path.exists(link_pathname):
        if os.path.isdir(link_pathname):
            if len(os.listdir(link_pathname)) == 0:
                print("Removing empty directory '{0}'.".format(link_pathname))
                if not args.dryrun:
                    os.rmdir(link_pathname)
                    return

        # The destination exists as a file or dir.  Back it up.
        if backup:
            backup_dir = os.path.join(args.cache_dir, "homefiles_backup")
            mkdir(args, True, backup_dir, 0o700)
            print("Moving '{0}' to '{1}'.".format(link_pathname, backup_dir))
            if not args.dryrun:
                shutil.move(link_pathname, backup_dir)
        else:
            print("Deleting file or directory '{0}'.".format(link_pathname))
            if not args.dryrun:
                os.unlink(link_pathname)


def make_link(args, enabled, filename, linkname=None):
    """
    If enabled is True, create a symbolic link from linkname to
    filename.

    If filename is relative, prefix it with $HOME/.homefiles.

    If linkname is relative, prefix it with $HOME.  If linkname is not
    specified, it is the same as filename.
    """

    if linkname is None:
        if os.path.isabs(filename):
            raise ValueError(
                "default linkname cannot be used with absolute filename"
            )
        linkname = filename

    # Determine the source and destination pathnames.
    if os.path.isabs(filename):
        file_pathname = filename
    else:
        file_pathname = os.path.normpath(
            os.path.join(args.homefiles, filename)
        )
    if os.path.isabs(linkname):
        link_pathname = linkname
    else:
        link_pathname = os.path.join(args.homedir, linkname)

    # The target filename should always exist.
    if not os.path.exists(file_pathname):
        print("ERROR: File '{0}' does not exist.".format(file_pathname))
        sys.exit(1)

    if enabled and not args.force and os.path.islink(link_pathname):
        # The destination already exists as a symbolic link.  Delete it if
        # it points to the wrong place.
        try:
            samefile = os.path.samefile(file_pathname, link_pathname)
        except OSError:
            samefile = False
        if not samefile:
            clean_link(args, linkname)
        else:
            if args.verbose:
                print(
                    "Link already exists from '{0}' to '{1}'.".format(
                        link_pathname,
                        file_pathname
                    )
                )
            return
    else:
        clean_link(args, linkname)

    if not enabled:
        if args.verbose:
            print("Not linking to '{0}' (not enabled).".format(filename))
        return

    # Ensure that the link_pathname directory exists.
    link_dir = os.path.dirname(link_pathname)
    if not os.path.isdir(link_dir):
        mkdir(args, True, link_dir, 0o766)

    # Make the link target relative.  This usually makes the link
    # shorter in ls output.
    link_target = os.path.relpath(
        file_pathname,
        link_dir
    )

    # Make the symbolic link from link_pathname to link_target.
    print(
        "Creating symbolic link from '{0}' to '{1}'.".format(
            link_pathname,
            link_target
        )
    )
    if not args.dryrun:
        os.symlink(link_target, link_pathname)


def make_dot_link(args, enabled, filename):
    """Create a symbolic link from home/.filename to homefiles/filename."""
    return make_link(args, enabled, filename, "." + filename)


def make_sig_link(args):
    """Create a link to the appropriate signature file."""
    # Determine the realm.
    computername = os.uname()[1].lower()
    prefix = computername[:7]
    if prefix == "fdsvbld":
        realm = "ISC"
    elif prefix == "fdsvdfw":
        realm = "ISCP"
    elif prefix == "fdsvmad":
        realm = "ISC"
    elif prefix == "fdsvsna":
        realm = "ISCP"
    elif computername in ["apple", "passion", "wampi", "wampi-win2003"]:
        realm = "ISC"
    else:
        realm = "HOME"

    # Link the correct signature.
    if realm in ["ISC", "ISCP"]:
        make_link(args, True, "signature-corelogic", ".signature")
    else:
        make_link(args, True, "signature-home", ".signature")


def create_dotless(args):
    """
    Create less dotfile.

    The lesskey program creates the less dotfile in cache_dir/less.
    """

    dotless_dir = os.path.join(args.cache_dir, "less")
    dotless_pathname = os.path.join(dotless_dir, "less")
    history_pathname = os.path.join(dotless_dir, "history")

    enabled = file_in_path("less") and file_in_path("lesskey")
    if enabled:
        if args.force or not os.path.exists(dotless_pathname):
            mkdir(args, True, dotless_dir, 0o700)
            print("Running lesskey to create '{0}'.".format(dotless_pathname))
            lesskey = "#env\nLESSHISTFILE={0}\n".format(history_pathname)
            run_command(
                args,
                ["lesskey", "-o", dotless_pathname, "-"],
                lesskey
            )
    else:
        clean_link(args, dotless_pathname)
        clean_link(args, history_pathname)
        clean_link(args, dotless_dir)

    if not args.dryrun:
        make_link(args, enabled, dotless_pathname, ".less")


def process_terminfo(args):
    """Process terminfo compilation."""
    terminfo_dir = os.path.join(args.homedir, ".terminfo")
    terminfo_compiled = os.path.join(terminfo_dir, "r", "rxvt-unicode")
    if not os.path.exists(terminfo_compiled):
        print("Running tic to create '{0}'.".format(terminfo_compiled))
        terminfo_source = os.path.join(
            args.homefiles,
            "rxvt-unicode.terminfo"
        )
        run_command(
            args,
            [
                "tic",
                "-o",
                terminfo_dir,
                "-x",
                terminfo_source
            ]
        )


def link_dotfiles(args, explicit_cache_dir):
    """Create links in ~ to dotfiles."""
    # pylint: disable=R0915

    # Files and dirs in that have explicit references to ~/.cache must
    # use explicit_cache_dir instead of args.cache_dir.

    # Determine if gvim or vim is installed.  Python 2.4 does not have any().
    vim_installed = False
    for exe in ["gvim", "vim", "vim.tiny"]:
        vim_installed = file_in_path(exe)
        if vim_installed:
            break

    # All files in private_dir must be checked for existence before
    # calling make_link in case private_dir does not exist.
    make_dot_link(args, file_in_path("aspell"), "aspell.en.prepl")
    make_dot_link(args, file_in_path("aspell"), "aspell.en.pws")
    make_dot_link(args, True, "bournerc")

    enabled = os.path.exists("/bin/bash")
    clean_link(args, os.path.join(args.homedir, ".bash_history"), backup=False)
    clean_link(args, os.path.join(args.homedir, ".bash_profile"))
    make_dot_link(args, enabled, "bashrc")
    make_dot_link(args, enabled, "bash_logout")
    mkdir(args, enabled, os.path.join(args.cache_dir, "bash"), 0o700)

    make_dot_link(args, file_in_path("cwm"), "cwmrc")

    clean_link(args, os.path.join(args.homedir, ".emacs"))
    # ~/.cache/emacs is created by ~/.emacs.d/init.el

    if not sys.platform.startswith("openbsd"):
        make_dot_link(args, file_in_path("vi"), "exrc")

    make_dot_link(args, file_in_path("firefox"), "keysnail.js")

    make_link(args, True, "image/ironcat-80.jpg", ".face")

    enabled = file_in_path("gdb")
    make_dot_link(args, enabled, "gdbinit")
    mkdir(args, enabled, os.path.join(explicit_cache_dir, "gdb"), 0o700)

    make_dot_link(args, file_in_path("git"), "gitconfig")

    goobookrc = os.path.join(args.private_dir, "goobookrc")
    if os.path.exists(goobookrc):
        make_link(args, file_in_path("goobook"), goobookrc, ".goobookrc")

    make_dot_link(args, True, "hushlogin")

    make_dot_link(args, True, "inputrc")

    enabled = os.path.exists("/bin/ksh")
    make_dot_link(args, enabled, "kshrc")
    if enabled:
        mkdir(args, enabled, os.path.join(args.cache_dir, "ksh"), 0o700)

    create_dotless(args)

    make_dot_link(
        args,
        file_in_path("mail") or file_in_path("mutt"),
        "mailcap"
    )

    make_dot_link(args, file_in_path("mg"), "mg")

    make_dot_link(args, file_in_path("mintty"), "minttyrc")

    enabled = file_in_path("mutt")
    make_dot_link(args, enabled, "mutt")
    # Create ~/.cache/mutt so mutt can store certs on first run.
    mkdir(args, enabled, os.path.join(explicit_cache_dir, "mutt"), 0o700)

    make_dot_link(args, file_in_path("muttprint"), "muttprintrc")

    enabled = file_in_path("mysql")
    mkdir(args, enabled, os.path.join(args.cache_dir, "mysql"), 0o700)

    if sys.platform.startswith("openbsd"):
        make_dot_link(args, file_in_path("vi"), "nexrc")

    enabled = file_in_path("orpie")
    make_dot_link(args, enabled, "orpierc")
    mkdir(args, enabled, os.path.join(args.cache_dir, "orpie"), 0o700)

    make_dot_link(args, True, "profile")

    make_dot_link(args, file_in_path("pychecker"), "pycheckrc")

    enabled = file_in_path("pdb")
    make_dot_link(args, enabled, "pdbrc")
    make_dot_link(args, enabled, "pdbrc.py")
    mkdir(args, enabled, os.path.join(args.cache_dir, "pdb"), 0o700)

    make_dot_link(args, file_in_path("pylint"), "pylintrc")

    enabled = file_in_path("python")
    clean_link(
        args,
        os.path.join(args.homedir, ".pythonstartup"),
        backup=False
    )
    mkdir(args, enabled, os.path.join(args.cache_dir, "python"), 0o700)

    make_dot_link(args, file_in_path("screen"), "screenrc")

    make_sig_link(args)

    if sys.platform.startswith("openbsd"):
        process_terminfo(args)

    make_dot_link(args, file_in_path("tmux"), "tmux.conf")

    make_dot_link(args, file_in_path("urxvt"), "urxvt")

    make_dot_link(args, file_in_path("valgrind"), "valgrindrc")

    clean_link(args, os.path.join(args.homedir, ".viminfo"), backup=False)
    make_dot_link(args, vim_installed, "vimrc")
    mkdir(args, vim_installed, os.path.join(explicit_cache_dir, "vim"), 0o700)

    xfce4_config_dir = os.path.join(args.homedir, ".config", "xfce4")
    mkdir(
        args,
        file_in_path("xfce4-terminal"),
        xfce4_config_dir,
        0o700
    )
    mkdir(
        args,
        file_in_path("xfce4-terminal"),
        os.path.join(xfce4_config_dir, "terminal"),
        0o700
    )
    make_link(
        args,
        file_in_path("xfce4-terminal"),
        "xfce4-terminal-terminalrc",
        ".config/xfce4/terminal/terminalrc"
    )
    make_link(
        args,
        file_in_path("xfce4-terminal"),
        "xfce4-terminal-accels.scm",
        ".config/xfce4/terminal/accels.scm"
    )

    make_dot_link(args, file_in_path("xzgv"), "xzgvrc")

    make_dot_link(args, file_in_path("w3m"), "w3m")

    if sys.platform.startswith("openbsd"):
        # Load ~/.profile in new xterm.
        make_dot_link(args, True, "Xdefaults")
    else:
        # Smack ~/.Xdefaults if it exists.
        clean_link(args, os.path.join(args.homedir, ".Xdefaults"))

    # Smack ~/.Xresources if it exists.
    clean_link(args, os.path.join(args.homedir, ".Xresources"))

    # xxxterm is previous name for xombrero.
    make_link(args, file_in_path("xxxterm"), "xombrero.conf", ".xxxterm.conf")
    make_dot_link(args, file_in_path("xombrero"), "xombrero.conf")

    mkdir(
        args,
        file_in_path("xpra"),
        os.path.join(args.homedir, ".xpra"),
        0o700
    )
    make_link(args, file_in_path("xpra"), "xpra.conf", ".xpra/xpra.conf")

    make_dot_link(args, args.is_xwindows, "xsessionrc")


def link_binfiles(args):
    """Create links in ~/bin."""
    bindir = os.path.join(args.homedir, "bin")
    mkdir(args, True, bindir, 0o777)
    make_link(args, True, "bin/append-missing-newline")
    make_link(args, args.is_cygwin, "bin/cygwin-fix-sshd")
    make_link(args, True, "bin/cmake-clean")
    make_link(args, True, "bin/find-non-ascii")
    make_link(args, True, "bin/findfile")
    make_link(args, True, "bin/hed")
    make_link(args, True, "bin/install-essentials")
    make_link(args, file_in_path("gnome-open"), "bin/mailto-gmail")
    make_link(args, file_in_path("mutt"), "bin/mailto-mutt")
    make_link(args, True, "bin/open")
    make_link(args, True, "bin/pycheck")
    make_link(args, True, "bin/strip-bom")
    make_link(args, True, "bin/svn-clean")
    make_link(args, True, "bin/svn-ignore")
    make_link(args, True, "bin/ssh-reverse-tunnel")
    make_link(args, True, "bin/tgrep")
    make_link(args, True, "bin/tm")
    make_link(args, True, "bin/unicode2ascii")


def xfwm4_remove_key_binding(args, binding):
    """Remove a xfwm4 key binding."""
    if os.path.exists("/usr/bin/xfconf-query"):
        cmdargs = [
            "/usr/bin/xfconf-query",
            "--channel",
            "xfce4-keyboard-shortcuts",
            "--property",
            binding
        ]
        output = force_run_command(cmdargs)
        if output.find("does not exist on channel") != -1:
            if args.verbose:
                print("Key binding '{0}' already removed.".format(binding))
        else:
            print("Removing key binding '{0}'.".format(binding))
            run_command(args, cmdargs + ["--reset"])


def xfwm4_add_key_binding(args, binding, command):
    """Add a xfwm4 key binding."""
    if os.path.exists("/usr/bin/xfconf-query"):
        cmdargs = [
            "/usr/bin/xfconf-query",
            "--channel",
            "xfce4-keyboard-shortcuts",
            "--property",
            binding
        ]
        output = force_run_command(cmdargs).strip()
        if output == command:
            if args.verbose:
                print(
                    "Key '{0}' already bound to '{1}'.".format(binding, command)
                )
        else:
            print(output)
            if output.find("does not exist on channel") != -1:
                output = ""
            print(
                "Changing binding of key '{0}' from '{1}' to '{2}'.".format(
                    binding,
                    output,
                    command
                )
            )
            run_command(
                args,
                cmdargs + ["--create", "--type", "string", "--set", command]
            )


def configure_wm_keybindings(args):
    """Setup window manager key bindings."""

    # xfconf-query cannot run unless there is a valid DISPLAY.
    if "DISPLAY" not in os.environ:
        return

    # Use "xfconf-query --channel xfce4-keyboard-shortcuts --list --verbose" to
    # list current keybindings.

    # Note that there are "/xfwm4/default" properties and
    # "/xfwm4/custom" properties.  I don't know exactly the
    # difference, but it sounds like the custom properties would
    # override the defaults.

    # C-F3,C-F4 are set in .emacs.d/init.el so we take them away from xfwm4.
    xfwm4_remove_key_binding(args, "/xfwm4/custom/<Control>F3")
    xfwm4_remove_key_binding(args, "/xfwm4/custom/<Control>F4")

    # <Control><Alt>Left moves to the previous workspace, so make
    # <Shift><Alt>Left move the current window to the previous
    # workspace.
    xfwm4_add_key_binding(
        args,
        "/xfwm4/custom/<Shift><Alt>Left",
        "move_window_prev_workspace_key"
    )
    xfwm4_add_key_binding(
        args,
        "/xfwm4/custom/<Shift><Alt>Right",
        "move_window_next_workspace_key"
    )

    # By default <Super>e launches Leafpad.  But I use Emacs not
    # Leafpad.
    xfwm4_add_key_binding(
        args,
        "/commands/custom/<Super>e",
        "emacs"
    )

    # Use <Super>f to launch a file manager.
    xfwm4_add_key_binding(
        args,
        "/commands/custom/<Super>f",
        "exo-open --launch FileManager"
    )

    # Use <Super>t to launch a terminal window.
    xfwm4_add_key_binding(
        args,
        "/commands/custom/<Super>t",
        "exo-open --launch TerminalEmulator"
    )

    # Use the Windows key "<Super>" to manipulate windows.
    winkey = "/xfwm4/custom/<Super>"
    ctrlwinkey = "/xfwm4/custom/<Super><Ctrl>"
    xfwm4_add_key_binding(args, winkey + "Left", "prev_workspace_key")
    xfwm4_add_key_binding(args, winkey + "Right", "next_workspace_key")
    for num in range(1, 10):
        xfwm4_add_key_binding(
            args,
            winkey + "F{0}".format(num),
            "workspace_{0}_key".format(num)
        )
    xfwm4_add_key_binding(
        args,
        ctrlwinkey + "Left",
        "move_window_prev_workspace_key"
    )
    xfwm4_add_key_binding(
        args,
        ctrlwinkey + "Right",
        "move_window_next_workspace_key"
    )
    for num in range(1, 10):
        xfwm4_add_key_binding(
            args,
            ctrlwinkey + "F{0}".format(num),
            "move_window_workspace_{0}_key".format(num)
        )

    # But these do not work.
    xfwm4_remove_key_binding(args, "/commands/custom/<Super>1")
    xfwm4_remove_key_binding(args, "/commands/custom/<Super>2")
    xfwm4_add_key_binding(args, winkey + "1", "workspace_1_key")
    xfwm4_add_key_binding(args, winkey + "2", "workspace_2_key")


def create_tmp_file(prefix, suffix, content_bytes):
    """Create a temporary file containing contents."""
    handle, pathname = tempfile.mkstemp(prefix=prefix, suffix=suffix)
    os.fchmod(handle, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR)
    tmp_file = os.fdopen(handle, "wb")
    tmp_file.write(content_bytes)
    return pathname


def install_fonts(args):
    """Install fonts."""

    font_src_dir = os.path.join(args.homefiles, "fonts")

    # See http://blogs.technet.com/b/heyscriptingguy/archive/\
    # 2008/04/25/how-can-i-install-fonts-using-a-script.aspx
    if args.is_cygwin or args.is_windows:
        # Need to see what cygwin local and cygwin ssh do here...
        system_root = os.environ["SYSTEMROOT"]
        dst_dir = os.path.join(system_root, "Fonts")
        for filename in os.listdir(font_src_dir):
            if filename.endswith(".ttf"):
                src_pathname = os.path.join(font_src_dir, filename)
                dst_pathname = os.path.join(dst_dir, filename)
                dst_pathname = cygpath_u(dst_pathname)
                if not args.force and os.path.exists(dst_pathname):
                    continue
                print("Installing font '{0}'.".format(src_pathname))
                vbs_text = bytes(
                    'Set objShell = CreateObject("Shell.Application")\r\n' +
                    'Set objFolder = objShell.Namespace(&H14&)\r\n' +
                    'objFolder.CopyHere "{0}"\r\n'.format(
                        cygpath_w(src_pathname)
                    ),
                    'UTF-8'
                )
                vbs_pathname = create_tmp_file(
                    "install-font",
                    ".vbs",
                    vbs_text
                )
                cmd_exe = os.path.join(
                    cygpath_u(system_root),
                    "system32",
                    "cmd.exe"
                )
                run_command(
                    args,
                    [
                        cmd_exe,
                        "/c",
                        "start",
                        cygpath_w(vbs_pathname)
                    ]
                )
                # We should give the "/wait" parameter to the
                # start command above.  But that sometimes causes
                # a hang.  Therefore we just sleep here.
                time.sleep(5)
                os.unlink(vbs_pathname)
    else:
        # Install for any program that uses the Fontconfig library,
        # which includes gimp and OpenOffice among others.
        fontconfig_dir = os.path.join(args.homedir, ".fonts")
        mkdir(args, True, fontconfig_dir, 0o755)
        font_glob = os.path.join(font_src_dir, "*.ttf")
        for font_pathname in glob.glob(font_glob):
            font_filename = os.path.basename(font_pathname)
            fontconfig_link = os.path.join(fontconfig_dir, font_filename)
            make_link(args, True, font_pathname, fontconfig_link)


def main():
    """main"""

    arg_parser = argparse.ArgumentParser(
        description="Install files in ~/.homefiles using symbolic links, "
        "configure keybindings, and install fonts."
    )
    arg_parser.add_argument(
        "--home",
        action="store",
        dest="homedir",
        metavar="DIR",
        default=os.path.expanduser("~"),
        help="directory to install to (default=%(default)s)"
    )
    arg_parser.add_argument(
        "--private",
        action="store",
        dest="private_dir",
        metavar="DIR",
        default=os.path.expanduser(os.path.join("~", "private")),
        help="private directory (default=%(default)s)"
    )
    arg_parser.add_argument(
        "--cache",
        action="store",
        dest="cache_dir",
        metavar="DIR",
        default=xdg_cache_home,
        help="cache directory (default=%(default)s)"
    )
    arg_parser.add_argument(
        "--force",
        action="store_true",
        dest="force",
        default=False,
        help="replace existing files and symbolic links"
    )
    arg_parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        dest="dryrun",
        default=False,
        help="print commands, but do not execute them"
    )
    arg_parser.add_argument(
        "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
        help="produce more verbose output"
    )
    args = arg_parser.parse_args()

    # Canonicalize directories.
    args.homedir = os.path.expanduser(args.homedir)
    args.private_dir = os.path.expanduser(args.private_dir)
    args.homefiles = os.path.dirname(os.path.abspath(__file__))

    # Files and dirs in that have explicit references to ~/.cache must
    # use explicit_cache_dir instead of args.cache_dir.
    explicit_cache_dir = os.path.expanduser(os.path.join("~", ".cache"))

    # Determine what platform we are on.
    args.is_cygwin = sys.platform == "cygwin"
    args.is_windows = sys.platform.startswith("win")
    args.is_xwindows = not (args.is_cygwin or args.is_windows)

    # Ensure that directories exist.
    mkdir(args, True, args.cache_dir, 0o700)
    mkdir(args, True, explicit_cache_dir, 0o700)

    link_dotfiles(args, explicit_cache_dir)

    link_binfiles(args)

    configure_wm_keybindings(args)

    install_fonts(args)

    return 0


if __name__ == "__main__":
    sys.exit(main())
