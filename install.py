#!/usr/bin/env python

"""
Installs files in tschutter/homefiles using symbolic links.
"""

import sys
if sys.version_info < (2, 4):
    print "Python versions prior to 2.4 not supported."
    sys.exit(1)

import glob
import optparse
import os
import shutil
import stat
import subprocess
import tempfile
import time


def force_run_command(args, stdinstr=None):
    """Run an external command, returning stdout and stderr as a string."""
    if sys.version_info < (2, 4):
        print "WARNING: Not running %s" % " ".join(args)
        return ""
    else:
        if stdinstr == None:
            stdinstr = ""
        process = subprocess.Popen(
            args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        stdoutdata, stderrdata = process.communicate(stdinstr)
        return stdoutdata + stderrdata


def run_command(options, args, stdinstr=None):
    """Run an external command, printing stdout and stderr."""
    if sys.version_info < (2, 4):
        print "WARNING: Not running %s" % " ".join(args)
        return

    if options.verbose:
        print "Running '%s'" % " ".join(args)
    if not options.dryrun:
        output = force_run_command(args, stdinstr)
        output = output.rstrip()
        if len(output) > 0:
            print output


def cygpath_w(pathname):
    """Converts a pathname to Windows style X:\dir\file."""
    if sys.platform == "cygwin":
        pipe = subprocess.Popen(
            ["cygpath", "--windows", pathname],
            stdout=subprocess.PIPE
        ).stdout
        for line in pipe:
            pathname = line.strip()
        pipe.close()

    return pathname


def cygpath_u(pathname):
    """Converts a pathname to Cygwin style /cygpath/X/dir/file."""
    if sys.platform == "cygwin":
        pipe = subprocess.Popen(
            ["cygpath", "--unix", pathname],
            stdout=subprocess.PIPE
        ).stdout
        for line in pipe:
            pathname = line.strip()
        pipe.close()

    return pathname


def simplify_path(path):
    """Perform the inverse of os.path.expanduser()."""
    homedir = os.path.expanduser("~")
    path = os.path.abspath(path)
    if path.startswith(homedir):
        path = os.path.join("~", path[len(homedir) + 1:])
    return path


def mkdir(options, directory, mode):
    """Create directory."""
    if not os.path.isdir(directory):
        print "Creating '%s' directory." % simplify_path(directory)
        if not options.dryrun:
            os.mkdir(directory, mode)


def file_in_path(filename):
    """Returns True if filename is in PATH."""
    # Get the PATH.
    path = os.getenv("PATH", os.defpath)

    # Add ~/bin in case this is not a login shell.
    path = "%s%s%s" % (path, os.pathsep, os.path.expanduser("~/bin"))

    # Loop through all of the path components searching for filename.
    for pathdir in path.split(os.pathsep):
        pathname = os.path.join(pathdir, filename)
        if os.path.exists(pathname):
            return True
    return False


def clean_link(options, linkname, backup=True):
    """Delete link or backup files and dirs."""
    link_pathname = os.path.join(options.homedir, linkname)

    if os.path.islink(link_pathname):
        # The destination exists as a symbolic link.
        print "Deleting symbolic link '%s'." % link_pathname
        if not options.dryrun:
            os.unlink(link_pathname)

    elif os.path.exists(link_pathname):
        if os.path.isdir(link_pathname):
            if len(os.listdir(link_pathname)) == 0:
                print "Removing empty directory '%s'." % link_pathname
                if not options.dryrun:
                    os.rmdir(link_pathname)
                    return

        # The destination exists as a file or dir.  Back it up.
        if backup:
            backup_dir = os.path.join(options.var_dir, "homefiles_backup")
            mkdir(options, backup_dir, 0700)
            print "Moving '%s' to '%s'." % (link_pathname, backup_dir)
            if not options.dryrun:
                shutil.move(link_pathname, backup_dir)
        else:
            print "Deleting file or directory '%s'." % link_pathname
            if not options.dryrun:
                os.unlink(link_pathname)


def make_link(options, enabled, filename, linkname=None):
    """If enabled is True, create a symbolic link from linkname to
    filename.

    If linkname is relative, prefix it with $HOME.  If filename is
    relative, prefix it with $HOME/.homefiles.  If linkname is not
    specified, it is the same as filename.
    """

    if linkname == None:
        if os.path.isabs(filename):
            raise ValueError(
                "default linkname cannot be used with absolute filename"
            )
        linkname = filename

    # Determine the source and destination pathnames.
    if os.path.isabs(filename):
        file_pathname = filename
    else:
        file_pathname = os.path.join(options.homefiles, filename)
    if os.path.isabs(linkname):
        link_pathname = linkname
    else:
        link_pathname = os.path.join(options.homedir, linkname)

    # The target filename should always exist.
    if not os.path.exists(file_pathname):
        print "ERROR: File '%s' does not exist." % file_pathname
        sys.exit(1)

    if enabled and not options.force and os.path.islink(link_pathname):
        # The destination already exists as a symbolic link.  Delete it if
        # it points to the wrong place.
        try:
            samefile = os.path.samefile(file_pathname, link_pathname)
        except OSError:
            samefile = False
        if not samefile:
            clean_link(options, linkname)
        else:
            if options.verbose:
                print "Link already exists from '%s' to '%s'." % (
                    link_pathname,
                    file_pathname
                )
            return
    else:
        clean_link(options, linkname)

    if not enabled:
        if options.verbose:
            print "Not linking to '%s' (not enabled)." % filename
        return

    # Ensure that the link_pathname directory exists.
    link_dir = os.path.dirname(link_pathname)
    if not os.path.isdir(link_dir):
        mkdir(options, link_dir, 0766)

    # Make the link target relative.  This usually makes the link
    # shorter in ls output.
    if sys.version_info < (2, 6):
        link_target = file_pathname
    else:
        link_target = os.path.relpath(
            file_pathname,
            link_dir
        )

    # Make the symbolic link from link_pathname to link_target.
    print "Creating symbolic link from '%s' to '%s'." % (
        link_pathname,
        link_target
    )
    if not options.dryrun:
        os.symlink(link_target, link_pathname)


def make_dot_link(options, enabled, filename):
    """Create a symbolic link from home/.filename to homefiles/filename.
    """
    return make_link(options, enabled, filename, "." + filename)


def make_sig_link(options):
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
        make_link(options, True, "signature-corelogic", ".signature")
    else:
        make_link(options, True, "signature-home", ".signature")


def create_dotless(options, enabled):
    """
    Create ~/.homefiles/less dotfile.

    The lesskey program creates the less dotfile.
    """

    dotless_pathname = os.path.join(options.homefiles, "less")
    lesskey = [
        "#env",
        "LESSHISTFILE=%s" % os.path.join(options.var_dir, "less_history")
    ]

    if enabled:
        if options.force or not os.path.exists(dotless_pathname):
            print "Running lesskey to create '%s'." % dotless_pathname
            run_command(
                options,
                ["lesskey", "-o", dotless_pathname, "-"],
                "\n".join(lesskey)
            )
    else:
        clean_link(options, dotless_pathname)


def process_terminfo(options):
    """Process terminfo compilation."""
    terminfo_dir = os.path.join(options.homedir, ".terminfo")
    terminfo_compiled = os.path.join(terminfo_dir, "r", "rxvt-unicode")
    if not os.path.exists(terminfo_compiled):
        print "Running tic to create '%s'." % terminfo_compiled
        terminfo_source = os.path.join(
            options.homefiles,
            "rxvt-unicode.terminfo"
        )
        run_command(
            options,
            [
                "tic",
                "-o",
                terminfo_dir,
                "-x",
                terminfo_source
            ]
        )


def link_dotfiles(options):
    """Create links in ~ to dotfiles."""

    # Determine if gvim or vim is installed.  Python 2.4 does not have any().
    vim_installed = False
    for exe in ["gvim", "vim", "vim.tiny"]:
        vim_installed = file_in_path(exe)
        if vim_installed:
            break

    make_dot_link(options, file_in_path("aspell"), "aspell.en.prepl")
    make_dot_link(options, file_in_path("aspell"), "aspell.en.pws")
    make_dot_link(options, True, "bournerc")
    clean_link(
        options,
        os.path.join(options.homedir, ".bash_history"),
        backup=False
    )
    clean_link(options, os.path.join(options.homedir, ".bash_profile"))
    make_dot_link(options, os.path.exists("/bin/bash"), "bashrc")
    make_dot_link(options, os.path.exists("/bin/bash"), "bash_logout")
    clean_link(options, os.path.join(options.homedir, ".emacs"))
    make_dot_link(options, file_in_path("emacs"), "emacs.d")
    if not sys.platform.startswith("openbsd"):
        make_dot_link(options, file_in_path("vi"), "exrc")
    make_dot_link(options, file_in_path("gdb"), "gdbinit")
    make_dot_link(options, file_in_path("git"), "gitconfig")
    make_dot_link(options, True, "inputrc")
    make_dot_link(options, os.path.exists("/bin/ksh"), "kshrc")
    make_dot_link(options, file_in_path("lbdbq"), "lbdbrc")
    if file_in_path("less") and file_in_path("lesskey"):
        # Inside if, because make_dot_link complains if create_dotless is
        # not run.
        create_dotless(options, True)
        make_dot_link(options, True, "less")
    make_dot_link(
        options,
        file_in_path("mail") or file_in_path("mutt"),
        "mailcap"
    )
    make_dot_link(options, file_in_path("mg"), "mg")
    make_dot_link(options, file_in_path("mintty"), "minttyrc")
    make_dot_link(options, file_in_path("mutt"), "mutt")
    make_dot_link(options, file_in_path("muttprint"), "muttprintrc")
    if sys.platform.startswith("openbsd"):
        make_dot_link(options, file_in_path("vi"), "nexrc")
    make_dot_link(options, file_in_path("password-gorilla"), "gorillarc")
    make_dot_link(options, True, "profile")
    make_dot_link(options, file_in_path("pychecker"), "pycheckrc")
    make_dot_link(options, file_in_path("pdb"), "pdbrc")
    make_dot_link(options, file_in_path("pdb"), "pdbrc.py")
    make_dot_link(options, file_in_path("pylint"), "pylintrc")
    make_dot_link(options, file_in_path("python"), "pythonstartup")
    make_dot_link(options, file_in_path("screen"), "screenrc")
    make_sig_link(options)
    if sys.platform.startswith("openbsd"):
        process_terminfo(options)
    make_dot_link(options, file_in_path("tmux"), "tmux.conf")
    make_dot_link(options, file_in_path("urxvt"), "urxvt")
    make_dot_link(options, file_in_path("valgrind"), "valgrindrc")
    clean_link(
        options,
        os.path.join(options.homedir, ".viminfo"),
        backup=False
    )
    make_dot_link(options, vim_installed, "vimrc")
    make_link(
        options,
        file_in_path("xfce4-terminal"),
        "terminalrc",
        ".config/Terminal/terminalrc"
    )
    make_dot_link(options, file_in_path("xzgv"), "xzgvrc")
    make_dot_link(options, file_in_path("w3m"), "w3m")
    # Smack the ~/.Xdefaults and ~/.Xresources link if they exist.
    clean_link(options, os.path.join(options.homedir, ".Xdefaults"))
    clean_link(options, os.path.join(options.homedir, ".Xresources"))
    make_dot_link(options, options.is_xwindows, "xsessionrc")


def link_binfiles(options):
    """Create links in ~/bin."""
    bindir = os.path.join(options.homedir, "bin")
    mkdir(options, bindir, 0777)
    make_link(options, True, "bin/abook-lookup")
    make_link(options, True, "bin/append-missing-newline")
    make_link(options, options.is_cygwin, "bin/cygwin-fix-sshd")
    make_link(options, True, "bin/cmake-clean")
    make_link(options, True, "bin/find-non-ascii")
    make_link(options, True, "bin/findfile")
    make_link(options, True, "bin/install-essentials")
    make_link(options, True, "bin/open")
    make_link(options, True, "bin/pycheck")
    make_link(options, True, "bin/strip-bom")
    make_link(options, True, "bin/svn-ignore")
    make_link(options, True, "bin/tgrep")
    make_link(options, True, "bin/tm")
    make_link(options, os.path.exists("/usr/bin/u1sdtool"), "bin/u1sdtool")
    make_link(options, True, "bin/unicode2ascii")


def xfwm4_remove_keybinding(options, binding):
    """Remove a xfwm4 keybinding."""
    if os.path.exists("/usr/bin/xfconf-query"):
        args = [
            "/usr/bin/xfconf-query",
            "--channel",
            "xfce4-keyboard-shortcuts",
            "--property"
        ]
        output = force_run_command(args + [binding])
        if output.find("does not exist on channel") != -1:
            if options.verbose:
                print "Keybinding '%s' already removed." % binding
        else:
            print "Removing keybinding '%s' from xfce4 config." % binding
            run_command(options, args + [binding, "--reset"])


def configure_wm_keybindings(options):
    """Setup window manager keybindings."""

    # xfconf-query cannot run unless there is a valid DISPLAY.
    if "DISPLAY" in os.environ:
        # C-F3,C-F4 are set in emacs.d/init.el so we take them away from xfwm4.
        xfwm4_remove_keybinding(options, "/xfwm4/custom/<Control>F3")
        xfwm4_remove_keybinding(options, "/xfwm4/custom/<Control>F4")


def create_tmp_file(prefix, suffix, contents):
    """Create a temporary file containing contents."""
    handle, pathname = tempfile.mkstemp(prefix=prefix, suffix=suffix)
    os.fchmod(handle, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR)
    tmp_file = os.fdopen(handle, "wb")
    tmp_file.write(contents)
    return pathname


def install_fonts(options):
    """Install fonts."""

    font_src_dir = os.path.join(options.homefiles, "fonts")

    # See http://blogs.technet.com/b/heyscriptingguy/archive/\
    # 2008/04/25/how-can-i-install-fonts-using-a-script.aspx
    if options.is_cygwin or options.is_windows:
        # Need to see what cygwin local and cygwin ssh do here...
        system_root = os.environ["SYSTEMROOT"]
        dst_dir = os.path.join(system_root, "Fonts")
        for filename in os.listdir(font_src_dir):
            if filename.endswith(".ttf"):
                src_pathname = os.path.join(font_src_dir, filename)
                dst_pathname = os.path.join(dst_dir, filename)
                dst_pathname = cygpath_u(dst_pathname)
                if not options.force and os.path.exists(dst_pathname):
                    continue
                print "Installing font '%s'." % src_pathname
                if not options.dryrun:
                    vbs_text = (
                        'Set objShell = CreateObject("Shell.Application")\r\n'
                        'Set objFolder = objShell.Namespace(&H14&)\r\n'
                        'objFolder.CopyHere "%s"\r\n' % cygpath_w(src_pathname)
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
                        options,
                        [
                            cmd_exe,
                            "/c",
                            "start",
                            cygpath_w(vbs_pathname)
                        ],
                        None
                    )
                    # We should give the "/wait" parameter to the
                    # start command above.  But that sometimes causes
                    # a hang.  Therefore we just sleep here.
                    time.sleep(5)
                    os.unlink(vbs_pathname)
    else:
        # Note that ttf-ubuntu-font-family 0.71 did not include UbuntuMono.
        system_has_ubuntu_mono = os.path.exists(
            "/usr/share/fonts/truetype/ubuntu-font-family/UbuntuMono-R.ttf"
        )
        make_dot_link(options, not system_has_ubuntu_mono, "fonts")

    # Install fonts in gimp(1).
    font_glob = os.path.join(font_src_dir, "*.ttf")
    gimp_font_dir_glob = os.path.join(options.homedir, ".gimp-*", "fonts")
    for gimp_font_dir in glob.glob(gimp_font_dir_glob):
        for font_pathname in glob.glob(font_glob):
            font_filename = os.path.basename(font_pathname)
            gimp_link = os.path.join(gimp_font_dir, font_filename)
            make_link(
                options,
                True,
                font_pathname,
                gimp_link
            )


def main():
    """main"""
    option_parser = optparse.OptionParser(
        usage="usage: %prog [options]\n" +
            "  Installs files in tschutter/homefiles using symbolic links."
    )
    option_parser.add_option(
        "--home",
        action="store",
        dest="homedir",
        metavar="DIR",
        default="~",
        help="specify directory to install to (default=%default)"
    )
    option_parser.add_option(
        "--private",
        action="store",
        dest="private_dir",
        metavar="DIR",
        default=os.path.join("~", "private"),
        help="specify private directory (default=%default)"
    )
    option_parser.add_option(
        "--var",
        action="store",
        dest="var_dir",
        metavar="DIR",
        default=os.path.join("~", ".var"),
        help="specify var directory (default=%default)"
    )
    option_parser.add_option(
        "--force",
        action="store_true",
        dest="force",
        default=False,
        help="replace existing files and symbolic links"
    )
    option_parser.add_option(
        "-n",
        "--dry-run",
        action="store_true",
        dest="dryrun",
        default=False,
        help="print commands that would be executed, but do not execute them"
    )
    option_parser.add_option(
        "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
        help="produce more verbose output"
    )

    (options, args) = option_parser.parse_args()
    if len(args) != 0:
        option_parser.error("invalid argument")
    options.homedir = os.path.expanduser(options.homedir)
    options.private_dir = os.path.expanduser(options.private_dir)
    options.var_dir = os.path.expanduser(options.var_dir)

    # Determine what platform we are on.
    options.is_cygwin = sys.platform == "cygwin"
    options.is_windows = sys.platform.startswith("win")
    options.is_xwindows = not (options.is_cygwin or options.is_windows)

    options.homefiles = os.path.dirname(os.path.abspath(__file__))

    mkdir(options, options.var_dir, 0700)
    if file_in_path("mutt"):
        # Create ~/.var/mutt so mutt can store certs on first run.
        mkdir(options, os.path.join(options.var_dir, "mutt"), 0700)

    link_dotfiles(options)

    link_binfiles(options)

    configure_wm_keybindings(options)

    install_fonts(options)

    return 0

if __name__ == "__main__":
    sys.exit(main())
