#!/usr/bin/env python3

"""Convert Music library (mostly in FLAC format) to MP3 format."""

import argparse
import os
import shutil
import sys


def mkdir(args, directory):
    """Create directory if it does not exist."""

    if not os.path.isdir(directory):
        if not args.quiet:
            print(f"mkdir {directory}")
        if not args.dry_run:
            os.makedirs(directory)


def process_file(args, ext, src_pathname, dst_pathname):
    """Process one file."""

    if ext == ".mp3":  # QWER or newer than
        if not args.quiet:
            print(f"cp {src_pathname} {dst_pathname}")
        if not args.dry_run:
            shutil.copyfile(src_pathname, dst_pathname)

    elif ext == ".flac":  # QWER or newer than
        command = [
            "ffmpeg",
            "-hide_banner",
            "-loglevel", "warning",
            "-i", src_pathname,
            "-qscale:a 8",
            dst_pathname
        ]
        command = " ".join(command)
        if not args.quiet:
            print(command)
        if not args.dry_run:
            os.system(command)

    else:
        print(f"Skipping file '{src_pathname}' of unknown type.")


def main():
    """Main."""

    arg_parser = argparse.ArgumentParser(
        description="Update MP3 files from FLAC files."
    )
    arg_parser.add_argument(
        "dirs",
        metavar="dir",
        nargs="*",
        help="subdir to process (default=all)"
    )
    arg_parser.add_argument(
        "-n", "--dry-run",
        dest="dry_run",
        action="store_true",
        default=False,
        help="do not execute any commands"
    )
    arg_parser.add_argument(
        "-q", "--quiet",
        dest="quiet",
        action="store_true",
        default=False,
        help="do not display commands"
    )
    arg_parser.add_argument(
        "--root",
        default="/media/tom/external4TB",
        help="directory containing Music and Music-mp3 (default=%(default)s)"
    )
    args = arg_parser.parse_args()
    if len(args.dirs) == 0:
        args.dirs.append(".")

    src_root = os.path.join(args.root, "Music")
    if not os.path.isdir(src_root):
        print(f"ERROR: Source dir '{src_root}' does not exist.")
        return 1
    dst_root = os.path.join(args.root, "Music-mp3")

    for subdir in args.dirs:
        src_dir = os.path.join(src_root, subdir)
        dst_dir = os.path.join(dst_root, subdir)

        for root, _dirs, files in os.walk(src_dir):
            for filename in sorted(files):
                src_pathname = os.path.join(root, filename)
                if not src_pathname.startswith(src_dir):
                    print(f"ERROR: '{src_pathname} prefix != '{src_dir}'")
                rel_filename = src_pathname[len(src_dir) + 1:]  # + 1 for the /
                rel_filename, ext = os.path.splitext(rel_filename)
                dst_pathname = os.path.join(dst_dir, rel_filename + ".mp3")
                dst_subdir = os.path.dirname(dst_pathname)

                mkdir(args, dst_subdir)

                if not os.path.isfile(dst_pathname):
                    process_file(args, ext, src_pathname, dst_pathname)

    return 0


if __name__ == "__main__":
    sys.exit(main())
