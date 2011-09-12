#!/usr/bin/env python3

"""\
This walks a directory tree and strips the tags from all the XML files it finds
there. It saves the data as a text files alongside the original XML file.
"""


import argparse
from collections import deque
import datetime
import os
import sys
import time
from xml.etree import cElementTree as ET


if sys.platform == 'win32':
    timer = time.clock
else:
    timer = time.time


def strip_tags(input, output):
    """This strips the tags from input and writes the text to output. """
    print('{0} => {1}'.format(input, output))

    try:
        xml = ET.parse(input)
    except Exception as exc:
        print('Error: {0}'.format(exc), file=sys.stderr)
    else:
        with open(output, 'w') as out:
            _strip_tags(xml.getroot(), out)


def _strip_tags(node, out):
    """This strips the tags from node, writing the text to out. """
    if node.text: out.write(node.text)

    for child in node:
        _strip_tags(child, out)

    if node.tail: out.write(node.tail)


def get_files(args):
    """\
    This takes input arguments and yields the files, including the files in the
    directories.

    """

    for path in args:
        path = os.path.abspath(path)
        if os.path.isfile(path):
            yield path
        else:
            for (root, _, files) in os.walk(path):
                for fn in files:
                    yield os.path.join(root, fn)


def is_xml(filename):
    """Returns true if it is an XML file. """
    return filename.lower().endswith('.xml')


def get_text(filename):
    """Returns filename with the extension changed to .txt. """
    (base, _) = os.path.splitext(filename)
    return base + '.txt'


def parse_args(argv):
    """This parses the command-line arguments in argv. """
    cwd = os.getcwd()
    ap = argparse.ArgumentParser(description=__doc__)

    ap.add_argument('files', metavar='FILES/DIRS', nargs='*',
                    help='the files or directories to process '
                         '(default: {0})'.format(cwd))

    args = ap.parse_args(argv)
    if not args.files:
        args.files = [cwd]

    return args


def main(argv=None):
    args = parse_args(argv or sys.argv[1:])
    start = timer()

    files = ( (fn, get_text(fn)) for fn in get_files(args.files) if is_xml(fn) )
    for (xml, txt) in files:
        strip_tags(xml, txt)

    end = timer()
    elapsed = datetime.timedelta(seconds=end-start)
    print('done.')
    print('elapsed time: {0}'.format(elapsed))


if __name__ == '__main__':
    sys.exit(main())

