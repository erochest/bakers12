#!/usr/bin/env python3


"""This takes the output of bakers12 tokenizer and cleans it up to match the
output of bin/readAna.hs.
"""

import csv
import os
import sys


def clean_row(row):
    (token, _, filename, offset, length, _) = row
    basename = os.path.basename(filename)
    return (token, basename, offset, length)


def main(argv=None):
    argv = argv or sys.argv[1:]
    for filename in argv:
        with open(filename, 'r') as fin:
            with open(filename + '.clean', 'w') as fout:
                reader = csv.reader(fin)
                writer = csv.writer(fout)
                writer.writerows(map(clean_row, reader))


if __name__ == '__main__':
    main()

