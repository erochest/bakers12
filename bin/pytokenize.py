#!/usr/bin/env python3


from collections import namedtuple
import csv
import itertools
import os
import re
import sys


Token = namedtuple('Token', 'text source raw offset length')
reTOKEN = re.compile(r"\w+('+\w+)?")


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


def normalize(raw):
    return "'".join(raw.lower().split("'"))


def tokenize(filename):
    stream_start = 0
    with open(filename) as fin:
        for line in fin:
            for match in reTOKEN.finditer(line):
                raw = match.group(0)
                (start, end) = match.span()

                yield Token(
                        text=normalize(raw),
                        raw=raw,
                        source=filename,
                        offset=start+stream_start,
                        length=end-start,
                        )

            stream_start += len(line)


def type_to_token_ratio(tokens):
    count = 0.0
    types = set()

    for token in tokens:
        count += 1.0
        types.add(token.text)

        yield (token, len(types) / count)


def output_token(token_ratio):
    (token, ratio) = token_ratio
    return [
            token.text,
            token.raw,
            token.source,
            token.offset,
            token.length,
            ratio,
            ]


def main(argv=None):
    argv = sys.argv[1:] if argv is None else argv

    output = csv.writer(sys.stdout)

    files = get_files(argv)
    tokens = itertools.chain.from_iterable(map(tokenize, files))
    token_ratios = type_to_token_ratio(tokens)
    rows = map(output_token, token_ratios)

    output.writerows(rows)


if __name__ == '__main__':
    main()

