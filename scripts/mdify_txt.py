#!/usr/bin/env python3
from argparse import ArgumentParser
from shutil import copyfileobj
from sys import stdout

def get_args():
    parser = ArgumentParser()

    parser.add_argument("file", metavar="File",
        help="markdown file")
    parser.add_argument("title", metavar="Title", default="", nargs='?',
        help="title for markdown page")
    parser.add_argument("--layout", default="default",
        help="layout for markdown page")

    return parser.parse_args()

def main():
    args = get_args()

    str1 = """\
---
title: {}
layout: {}
---
[Download]({})

```lisp""".format(args.title, args.layout, args.file)
    str2 = "```\n"

    print(str1)
    with open(args.file, "r") as f:
        copyfileobj(f, stdout)
    print(str2)

if __name__ == "__main__":
    main()
