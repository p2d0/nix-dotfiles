#!/usr/bin/env python3

import re
import sys

f = open(sys.argv[1])

patt = re.compile(r'^\d\d:\d\d:\d\d', re.M)
dup_line = ''
for line in f:
    # line = f.readline()

    # Find a line starting with a time stamp: 00:13:23 ...
    res = re.findall(patt, line)
    if res:
        # If so, print this line and read the next line which we save to
        # store the result in dup_line.
        # In the next loop, If we find another sections starting with a timestamp,
        # the dup_line will be matched with the line below. If True, just pass
        # and do not print the duplicated line
        # Else, read another pattern to match a duplicated line
        print(line, end='')
        next_line = f.readline()
        if dup_line and next_line == dup_line:
            dup_line = ''
            res = []
            continue
        else:
            dup_line = next_line
            print(dup_line, end='')
            res = []
    else:
        print(line, end='')

f.close()
