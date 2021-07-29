#!/usr/bin/env python

import sys
import fileinput
import json
import re

if __name__ == "__main__":
    filedict = {}
    p = re.compile(" *(\d+\.\d+) *% *\d+/\d+ *([a-zA-Z](.*))")

    for line in fileinput.input():
        match_object = re.search(p, line)
        percentage, name = float(match_object.group(1)), match_object.group(2)
        if name == "Project coverage":
            filedict["TOTAL"] = float(match_object.group(1))
        else:
            filedict[name[len("src/") :]] = float(match_object.group(1))

    json_object = json.dumps(filedict, sort_keys=True, indent=4)
    print(json_object)
