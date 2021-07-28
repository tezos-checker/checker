#!/usr/bin/env python

import fileinput
import json

if __name__ == "__main__":
    filedict = {}
    for line in fileinput.input():
        line = line.split()
        percentage, filename = line[0], line[3]
        percentage = float(percentage)
        prefix = "src/"
        if filename.startswith(prefix):
            filename = filename[len(prefix) :]
        else:
            filename = "TOTAL"
        filedict[filename] = percentage

    json_object = json.dumps(filedict, sort_keys=True, indent=4)
    print(json_object)
