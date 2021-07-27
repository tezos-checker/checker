#!/usr/bin/env python

import fileinput
import json


def remove_prefix_if_possible(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix) :]
    else:
        return text


if __name__ == "__main__":
    filedict = {}
    for line in fileinput.input():
        line = line.split()
        percentage, filename = line[0], line[3]
        if filename.startswith("src/"):
            filename = remove_prefix_if_possible(filename, "src/")
        else:
            filename = "TOTAL"
        filedict[filename] = percentage

    json_object = json.dumps(filedict, sort_keys=True, indent=4)
    print(json_object)
