#!/usr/bin/env python

import fileinput
import json

if __name__ == "__main__":
    filedict = {"files": {}}
    for line in fileinput.input():
        line = line.split()
        # TODO: would have preferred to use re here; more robust.
        percentage, filename = line[0], line[3]
        prefix = "src/"
        if filename.startswith(prefix):
            filename = filename[len(prefix) :]
            filedict["files"][filename] = percentage
        else:
            filedict["total"] = percentage

    json_object = json.dumps(filedict, sort_keys=True, indent=4)
    print(json_object)
