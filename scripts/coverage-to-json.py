#!/usr/bin/env python

import sys
import fileinput
import json
import re

if __name__ == "__main__":
    filedict = {}
    # Input looks like this:
    #
    #    27.16 %     85/313    src/avl.ml
    #    96.85 %    246/254    src/burrow.ml
    #   ...
    #   100.00 %      4/4      src/ptr.ml
    #    81.86 %   1859/2271   Project coverage
    #
    # So group(1) holds the percentage, group(2)/group(3) is the ratio, and
    # group(4) captures the filename (or "Project coverage" in the last line)
    p = re.compile("\s*(\d+\.\d+)\s*%\s*(\d+)/(\d+)\s*([a-zA-Z](.*))")

    for line in fileinput.input():
        match_object = re.search(p, line)
        if match_object:
            percentage = float(match_object.group(1))
            filename = match_object.group(4)
            prefix = "src/"
            if filename.startswith(prefix):
                # an ocaml file
                filename = filename[len(prefix) :]
            elif filename == "Project coverage":
                # the summary for the entire project
                filename = "TOTAL"
            else:
                sys.exit("malformed filename in coverage report: {}".format(filename))
        else:
            sys.exit("malformed line in coverage report: {}".format(line))

        filedict[filename] = percentage

    json_object = json.dumps(filedict, sort_keys=True, indent=4)
    print(json_object)
