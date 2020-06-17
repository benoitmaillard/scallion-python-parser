import ast
import sys
import os

import time

def main(argv):
    parse_dir(argv[1])

def parse(path):
    with open(path, 'r') as f:
        start = time.time()

        tree = ast.parse(f.read())
        end = time.time()
        print(end - start)

def parse_dir(path):
    start = time.time()
    # code from https://stackoverflow.com/questions/18394147/recursive-sub-folder-search-and-return-files-in-a-list-python
    result = [os.path.join(dp, f) for dp, dn, filenames in os.walk(path) for f in filenames if os.path.splitext(f)[1] == '.py']

    
    for x in result:
        parse(x)

    end = time.time()
    print(end - start)

if __name__ == "__main__":
    main(sys.argv[1:])