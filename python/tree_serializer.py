import ast
import astpretty
import sys
import json

import time

IGNORED_FIELDS = [
    "lineno", 
    "col_offset", 
    "end_lineno", 
    "end_col_offset",
    "type_ignores",
    "ctx",
    "type_comment",
    "kind",
]

class CustomEncoder(json.JSONEncoder):
    def default(self, o):
        d = {
            field : val for (field, val) in o.__dict__.items()
            if field not in IGNORED_FIELDS
        }

        d["nodeName"] = type(o).__name__

        if isinstance(o, ast.Constant):
            d["type"] = type(o.value).__name__
            if isinstance(o.value, complex):
                d["value"] = o.value.imag
            if isinstance(o.value, bytes):
                d["value"] = o.value.decode("utf-8")
            if o.value == None:
                d["value"] = None
            if o.value == Ellipsis:
                d["value"] = "..."


        return d

def main(argv):
    with open(argv[1], 'r') as f:
        start = time.time()

        tree = ast.parse(f.read())
        end = time.time()
        print(end - start)

        if (argv[0] == "pprint"):
            astpretty.pprint(tree)
        elif (argv[0] == "json"):
            json_str = serialize(tree)

            with open(argv[2], 'w') as output_f:
                output_f.write(json_str)


def serialize(module: ast.Module) -> str:
    return json.dumps(module, cls=CustomEncoder, indent=2)

if __name__ == "__main__":
    main(sys.argv[1:])