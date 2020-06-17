# Scallion python parser
Parser for Python syntax built using [Scallion](https://github.com/epfl-lara/scallion)

## Usage

Run the lexer
```
sbt:spp> run tokenize examples/django_request.py
```

Run the parser with raw display
```
sbt:spp> run parse examples/django_request.py
```

Run the parser with pretty printing
```
sbt:spp> run pprint examples/pprint_test.py
```

### Measure execution time

Run the parser on an entire folder and display execution time
```
sbt:spp> run parsedir <folder_path>
```

Run CPython on an entire folder and display execution time
```
$ python python/performance_measurement.py <folder_path>
```

### Compare produced tree against CPython

First, we have to export the tree to JSON with Python
```
$ python python/tree_serializer.py json examples/django_request.py examples/django_request.json
```

After that, we can run the comparison from SBT. The tree produced
by the Scala implementation is displayed, along with the result of the comparison.
Both JSON files are stored in `debug/ref.json` and `debug/output.json` and
can be compared manually as well.
```
run compare examples/pprint_test.py examples/django_request.json
```

Some examples of files from well-known open source projects such as Djanog, Tensorflow, Pytorch
and Flask are available in the `examples/` folder.