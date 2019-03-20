# csv2es

Convert CSV to ElasticSearch Bulk Index Format

## Build & run

```bash
rebar3 escriptize
_build/default/bin/csv2es --help
```

## CSV format

CSV file format.

```csv
Row 1: # This is a comment
Row 2: field name 1;field name 2;...;...
Row 3: value 1;value 2;...;...
Row 4: value 3;value 4;...;...
````

## Example

This example assumes that ElasticSerach is running on localhost and port 9200.

Create the bulk file:

```bash
_build/default/bin/csv2es -c example/csv.csv bulk-file
```

Create ES index:

Alt 1 - using a mapping file:

```bash
curl -s -H "Content-Type: application/json" -XPUT localhost:9200/csv2es --data-binary @example/mapping.json
```

Alt 2 - make ES do some guesswork:

```bash
curl -s -H "Content-Type: application/json" -XPUT localhost:9200/csv2es --data-binary @example/numeric_detection.json
```

Load the bulk file:

```bash
curl -s -H "Content-Type: application/x-ndjson" -XPOST localhost:9200/_bulk --data-binary @bulk-file
```

Query:

```bash
curl -XGET 'http://127.0.0.1:9200/csv2es/_search?pretty&size=10'
```

In the response the fields "value1" and "value2" might look like strings but they aren't.