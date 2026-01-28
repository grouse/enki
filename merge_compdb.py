#!/usr/bin/env python3
import json
import sys

if len(sys.argv) < 3:
    print("Usage: merge_compdb.py output.json input1.json [input2.json ...]", file=sys.stderr)
    sys.exit(1)

output_file = sys.argv[1]
input_files = sys.argv[2:]

merged = []
for input_file in input_files:
    with open(input_file) as f:
        data = json.load(f)
        if isinstance(data, list):
            merged.extend(data)
        else:
            print(f"Warning: {input_file} is not a JSON array", file=sys.stderr)

with open(output_file, 'w') as out:
    json.dump(merged, out, indent=2)
