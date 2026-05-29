#!/usr/bin/env python3
import json
import sys

if len(sys.argv) < 3:
    print("Usage: merge_compdb.py output.json input1.json [input2.json ...]", file=sys.stderr)
    sys.exit(1)

output_file = sys.argv[1]
input_files = sys.argv[2:]

merged = []
aliases = []
for input_file in input_files:
    with open(input_file) as f:
        data = json.load(f)
        if isinstance(data, list):
            merged.extend(data)
        elif isinstance(data, dict) and isinstance(data.get("aliases"), list):
            aliases.extend(data["aliases"])
        else:
            print(f"Warning: {input_file} is not a JSON array", file=sys.stderr)

for alias in aliases:
    source = alias.get("from")
    target = alias.get("to")
    if not source or not target:
        print("Warning: malformed compile command alias", file=sys.stderr)
        continue

    source_entry = None
    for entry in merged:
        if entry.get("file") == source and " -c " in entry.get("command", ""):
            source_entry = entry
            break
    if source_entry is None:
        for entry in merged:
            if entry.get("file") == source:
                source_entry = entry
                break
    if source_entry is None:
        print(f"Warning: compile command alias source not found: {source}", file=sys.stderr)
        continue

    target_entry = dict(source_entry)
    target_entry["file"] = target
    if "command" in target_entry:
        target_entry["command"] = target_entry["command"].replace(source, target)
    merged.append(target_entry)

with open(output_file, 'w') as out:
    json.dump(merged, out, indent=2)
