import sys
import csv
import os

results_file = sys.argv[1]

if len(sys.argv) > 2:
    json_dir = sys.argv[2]
else:
    json_dir = "measurements"

missing = []
found = []

with open(results_file, newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        json_filename = row["File"] + ".json"
        json_path = os.path.join(json_dir, json_filename)

        if os.path.isfile(json_path):
            found.append(json_filename)
        else:
            missing.append(json_filename)

print("\n=== Found JSON files ===")
for f in found:
    print(f)

print("\n=== Missing JSON files ===")
for f in missing:
    print(f)

print(f"\nChecked {len(found) + len(missing)} entries: {len(found)} found, {len(missing)} missing.")

