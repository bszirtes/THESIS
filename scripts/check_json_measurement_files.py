import argparse
import csv
import os

def check_json_files(results_file, dir="measurements"):
    """Checks for the presence of JSON files listed in the CSV results file."""
    missing = []
    found = []

    with open(results_file, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            json_filename = row["File"] + ".json"
            json_path = os.path.join(dir, json_filename)

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

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Check for missing JSON files listed in a CSV file.")
    parser.add_argument("results_file", help="CSV file listing result entries")
    parser.add_argument("--dir", default="measurements", help="Directory containing JSON files (default: 'measurements')")
    args = parser.parse_args()

    check_json_files(args.results_file, args.dir)

