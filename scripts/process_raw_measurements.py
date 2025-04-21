import pandas as pd
import argparse

def remove_outliers(series):
    """Removes outliers using IQR method."""
    Q1 = series.quantile(0.25)
    Q3 = series.quantile(0.75)
    IQR = Q3 - Q1
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    return series[(series >= lower_bound) & (series <= upper_bound)]

def process_csv(file_path, output_path, num_trials=10):
    """Processes a CSV file, removes outliers, selects the best trials, and groups by Module, Function, and Parameter."""
    # Load data
    df = pd.read_csv(file_path)

    # Fill empty 'Function' values with empty string or placeholder
    df['Function'] = df['Function'].fillna('').astype(str)

    # Drop rows with zero or negative runtime or consumption
    df = df[(df['Runtime (s)'] > 0) & (df['Consumption (μJ)'] > 0)].copy()

    # Group by Parameter and process best trials
    grouped = df.groupby('Parameter')
    filtered_data = []

    for param, group in grouped:
        group = group.copy()
        group['Consumption (μJ)'] = remove_outliers(group['Consumption (μJ)'])
        group = group.dropna(subset=['Consumption (μJ)'])

        if len(group) > num_trials:
            diff = (group['Consumption (μJ)'] - group['Consumption (μJ)'].median()).abs()
            best_indices = diff.nsmallest(num_trials).index
            group = group.loc[best_indices]

        if not group.empty:
            filtered_data.append(group)

    if not filtered_data:
        print("No valid data after filtering.")
        return

    # Combine filtered trials
    result_df = pd.concat(filtered_data)

    # Drop unneeded columns
    result_df = result_df.drop(columns=['Sample frequency (ns)', 'File'])

    # Group by Module, Function, and Parameter, then average numeric fields
    grouped_avg = result_df.groupby(['Module', 'Function', 'Parameter'], dropna=False, as_index=False).mean()

    # Round Samples to whole number
    grouped_avg['Samples'] = grouped_avg['Samples'].round(0).astype(int)

    # Save to output
    grouped_avg.to_csv(output_path, index=False)
    print(f"Processed measurement data saved to {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process CSV data, remove outliers, select best trials, and group averages.")
    parser.add_argument("input_file", help="Input CSV file path")
    parser.add_argument("output_file", help="Output CSV file path")
    parser.add_argument("--num_trials", type=int, default=10, help="Number of best trials to select (default: 10)")
    args = parser.parse_args()

    process_csv(args.input_file, args.output_file, args.num_trials)

