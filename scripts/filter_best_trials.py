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
    """Processes a CSV file, removes outliers, and selects the best trials."""
    # Load data
    df = pd.read_csv(file_path)

    # Drop rows with zero or negative runtime
    df = df[df['Runtime (s)'] > 0].copy()

    # Drop rows with zero or negative consumption
    df = df[df['Consumption (μJ)'] > 0].copy()

    # Group by 'Parameter'
    grouped = df.groupby('Parameter')
    filtered_data = []

    for param, group in grouped:
        # Remove outliers in 'Runtime (s)'
        group.loc[:, 'Runtime (s)'] = remove_outliers(group['Runtime (s)'])
        group = group.dropna(subset=['Runtime (s)'])

        # Remove outliers in 'Average (μW)'
        group.loc[:, 'Average (μW)'] = remove_outliers(group['Average (μW)'])
        group = group.dropna(subset=['Average (μW)'])

        if len(group) > num_trials:
            # Select the 'num_trials' trials closest to the median 'Consumption (μJ)'
            diff = (group['Consumption (μJ)'] - group['Consumption (μJ)'].median()).abs()
            best_indices = diff.nsmallest(num_trials).index
            group = group.loc[best_indices]
        elif len(group) > 0:
            #if less than num_trials but more than zero, keep what is left.
            pass
        else:
            #if group is empty, continue to next group
            continue

        filtered_data.append(group)

    # Combine results and save to new CSV
    result_df = pd.concat(filtered_data)
    result_df = result_df.sort_values(by='Parameter')
    result_df.to_csv(output_path, index=False)
    print(f"Filtered and sorted data saved to {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process CSV data, remove outliers, and select best trials.")
    parser.add_argument("input_file", help="Input CSV file path")
    parser.add_argument("output_file", help="Output CSV file path")
    parser.add_argument("--num_trials", type=int, default=10, help="Number of best trials to select (default: 10)")
    args = parser.parse_args()

    process_csv(args.input_file, args.output_file, args.num_trials)
