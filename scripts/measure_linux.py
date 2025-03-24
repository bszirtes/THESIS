import argparse
import csv
import subprocess
import sys
import time
import json
import timeit
import datetime
import os

def create_measurement_folder(folder_name, verbose):
    current_directory = os.getcwd()
    folder_path = os.path.join(current_directory, folder_name)
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)
        if verbose:
            print(f"[DEBUG] Folder '{folder_name}' created")
    else:
        if verbose:
            print(f"[DEBUG] Folder '{folder_name}' already exists")

def run_scaphandre(folder_name, file_name, regex, nano, container, verbose):
    if nano > 0:
        if container:
            command = f"scaphandre json --step 0 --step-nano {nano} --containers --container-regex={container} -f {folder_name}/{file_name}.json"
        else:
            command = f"scaphandre json --step 0 --step-nano {nano} --process-regex={regex} -f {folder_name}/{file_name}.json"
    else:
        if container:
            command = f"scaphandre json --step 1 --containers --container-regex={container} -f {folder_name}/{file_name}.json"
        else:
            command = f"scaphandre json --step 1 --process-regex={regex} -f {folder_name}/{file_name}.json"
    if verbose:
        print(f"[DEBUG] Running Scaphandre command: {command}")
    return subprocess.Popen(command, stdout=subprocess.PIPE if verbose else subprocess.DEVNULL,
                            stderr=subprocess.PIPE if verbose else subprocess.STDOUT, shell=True)

# TODO: Make this optional with an argument
def compile_program(program_exe, module):
    if program_exe == "beam.smp" or program_exe == "erlang":
        print(f"[INFO] Compiling Erlang module: {module}.erl")
        subprocess.run(f"erlc {module}.erl", shell=True)
    else:
        print(f"[INFO] Compiling C++ program: {module}.cpp")
        subprocess.run("cmake .", shell=True)
        subprocess.run("make", shell=True)

def run_program(module, function, parameter, program_exe, container, index):
    if program_exe == "beam.smp" and not container:
        erl_command = f"erl -noshell -run {module} {function} {parameter} -s init stop"
        print(f"[INFO] Running Erlang command: {erl_command}")
        return erl_command
    elif program_exe == "erlang":
        distributed_erl_command = f"erl -noshell -sname 'runner{index}@localhost' -setcookie cookie -run {module} {function} {parameter} -s init stop"
        print(f"[INFO] Running Distributed Erlang command: {distributed_erl_command}")
        return distributed_erl_command
    elif container:
        distributed_cpp_command = f"./{module}.sh {parameter}"
        print(f"[INFO] Running Distributed C++ command: {distributed_cpp_command}")
        return distributed_cpp_command
    else:
        cpp_command = f"./{module}.out {parameter}"
        print(f"[INFO] Running C++ command: {cpp_command}")
        return cpp_command

def measure_energy_consumption(module, function, parameters, rep, nano, folder_name, result_file, program_exe, measure_cmd, container, verbose):
    for parameter in parameters:
        for i in range(rep):
            print(f"[INFO] Starting measurement for parameter '{parameter}' (Repetition {i+1}/{rep})")

            # Set file name for Scaphandre output
            current_time = datetime.datetime.now().strftime('%Y-%m-%d-%H-%M-%S')
            file_name = f"{module}__{function}__{current_time}"

            # Compile if necessary and prepare the execution command
            if not measure_cmd:
                if not container and i == 0:
                    compile_program(program_exe, module)
                command = run_program(module, function, parameter, program_exe, container, i)
            else:
                command = measure_cmd
                print(f"[INFO] Using custom command: {command}")

            # Start Scaphandre
            scaphandre_process = run_scaphandre(folder_name, file_name, r"\/" + program_exe, nano, container, verbose)
            time.sleep(5)

            # Run the command and measure the runtime
            print("[INFO] Starting program execution and timing...")
            start_time = timeit.default_timer()
            process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
            process.wait()
            end_time = timeit.default_timer()
            runtime = end_time - start_time
            print(f"[INFO] Execution time: {runtime:.4f} seconds")

            # Kill Scaphandre and any related process
            if verbose:
                print("[DEBUG] Shutting down Scaphandre process")
            scaphandre_process.terminate()
            try:
                scaphandre_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                print("[WARNING] Scaphandre process did not terminate gracefully. Killing it...")
                scaphandre_process.kill()
            subprocess.run('pkill -f scaphandre', shell=True)
            if program_exe == "beam.smp" and not container:
                subprocess.run(f'pkill -f {program_exe}', shell=True)
            time.sleep(5)

            # Process the JSON file generated by Scaphandre
            json_file_path = os.path.join(os.getcwd(), f"{folder_name}/{file_name}.json")
            print(f"[INFO] Reading Scaphandre output from {json_file_path}")

            with open(json_file_path, "r") as file:
                data = json.load(file)

            # Calculate the average energy consumption
            total_consumption = 0.0
            number_samples = 0

            # Print the expected process name
            if verbose and not container:
                print(f"[DEBUG] Expected process name for energy consumption filtering: '{program_exe}'")

            # Iterate through the entries
            for entry in data:
                consumers = entry.get("consumers", [])
                valid_entry = 0
                for consumer in consumers:
                    exe = consumer.get("exe", "").lower()
                    cmdline = consumer.get("cmdline", "")
                    consumption = consumer.get("consumption", 0.0)

                    # Filter out unwanted consumers
                    if (("scaphandre" not in exe and "dash" not in exe) and consumption > 0.0):
                        if verbose:
                            print(f"[DEBUG] Matching process found: '{exe}', cmdline: '{cmdline}', adding consumption: {consumption}")
                        total_consumption += consumption
                        valid_entry = 1
                number_samples += valid_entry

            # Calculate average energy if samples were found
            average_energy = total_consumption / number_samples if number_samples > 0 else 0
            final_consumption = average_energy * runtime

            # Print the final calculated values
            print(f"[INFO] Sum of consumption samples: {total_consumption} μW")
            print(f"[INFO] Number of samples: {number_samples}")
            print(f"[INFO] Average energy per sample: {average_energy} μW")
            print(f"[INFO] Final energy consumption for this run: {final_consumption} μJ")

            # Write results to the CSV file
            output_file = result_file if result_file else f"{program_exe}_{module}"
            if nano == 0: nano = 1000000000
            if not os.path.exists(f"{output_file}.csv"):
                if verbose:
                    print(f"[DEBUG] Output file '{output_file}' created")
                with open(f"{output_file}.csv", 'a', newline='') as csv_file:
                    csv_writer = csv.writer(csv_file, delimiter=',')
                    csv_writer.writerow(["Module", "Function", "Parameter", "Runtime (s)", "Samples", "Average (μW)", "Consumption (μJ)", "Sample frequency (ns)", "File"])
                    csv_writer.writerow([module, function, parameter, runtime, number_samples, average_energy, final_consumption, nano, file_name])
            else:
                if verbose:
                    print(f"[DEBUG] Output file '{output_file}' exists")
                with open(f"{output_file}.csv", 'a', newline='') as csv_file:
                    csv_writer = csv.writer(csv_file, delimiter=',')
                    csv_writer.writerow([module, function, parameter, runtime, number_samples, average_energy, final_consumption, nano, file_name])

            print(f"[INFO] Results saved to {output_file}.csv")
            print("------------------------------")


if __name__ == "__main__":
    # Create the parser
    parser = argparse.ArgumentParser(
        description='CLI to measure energy consumption of a program using Scaphandre',
        epilog='GitHub repository: https://github.com/bszirtes/THESIS')

    # Define arguments
    parser.add_argument('module', type=str, help='module name (without extension for C++)')
    parser.add_argument('function', type=str, help='function name (can be anything for C++)')
    parser.add_argument('parameters', nargs='+', type=str, help='arguments for the function, separated by spaces')
    parser.add_argument('-c', '--cmd', type=str, default='', help='optional command to run the executable directly')
    parser.add_argument('-e', '--exe', type=str, default='beam.smp', help='program process name (e.g., beam.smp or erlang for Erlang or the executable for C++)')
    parser.add_argument('-r', '--rep', type=int, default=1, help='number of repetitions')
    parser.add_argument('-f', '--file', type=str, default='', help='CSV result filename prefix')
    parser.add_argument('--folder', type=str, default='measurements', help='folder to store measurements')
    parser.add_argument('--container', type=str, default='', help='containers to measure (for distributed C++ cases)')
    parser.add_argument('--nano', type=int, default=0, help='amount of nanoseconds between each measurement')
    parser.add_argument('-v', '--verbose', action='store_true', help='enable verbose output')

    # Parse arguments
    args = parser.parse_args()

    # Create folder for measurements
    create_measurement_folder(args.folder, args.verbose)

    # Run the measurement with verbosity option
    measure_energy_consumption(args.module, args.function, args.parameters, args.rep, args.nano, args.folder, args.file, args.exe, args.cmd, args.container, args.verbose)

    # Exit the program
    sys.exit()

