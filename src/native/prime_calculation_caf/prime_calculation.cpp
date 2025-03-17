#include <caf/all.hpp>
#include <chrono>
#include <cmath>
#include <iostream>

// Function to check if a number is prime
bool is_prime(int n) {
  if (n < 2)
    return false; // Numbers less than 2 are not prime
  if (n == 2)
    return true; // 2 is the only even prime number
  int max = static_cast<int>(
      std::sqrt(n)); // Determine upper limit for divisibility check
  for (int i = 2; i <= max; ++i) {
    if (n % i == 0)
      return false; // If n is divisible by any number in range, it's not prime
  }
  return true; // If no divisors were found, n is prime
}

// Actor behavior for prime calculation
caf::behavior prime_calculator(caf::event_based_actor *self) {
  return {[=](int max) -> int {
    int prime_count = 0;
    for (int n = 2; n <= max; ++n) {
      if (is_prime(n)) {
        ++prime_count; // Increment count if n is prime
      }
    }
    return prime_count; // Return total prime count
  }};
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <range>" << std::endl;
    return 1; // Exit with error if no range argument is provided
  }

  int range = std::stoi(argv[1]); // Convert input argument to integer

  // Start CAF actor system
  caf::actor_system_config cfg;
  caf::actor_system system{cfg};

  // Spawn the prime calculator actor
  auto calculator = system.spawn(prime_calculator);

  // Measure execution time
  auto start = std::chrono::high_resolution_clock::now();

  // Create a scoped actor to interact with the prime calculator
  caf::scoped_actor self{system};

  int prime_count;
  self->request(calculator, std::chrono::seconds(300), range)
      .receive(
          [&](int result) { prime_count = result; }, // Receive and store result
          [&](caf::error &err) {
            std::cerr << "Error: " << to_string(err) << std::endl;
            exit(1); // Handle errors and exit on failure
          });

  auto end = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed = end - start; // Compute elapsed time

  // Print the results
  std::cout << "Found " << prime_count << " prime numbers in range 1 to "
            << range << "\n";
  std::cout << "Time elapsed: " << elapsed.count() << " seconds.\n";

  return 0; // Exit successfully
}

