#include <caf/all.hpp>
#include <chrono>
#include <cmath>
#include <iostream>

// Function to check if a number is prime
bool is_prime(int n) {
  if (n < 2)
    return false;
  if (n == 2)
    return true;
  int max = static_cast<int>(std::sqrt(n));
  for (int i = 2; i <= max; ++i) {
    if (n % i == 0)
      return false;
  }
  return true;
}

// Actor behavior for prime calculation
caf::behavior prime_calculator(caf::event_based_actor *self) {
  return {[=](int max) -> int {
    int prime_count = 0;
    for (int n = 2; n <= max; ++n) {
      if (is_prime(n)) {
        ++prime_count;
      }
    }
    return prime_count;
  }};
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <range>" << std::endl;
    return 1;
  }

  int range = std::stoi(argv[1]);

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
      .receive([&](int result) { prime_count = result; },
               [&](caf::error &err) {
                 std::cerr << "Error: " << to_string(err) << std::endl;
                 exit(1);
               });

  auto end = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed = end - start;

  // Print the results
  std::cout << "Found " << prime_count << " prime numbers in range 1 to "
            << range << "\n";
  std::cout << "Time elapsed: " << elapsed.count() << " seconds.\n";

  return 0;
}
