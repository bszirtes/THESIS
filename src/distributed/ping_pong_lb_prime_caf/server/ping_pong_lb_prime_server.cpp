#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <iostream>

using namespace caf;

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

// Function to calculate primes in a given range
int find_primes(int range) {
  int prime_count = 0;
  for (int n = 2; n <= range; ++n) {
    if (is_prime(n)) {
      ++prime_count;
    }
  }
  return prime_count;
}

// Server behavior: Processes messages from clients and responds with the found
// number of primes in the received range. The server notifies the main actor
// upon termination.
behavior server_fun(event_based_actor *self, const actor &load_balancer,
                    const bool verbose) {
  // Register with the load balancer
  self->send(load_balancer, self, atom("register"));

  return {[=](const actor &client, int prime_range) {
            if (verbose)
              aout(self) << "Server <" << self->id()
                         << "> received: " << prime_range << std::endl;
            self->send(client, find_primes(prime_range));
          },
          [=](atom_value msg) {
            if (msg == atom("stop")) {
              if (verbose)
                aout(self) << "Server <" << self->id() << "> terminating."
                           << std::endl;
              self->quit();
            }
          }};
}

// Configuration structure for setting up the actor system parameters
struct config : public actor_system_config {
  int port = 4243;
  std::string load_balancer_address = "localhost";
  int load_balancer_port = 4242;
  bool verbose = false;

  config() {
    opt_group{custom_options_, "global"}
        .add(port, "port", "Server port")
        .add(load_balancer_address, "lb-address", "Load balancer address")
        .add(load_balancer_port, "lb-port", "Load balancer port")
        .add(verbose, "verbose", "Verbose output");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  // Connect to load balancer
  auto load_balancer = sys.middleman().remote_actor(cfg.load_balancer_address,
                                                    cfg.load_balancer_port);
  if (!load_balancer) {
    std::cerr << "Failed to connect to load balancer: "
              << to_string(load_balancer.error()) << std::endl;
    std::exit(1);
  }

  // Spawn server actor
  auto server = sys.spawn(server_fun, *load_balancer, cfg.verbose);

  // Publish the server actor on the given port
  auto res = sys.middleman().publish(server, cfg.port);
  if (!res) {
    std::cerr << "Failed to publish server: " << to_string(res.error())
              << std::endl;
    std::exit(1);
  } else {
    std::cout << "Server is running on port " << *res
              << " and connected to the load balancer" << std::endl;
  }
}

// Enable CAF's network capabilities
CAF_MAIN(io::middleman)
