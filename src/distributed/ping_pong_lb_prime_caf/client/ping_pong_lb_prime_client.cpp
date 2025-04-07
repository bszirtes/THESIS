#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <caf/send.hpp>
#include <iostream>

using namespace caf;

// Client behavior: Sends a predefined number of messages to the
// load balancer and waits for the number of the found primes in the sent range.
// It notifies the main actor upon completion.
behavior client_fun(event_based_actor *self, const actor &load_balancer,
                    const size_t num_messages, int prime_range,
                    const bool verbose) {
  size_t responses_received = 0;

  return {[=](atom_value msg) mutable {
            if (msg == atom("start")) {
              self->send(load_balancer, self, prime_range);
            }
          },
          [=](int found_primes) mutable {
            ++responses_received;
            if (verbose)
              aout(self) << "Client <" << self->id()
                         << "> received: " << found_primes << std::endl;
            if (responses_received == num_messages) {
              if (verbose)
                aout(self) << "Client <" << self->id()
                           << "> reached max messages, terminating."
                           << std::endl;
              self->quit();
            } else {
              self->send(load_balancer, self, prime_range);
            }
          }};
}

// Define config structure
struct config : public actor_system_config {
  std::string server_address = "localhost";
  int server_port = 4242;
  int num_messages = 10;
  int prime_range = 50000;
  bool verbose = false;

  config() {
    opt_group{custom_options_, "global"}
        .add(server_address, "server-address", "Server address")
        .add(server_port, "server-port", "Server port")
        .add(num_messages, "messages", "Number of messages to send")
        .add(prime_range, "range", "Range for prime calculation")
        .add(verbose, "verbose", "Verbose output");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  if (cfg.verbose)
    std::cout << "Client connecting to '" << cfg.server_address << ":"
              << cfg.server_port << "' to send " << cfg.num_messages
              << " messages" << std::endl;

  // Connect to the server
  auto server =
      sys.middleman().remote_actor(cfg.server_address, cfg.server_port);
  if (!server) {
    std::cerr << "Failed to connect to server: " << to_string(server.error())
              << std::endl;
    std::exit(1);
  }

  // Spawn client actor
  auto client_actor = sys.spawn(client_fun, *server, cfg.num_messages,
                                cfg.prime_range, cfg.verbose);
  anon_send(client_actor, atom("start"));
}

// Enable CAF's network capabilities
CAF_MAIN(io::middleman)
