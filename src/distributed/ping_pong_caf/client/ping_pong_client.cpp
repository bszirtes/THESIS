#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <caf/send.hpp>
#include <chrono>  // For std::chrono::milliseconds
#include <cstdlib> // For std::atoi
#include <iostream>
#include <thread> // For std::this_thread::sleep_for

using namespace caf;

// Client behavior: Sends a predefined number of "ping" messages to the
// server with optional delay and waits for "pong" responses.
behavior client_fun(event_based_actor *self, const actor &server,
                    const size_t num_messages, const int delay_ms,
                    const bool verbose) {
  size_t responses_received = 0;

  return {[=](atom_value msg) mutable {
    if (msg == atom("start")) {
      self->send(server, self, atom("ping"));
    }
    if (msg == atom("pong")) {
      ++responses_received;
      if (verbose)
        aout(self) << "Client received: " << to_string(msg) << std::endl;
      if (responses_received == num_messages) {
        if (verbose)
          aout(self) << "Client reached max pings, terminating." << std::endl;
        self->quit();
      } else {
        // Explicit wait before sending the next ping
        std::this_thread::sleep_for(std::chrono::milliseconds(delay_ms));
        self->send(server, self, atom("ping"));
      }
    }
  }};
}

// Define config structure
struct config : public actor_system_config {
  std::string server_address = "localhost";
  int server_port = 4242;
  int num_messages = 10;
  int delay_ms = 1000;
  bool verbose = false;

  config() {
    opt_group{custom_options_, "global"}
        .add(server_address, "server-address", "Server address")
        .add(server_port, "server-port", "Server port")
        .add(num_messages, "messages", "Number of messages to send")
        .add(delay_ms, "delay", "Delay (milliseconds) between messages")
        .add(verbose, "verbose", "Verbose output");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  if (cfg.verbose)
    std::cout << "Client connecting to '" << cfg.server_address << ":"
              << cfg.server_port << "' to send " << cfg.num_messages
              << " messages with " << cfg.delay_ms << "ms delay..."
              << std::endl;

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
                                cfg.delay_ms, cfg.verbose);
  anon_send(client_actor, atom("start"));
}

// Enable CAF's network capabilities
CAF_MAIN(io::middleman)
