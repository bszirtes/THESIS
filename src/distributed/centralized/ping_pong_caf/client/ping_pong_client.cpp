#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <chrono>  // For std::chrono::milliseconds
#include <cstdlib> // For std::atoi
#include <iostream>
#include <thread> // For std::this_thread::sleep_for

using namespace caf;

// Define client behavior with explicit wait
behavior ping_actor(event_based_actor *self, actor server, int max_pings,
                    int delay_ms) {
  int remaining_pings = max_pings;
  self->send(server, "ping", self);

  return {[=](const std::string &msg, actor server) mutable {
    if (msg == "pong" && remaining_pings > 1) {
      aout(self) << "Client received: " << msg << "\n";
      --remaining_pings;

      // Explicit wait before sending the next ping (blocking)
      std::this_thread::sleep_for(std::chrono::milliseconds(delay_ms));

      // Send the next ping after the delay
      self->send(server, "ping", self);
    } else {
      aout(self) << "Client reached max pings (" << max_pings
                 << "), terminating.\n";
      self->quit();
    }
  }};
}

// Define config structure
struct config : public actor_system_config {
  int max_pings = 10;  // Default number of ping messages
  int delay_ms = 1000; // Default delay (1000ms = 1 second)

  config() {
    opt_group{custom_options_, "global"}
        .add(max_pings, "pings,p", "Number of ping messages to send")
        .add(delay_ms, "delay,d", "Delay (milliseconds) between messages");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  int max_pings = cfg.max_pings;
  int delay_ms = cfg.delay_ms;

  auto server_host = "localhost"; // Change to "ping-pong-server" in Kubernetes
  auto server_port = uint16_t{4242};

  // Connect to the server
  auto server = sys.middleman().remote_actor(server_host, server_port);
  if (!server) {
    std::cerr << "Failed to connect to server: " << to_string(server.error())
              << std::endl;
    return;
  }

  // Start client actor with delay
  sys.spawn(ping_actor, *server, max_pings, delay_ms);
}

// Enable CAF's network capabilities
CAF_MAIN(io::middleman)
