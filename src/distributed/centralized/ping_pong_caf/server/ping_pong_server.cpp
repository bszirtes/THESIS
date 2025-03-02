#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <iostream>

using namespace caf;

// Define the server behavior
behavior ping_pong_server(event_based_actor *self) {
  return {[=](const std::string &msg, actor client) {
    if (msg == "ping") {
      aout(self) << "Server received: " << msg << "\n";
      self->send(client, "pong", self);
    }
  }};
}

// Define config structure to allow parameterization
struct config : public actor_system_config {
  int port = 4242;    // Default port
  int max_conns = 10; // Default max connections

  config() {
    opt_group{custom_options_, "global"}
        .add(port, "port", "Port number for the server")
        .add(max_conns, "max-conns", "Maximum concurrent connections");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  // Read parameters from the config
  int port = cfg.port;
  int max_conns = cfg.max_conns;

  std::cout << "Starting server on port " << port
            << " with max connections: " << max_conns << std::endl;

  // Spawn the server actor
  auto server = sys.spawn(ping_pong_server);

  // Publish the server actor on the given port
  auto res = sys.middleman().publish(server, port, nullptr, max_conns);

  if (!res) {
    std::cerr << "Failed to publish server: " << to_string(res.error())
              << std::endl;
  } else {
    std::cout << "Server is running on port " << *res << std::endl;
  }
}

// Enable CAF's network capabilities
CAF_MAIN(io::middleman)
