#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <iostream>

using namespace caf;

behavior server_actor(event_based_actor *self, actor load_balancer) {
  // Register with the load balancer
  self->send(load_balancer, atom("register"), self);

  return {[=](const std::string &msg, actor client) {
    if (msg == "ping") {
      aout(self) << "Server received: " << msg << "\n";
      self->send(client, "pong", self);
    }
  }};
}

struct config : public actor_system_config {
  int port = 4243; // Default server port
  std::string load_balancer_addr =
      "localhost";               // Default Load Balancer hostname
  int load_balancer_port = 4242; // Default Load Balancer port

  config() {
    opt_group{custom_options_, "global"}
        .add(port, "port", "Server port")
        .add(load_balancer_addr, "lb-addr", "Load Balancer address")
        .add(load_balancer_port, "lb-port", "Load Balancer port");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  std::cout << "Starting server on port " << cfg.port << std::endl;

  // Connect to Load Balancer
  std::cout << "Connecting to Load Balancer on " << cfg.load_balancer_addr
            << ":" << cfg.load_balancer_port << std::endl;
  auto load_balancer = sys.middleman().remote_actor(cfg.load_balancer_addr,
                                                    cfg.load_balancer_port);
  if (!load_balancer) {
    std::cerr << "Failed to connect to Load Balancer: "
              << to_string(load_balancer.error()) << std::endl;
    return;
  }

  // Spawn the server and register with the load balancer
  auto server = sys.spawn(server_actor, *load_balancer);
  auto res = sys.middleman().publish(server, cfg.port);
  if (!res) {
    std::cerr << "Failed to publish server: " << to_string(res.error())
              << std::endl;
  } else {
    std::cout << "Server is running on port " << *res
              << " and connected to the Load Balancer" << std::endl;
  }
}

CAF_MAIN(io::middleman)
