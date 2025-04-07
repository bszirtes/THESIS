#include <caf/all.hpp>
#include <caf/io/all.hpp>
#include <caf/stateful_actor.hpp>
#include <cstddef>
#include <iostream>
#include <ostream>
#include <vector>

using namespace caf;

struct lb_state {
  std::size_t server_index;
  std::vector<actor> servers;
};

// Load balancer behavior: Distributes the messages to servers in a round-robin
// manner.
behavior load_balancer_fun(stateful_actor<lb_state> *self) {
  return {
      [=](const actor &actor, atom_value msg) mutable {
        if (msg == atom("register")) { // Register a new server
          self->state.servers.push_back(actor);
          aout(self) << "Registered server <" << actor->address()
                     << ">, server count: " << self->state.servers.size()
                     << std::endl;
        }
      },
      [=](const actor &actor, int msg) {
        if (!self->state.servers.empty()) { // Forward message to a server
          self->send(self->state.servers[self->state.server_index], actor, msg);
          self->state.server_index = (self->state.server_index + 1) %
                                     self->state.servers.size(); // Round-robin
        }
      }};
}

struct config : public actor_system_config {
  int port = 4242;

  config() {
    opt_group{custom_options_, "global"}.add(port, "port",
                                             "Load balancer port");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  // Spawn load balancer actor
  auto balancer = sys.spawn(load_balancer_fun);

  // Publish the load balancer actor on the given port
  auto res = sys.middleman().publish(balancer, cfg.port);
  if (!res) {
    std::cerr << "Failed to publish load balancer: " << to_string(res.error())
              << std::endl;
  } else {
    std::cout << "Load Balancer is running on port " << *res << std::endl;
  }
}

// Enable CAF's network capabilities
CAF_MAIN(io::middleman)
