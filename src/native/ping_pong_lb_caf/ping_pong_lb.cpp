/*
 * ping_pong_lb.cpp
 *
 * This source file implements a distributed message-passing system using the
 * Actor model in the CAF framework. It simulates clients sending messages
 * (pings) to servers through a load balancer. Servers process messages and
 * respond with pongs. The main process ensures orderly termination.
 *
 * Parameters:
 * - servers: Number of server processes.
 * - clients: Number of client processes.
 * - messages: Messages each client sends.
 * - verbose: Verbose mode for debugging.
 *
 * The system terminates when the clients, the servers and the load balancer
 * finish processing.
 */

#include <caf/actor_system.hpp>
#include <caf/all.hpp>
#include <caf/atom.hpp>
#include <caf/send.hpp>
#include <caf/stateful_actor.hpp>
#include <cstddef>
#include <iostream>
#include <vector>

using namespace caf;

// Server behavior: Processes "ping" messages from clients and responds with
// "pong". The server notifies the main actor upon termination.
behavior server_fun(event_based_actor *self, const actor &main,
                    const bool verbose) {
  return {[=](const actor &client, atom_value msg) {
            if (msg == atom("ping")) {
              if (verbose)
                aout(self) << "Server <" << self->id()
                           << "> received: " << to_string(msg) << std::endl;
              self->send(client, atom("pong"));
            }
          },
          [=](atom_value msg) {
            if (msg == atom("stop")) {
              if (verbose)
                aout(self) << "Server <" << self->id() << "> terminating."
                           << std::endl;
              self->send(main, atom("server"), self->id(), atom("done"));
              self->quit();
            }
          }};
}

// Load balancer state to store the index of the current server
struct load_balancer_state {
  size_t server_index = 0;
};

// Load balancer: Distributes the messages to servers in a round-robin manner.
// The load balancer notifies the main actor upon termination.
behavior load_balancer_fun(stateful_actor<load_balancer_state> *self,
                           const std::vector<actor> &servers, const actor &main,
                           const bool verbose) {

  return {[=](const actor &client, atom_value ping) mutable {
            if (ping == atom("ping")) {
              self->send(servers[self->state.server_index], client, ping);
              self->state.server_index =
                  (self->state.server_index + 1) % servers.size();
            }
          },
          [=](atom_value msg) mutable {
            if (msg == atom("stop")) {
              if (verbose)
                aout(self) << "Load balancer <" << self->id()
                           << "> terminating." << std::endl;
              self->send(main, atom("lb"), self->id(), atom("done"));
              self->quit();
            }
          }};
}

// Client behavior: Sends a predefined number of "ping" messages to the
// load balancer and waits for "pong" responses. It notifies the main actor upon
// completion.
behavior client_fun(event_based_actor *self, const actor &load_balancer,
                    const size_t num_messages, const actor &main,
                    const bool verbose) {
  size_t responses_received = 0;

  return {[=](atom_value msg) mutable {
    if (msg == atom("start")) {
      self->send(load_balancer, self, atom("ping"));
    }
    if (msg == atom("pong")) {
      ++responses_received;
      if (verbose)
        aout(self) << "Client <" << self->id()
                   << "> received: " << to_string(msg) << std::endl;
      if (responses_received == num_messages) {
        if (verbose)
          aout(self) << "Client <" << self->id()
                     << "> reached max pings, terminating." << std::endl;
        self->send(main, atom("client"), self->id(), atom("done"));
        self->quit();
      } else {
        self->send(load_balancer, self, atom("ping"));
      }
    }
  }};
}

// Main actor state to store the server actors, the load balancer and the number
// of finished servers and clients
struct main_state {
  actor load_balancer;
  std::vector<actor> servers;
  size_t servers_finished = 0;
  size_t clients_finished = 0;
};

// Main actor: Waits for all clients and the server to finish before terminating
// the system.
behavior main_fun(stateful_actor<main_state> *self, const size_t num_clients,
                  const bool verbose) {

  return {
      [=](const actor &load_balancer) mutable {
        self->state.load_balancer = load_balancer;
      },

      [=](const std::vector<actor> servers) mutable {
        self->state.servers = servers;
      },

      [=](atom_value type, actor_id id, atom_value status) mutable {
        if (status == atom("done")) {
          if (type == atom("client")) {
            ++self->state.clients_finished;
            if (verbose) {
              aout(self) << "Client <" << id << "> finished." << std::endl;
            }
            if (self->state.clients_finished == num_clients) {
              aout(self) << "All clients finished, terminating servers."
                         << std::endl;
              for (auto &server : self->state.servers) {
                self->send(server, atom("stop"));
              }
            }
          } else if (type == atom("server")) {
            ++self->state.servers_finished;
            if (verbose) {
              aout(self) << "Server <" << id << "> finished." << std::endl;
            }
            if (self->state.servers_finished == self->state.servers.size()) {
              aout(self) << "All servers finished, terminating load balancer."
                         << std::endl;
              self->send(self->state.load_balancer, atom("stop"));
            }
          } else if (type == atom("lb")) {
            if (verbose) {
              aout(self) << "Load balancer <" << id << "> finished."
                         << std::endl;
            }
            aout(self) << "Terminating." << std::endl;
            self->quit();
          }
        }
      }};
}

// Configuration structure for setting up the actor system parameters
struct config : public actor_system_config {
  size_t num_servers = 2;
  size_t num_clients = 8;
  size_t num_messages = 10;
  bool verbose = false;

  config() {
    opt_group{custom_options_, "global"}
        .add(num_servers, "servers", "Number of Servers")
        .add(num_clients, "clients", "Number of Clients")
        .add(num_messages, "messages", "Number of Messages")
        .add(verbose, "verbose", "Verbose output");
  }
};

// Main function that initializes the actor system and spawns actors
void caf_main(actor_system &sys, const config &cfg) {
  // Spawn main actor to oversee termination
  auto main_actor = sys.spawn(main_fun, cfg.num_clients, cfg.verbose);

  // Spawn server actors
  std::cout << "Starting " << cfg.num_servers << " servers..." << std::endl;
  std::vector<actor> server_actors;
  for (size_t i = 0; i < cfg.num_servers; ++i) {
    server_actors.push_back(sys.spawn(server_fun, main_actor, cfg.verbose));
  }
  anon_send(main_actor, server_actors);

  // Spawn load balancer actor
  std::cout << "Starting load balancer..." << std::endl;
  auto load_balancer_actor =
      sys.spawn(load_balancer_fun, server_actors, main_actor, cfg.verbose);
  anon_send(main_actor, load_balancer_actor);

  // Spawn client actors
  std::cout << "Starting " << cfg.num_clients << " clients to send "
            << cfg.num_messages << " messages each..." << std::endl;
  for (size_t i = 0; i < cfg.num_clients; ++i) {
    auto client = sys.spawn(client_fun, load_balancer_actor, cfg.num_messages,
                            main_actor, cfg.verbose);
    anon_send(client, atom("start"));
  }
}

// Entry point for the CAF-based actor system
CAF_MAIN()
