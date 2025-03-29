/*
 * ping_pong.cpp
 *
 * This source file implements a distributed message-passing system using the
 * Actor model in the CAF framework. It simulates clients sending messages
 * (pings) to the server, which processes messages and
 * responds with pongs. The main process ensures orderly termination.
 *
 * Parameters:
 * - clients: Number of client processes.
 * - messages: Messages each client sends.
 * - verbose: Verbose mode for debugging.
 *
 * The system terminates when all clients and the server finish processing.
 */

#include <caf/actor_system.hpp>
#include <caf/all.hpp>
#include <caf/atom.hpp>
#include <caf/stateful_actor.hpp>
#include <cstddef>
#include <iostream>

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

// Client behavior: Sends a predefined number of "ping" messages to the
// server and waits for "pong" responses. It notifies the main actor upon
// completion.
behavior client_fun(event_based_actor *self, const actor &server,
                    const size_t num_messages, const actor &main,
                    const bool verbose) {
  size_t responses_received = 0;

  return {[=](atom_value msg) mutable {
    if (msg == atom("start")) {
      self->send(server, self, atom("ping"));
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
        self->send(server, self, atom("ping"));
      }
    }
  }};
}

// Main actor state to store the server actor and the number of finished clients
struct main_state {
  actor server;
  size_t clients_finished = 0;
};

// Main actor: Waits for all clients and the server to finish before terminating
// the system.
behavior main_fun(stateful_actor<main_state> *self, const size_t num_clients,
                  const bool verbose) {

  return {[=](const actor &server) mutable { self->state.server = server; },

          [=](atom_value type, actor_id id, atom_value status) mutable {
            if (status == atom("done")) {
              if (type == atom("client")) {
                ++self->state.clients_finished;
                if (verbose) {
                  aout(self) << "Client <" << id << "> finished." << std::endl;
                }
                if (self->state.clients_finished == num_clients) {
                  aout(self) << "All clients finished, terminating server."
                             << std::endl;
                  self->send(self->state.server, atom("stop"));
                }
              } else if (type == atom("server")) {
                if (verbose) {
                  aout(self) << "Server <" << id << "> finished." << std::endl;
                }
                aout(self) << "Terminating." << std::endl;
                self->quit();
              }
            }
          }};
}

// Configuration structure for setting up the actor system parameters
struct config : public actor_system_config {
  size_t num_clients = 4;
  size_t num_messages = 10;
  bool verbose = false;

  config() {
    opt_group{custom_options_, "global"}
        .add(num_clients, "clients", "Number of Clients")
        .add(num_messages, "messages", "Number of Messages")
        .add(verbose, "verbose", "Verbose output");
  }
};

// Main function that initializes the actor system and spawns actors
void caf_main(actor_system &sys, const config &cfg) {
  // Spawn main actor to oversee termination
  auto main_actor = sys.spawn(main_fun, cfg.num_clients, cfg.verbose);

  // Spawn server actor
  std::cout << "Starting server..." << std::endl;
  auto server_actor = sys.spawn(server_fun, main_actor, cfg.verbose);
  anon_send(main_actor, server_actor);

  // Spawn client actors
  std::cout << "Starting " << cfg.num_clients << " clients to send "
            << cfg.num_messages << " messages each..." << std::endl;
  for (size_t i = 0; i < cfg.num_clients; ++i) {
    auto client_actor = sys.spawn(client_fun, server_actor, cfg.num_messages,
                                  main_actor, cfg.verbose);
    anon_send(client_actor, atom("start"));
  }
}

// Entry point for the CAF-based actor system
CAF_MAIN()
