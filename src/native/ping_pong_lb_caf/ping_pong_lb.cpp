#include <caf/actor_system.hpp>
#include <caf/all.hpp>
#include <caf/atom.hpp>
#include <cstddef>
#include <iostream>
#include <mutex>
#include <vector>

using namespace caf;

// Global mutex for synchronized output
std::mutex output_mutex;

// Thread-safe print function
template <typename... Args> void safe_print(Args... args) {
  std::lock_guard<std::mutex> guard(output_mutex);
  (std::cout << ... << args) << std::endl;
}

// Worker behavior: Processes messages and notifies main when done
behavior worker_fun(event_based_actor *self, const actor &main, bool verbose) {
  return {[=](const actor &client, atom_value ping) {
            if (ping == atom("ping")) {
              if (verbose)
                safe_print("Worker ", self->id(),
                           " received: ", to_string(ping));
              self->send(client, atom("pong"));
            }
          },
          [=](atom_value stop) {
            if (stop == atom("stop")) {
              if (verbose)
                safe_print("Worker ", self->id(), " terminated.");
              self->send(main, atom("worker"), atom("done"));
              self->quit();
            }
          }};
}

// Load balancer behavior: Routes messages to workers in round-robin fashion
behavior load_balancer_fun(event_based_actor *self, std::vector<actor> workers,
                           actor main, size_t num_clients,
                           size_t num_messages) {
  size_t total_messages = num_clients * num_messages;
  size_t forwarded_messages = 0;
  size_t worker_index = 0;

  return {[=](const actor &client, atom_value ping) mutable {
    if (ping == atom("ping")) {
      self->send(workers[worker_index], client, ping);
      worker_index = (worker_index + 1) % workers.size();
      forwarded_messages++;
    }

    if (forwarded_messages == total_messages) {
      safe_print("Load Balancer ", self->id(),
                 " finished, stopping all workers.");
      for (auto &worker : workers) {
        self->send(worker, atom("stop"));
      }
      self->quit();
    }
  }};
}

// Client behavior: Sends messages and notifies main when done
behavior client_fun(event_based_actor *self, const actor &balancer,
                    size_t num_messages, const actor &main, bool verbose) {
  size_t responses_received = 0;

  return {[=](atom_value msg) mutable {
    if (msg == atom("start")) {
      for (size_t i = 0; i < num_messages; ++i) {
        self->send(balancer, self, atom("ping"));
      }
    }
    if (msg == atom("pong")) {
      ++responses_received;
      if (verbose)
        safe_print("Client ", self->id(), " received: ", to_string(msg));
      if (responses_received == num_messages) {
        if (verbose)
          safe_print("Client ", self->id(), " reached max (", num_messages,
                     ") pings, terminating.");
        self->send(main, atom("client"), atom("done"));
        self->quit();
      }
    }
  }};
}

// Main actor: Waits for all workers and clients to finish before terminating
behavior main_fun(event_based_actor *self, size_t num_workers,
                  size_t num_clients) {
  size_t expected_workers = num_workers;
  size_t expected_clients = num_clients;
  size_t received_workers = 0;
  size_t received_clients = 0;

  return {[=](atom_value type, atom_value status) mutable {
    if (status == atom("done")) {
      if (type == atom("worker")) {
        ++received_workers;
      } else if (type == atom("client")) {
        ++received_clients;
      }
      if (received_workers == expected_workers &&
          received_clients == expected_clients) {
        safe_print("All clients and workers finished.");
        safe_print("Terminating.");
        self->quit();
      }
    }
  }};
}

// Define config structure
struct config : public actor_system_config {
  size_t num_workers = 100;
  size_t num_clients = 100;
  size_t num_messages = 30000;
  bool verbose = false;

  config() {
    opt_group{custom_options_, "global"}
        .add(num_workers, "workers", "Number of Workers")
        .add(num_clients, "clients", "Number of Clients")
        .add(num_messages, "messages", "Number of Messages")
        .add(verbose, "verbose", "Verbose output");
  }
};

void caf_main(actor_system &sys, const config &cfg) {
  size_t num_workers = cfg.num_workers;
  size_t num_clients = cfg.num_clients;
  size_t num_messages = cfg.num_messages;
  bool verbose = cfg.verbose;

  // Spawn main actor
  safe_print("Starting watcher...");
  auto main_actor = sys.spawn(main_fun, num_workers, num_clients);

  // Spawn workers
  safe_print("Starting ", num_workers, " workers...");
  std::vector<actor> workers;
  for (size_t i = 0; i < num_workers; ++i) {
    workers.push_back(sys.spawn(worker_fun, main_actor, verbose));
  }

  // Spawn load balancer
  safe_print("Starting load balancer...");
  auto balancer = sys.spawn(load_balancer_fun, workers, main_actor, num_clients,
                            num_messages);

  // Spawn clients
  safe_print("Starting ", num_clients, " clients to send ", num_messages,
             " messages each...");
  for (size_t i = 0; i < num_clients; ++i) {
    auto client =
        sys.spawn(client_fun, balancer, num_messages, main_actor, verbose);
    anon_send(client, atom("start"));
  }
}

CAF_MAIN()
