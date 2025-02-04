#include <caf/all.hpp>
#include <iostream>
#include <mutex>
#include <string>
#include <vector>

using namespace caf;

// Global mutex for synchronized output
std::mutex output_mutex;

// Thread-safe print function
template <typename... Args> void safe_print(Args... args) {
  std::lock_guard<std::mutex> guard(output_mutex);
  (std::cout << ... << args) << std::endl;
}

// Worker behavior: Compute the length of a string and send it back to the
// client
behavior worker_fun(event_based_actor *self) {
  return {[=](const actor &client, const std::string &msg) {
            size_t length = msg.size();
            self->send(client, length);
          },
          [=](atom_value stop) { self->quit(); }};
}

// Load balancer behavior: Routes messages to workers in round-robin fashion
behavior load_balancer_fun(event_based_actor *self, std::vector<actor> workers,
                           actor main, size_t num_clients,
                           size_t num_messages) {
  size_t total_messages = num_clients * num_messages;
  size_t worker_index = 0;

  return {[=](const actor &client, const std::string &msg) mutable {
            self->send(workers[worker_index], client, msg);
            worker_index = (worker_index + 1) % workers.size();
          },
          [=](atom_value stop) {
            for (auto &worker : workers) {
              self->send(worker, atom("stop"));
            }
            self->send(main, atom("stop"));
            self->quit();
          }};
}

// Client behavior: Sends messages to the load balancer and forwards responses
// to the main actor
behavior client_fun(event_based_actor *self, const actor &balancer,
                    size_t num_messages, const std::string &message,
                    const actor &main) {
  size_t responses_received = 0;

  return {[=](atom_value start) mutable {
            for (size_t i = 0; i < num_messages; ++i) {
              self->send(balancer, self, message);
            }
          },
          [=](size_t length) mutable {
            self->send(main, length);
            ++responses_received;
            if (responses_received == num_messages) {
              self->quit();
            }
          }};
}

// Main actor that waits for all responses before terminating
behavior main_fun(event_based_actor *self, size_t total_messages) {
  size_t received_messages = 0;
  return {[=](size_t length) mutable {
    ++received_messages;
    // safe_print("Received ", length);
    if (received_messages == total_messages) {
      safe_print("Stopped");
      self->quit();
    }
  }};
}

void caf_main(actor_system &system) {
  constexpr size_t num_workers = 100;
  constexpr size_t num_clients = 30000;
  constexpr size_t num_messages = 100;
  const std::string binary_message = "Hello World";

  safe_print("Starting...");

  // Spawn main actor
  auto main_actor = system.spawn(main_fun, num_clients * num_messages);

  // Spawn workers
  std::vector<actor> workers;
  for (size_t i = 0; i < num_workers; ++i) {
    workers.push_back(system.spawn(worker_fun));
  }

  // Spawn load balancer
  auto balancer = system.spawn(load_balancer_fun, workers, main_actor,
                               num_clients, num_messages);

  // Spawn clients
  for (size_t i = 0; i < num_clients; ++i) {
    auto client = system.spawn(client_fun, balancer, num_messages,
                               binary_message, main_actor);
    anon_send(client, atom("start"));
  }
}

CAF_MAIN()
