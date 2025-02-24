#include <caf/all.hpp>
#include <caf/io/all.hpp>

using namespace caf;

behavior ping_pong_server(event_based_actor *self) {
  return {[=](const std::string &msg, actor client) {
    if (msg == "ping") {
      aout(self) << "Server received: " << msg << "\n";
      self->send(client, "pong", self);
    }
  }};
}

void caf_main(actor_system &sys) {
  auto server = sys.spawn(ping_pong_server);
  auto port = 4242;
  auto max_conns = 10;
  auto res = sys.middleman().publish(server, port, nullptr, max_conns);

  if (!res) {
    std::cerr << "Failed to publish server: " << to_string(res.error())
              << std::endl;
  } else {
    std::cout << "Server running on port " << *res << std::endl;
  }
}

CAF_MAIN(io::middleman)
