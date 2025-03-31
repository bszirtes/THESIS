cmake_minimum_required(VERSION 3.10)
project(ping_pong_lb_server)

# Add CAF include directory
include_directories(/usr/include/caf)

# Add CAF libraries
link_directories(/usr/lib/x86_64-linux-gnu)

# Add executable target
add_executable(ping_pong_lb_server.out ping_pong_lb_server.cpp)

# Link CAF libraries
target_link_libraries(ping_pong_lb_server.out
    caf_core
    caf_io
    caf_openssl
)
