cmake_minimum_required(VERSION 3.10)
project(prime_calculation)

# Add CAF include directory
include_directories(/usr/include/caf)

# Add CAF libraries
link_directories(/usr/lib/x86_64-linux-gnu)

# Add executable target
add_executable(prime_calculation.out prime_calculation.cpp)

# Link CAF libraries
target_link_libraries(prime_calculation.out
    caf_core
    caf_io
    caf_openssl
)
