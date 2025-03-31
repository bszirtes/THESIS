FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    g++ \
    cmake \
    libcaf-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the lb binary
COPY ping_pong_lb_load_balancer.out /app/ping_pong_lb_load_balancer

# Expose the default lb port
EXPOSE 4242

# Define the default entrypoint
ENTRYPOINT ["/app/ping_pong_lb_load_balancer"]
