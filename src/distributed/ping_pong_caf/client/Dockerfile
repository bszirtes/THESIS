# Dockerfile.client
FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    g++ \
    cmake \
    libcaf-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the client binary (assuming it's already compiled)
COPY ping_pong_client.out /app/ping_pong_client

# Define the default command
CMD ["/app/ping_pong_client", "--server=caf-server", "--pings=10", "--delay=1000"]
