FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    g++ \
    cmake \
    libcaf-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the server binary
COPY ping_pong_server.out /app/ping_pong_server

# Expose the default server port
EXPOSE 4242

# Define the default command
CMD ["/app/ping_pong_server", "--port=4242"]
