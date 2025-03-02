#!/bin/bash

# Function to display usage
usage() {
    echo "Usage: $0 <number-of-clients> <ping-count> <delay-ms> [--no-server]"
    echo "  --no-server: Skip deploying the server (use if server is already running)"
    exit 1
}

# Check if the correct number of arguments is provided
if [ "$#" -lt 3 ]; then
    usage
fi

# Get script arguments
CLIENT_COUNT=$1
PING_COUNT=$2
DELAY_MS=$3
NO_SERVER=false

# Check if the optional '--no-server' argument is provided
if [ "$4" == "--no-server" ]; then
    NO_SERVER=true
fi

# Check if the provided arguments are integers
if ! [[ "$CLIENT_COUNT" =~ ^[0-9]+$ ]] || ! [[ "$PING_COUNT" =~ ^[0-9]+$ ]] || ! [[ "$DELAY_MS" =~ ^[0-9]+$ ]]; then
    echo "Error: All arguments must be integers."
    usage
fi

# Step 1: Deploy the server if not skipping
if [ "$NO_SERVER" = false ]; then
    kubectl create namespace ping-pong
    echo "Deploying the CAF server..."
    kubectl apply -f server/server-deployment.yaml
    echo "CAF server deployed."

    # Step 2: Wait for the server to be up and running (optional but recommended)
    echo "Waiting for the server to be ready..."
    kubectl rollout status --namespace ping-pong deployment/caf-server
fi

# Step 3: Deploy the clients with the specified arguments
echo "Deploying $CLIENT_COUNT client(s)..."

# Create the client Job YAML dynamically with parameters
cat <<EOF >client/client-job.yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: caf-client
  namespace: ping-pong
spec:
  completions: $CLIENT_COUNT  # Set number of clients to run
  parallelism: $CLIENT_COUNT  # Set number of clients to run
  template:
    metadata:
      labels:
        app: caf-client
    spec:
      restartPolicy: Never
      containers:
        - name: caf-client
          image: localhost:5001/ping-pong-client:v2
          command: ["/app/ping_pong_client"]
          args:
            - "--server=caf-server"
            - "--pings=$PING_COUNT"
            - "--delay=$DELAY_MS"
EOF

# Step 4: Apply the dynamically generated client Job YAML
kubectl apply -f client/client-job.yaml

# Step 5: Wait for the job to finish (optional, but useful for monitoring)
echo "Waiting for client job(s) to complete..."
kubectl wait --namespace ping-pong --for=condition=complete job/caf-client

# Check if the job completed successfully or failed
JOB_STATUS=$(kubectl get jobs --namespace ping-pong caf-client -o jsonpath='{.status.succeeded}')
if [ "$JOB_STATUS" -eq 0 ]; then
    echo "Error: Client job failed to complete successfully."
    # Delete client job and exit with failure
    kubectl delete job --namespace ping-pong caf-client
    exit 1 # Exit the script with a non-zero status
else
    echo "Client job completed successfully."
fi

# Step 6: Cleanup - Delete the client job and server if needed
echo "Cleaning up..."

# Delete the client job
kubectl delete job --namespace ping-pong caf-client

# If server was deployed, delete it
if [ "$NO_SERVER" = false ]; then
    echo "Deleting CAF server..."
    # kubectl delete service --namespace ping-pong caf-server
    # kubectl delete deployment --namespace ping-pong caf-server
    # kubectl wait --namespace ping-pong --for=delete deployment/caf-server
    kubectl delete namespace ping-pong
    echo "Waiting for the 'ping-pong' namespace to be deleted..."
    kubectl wait --for=delete namespace/ping-pong
fi

echo "Cleanup completed. Script finished."
