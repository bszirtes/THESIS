#!/bin/bash

# Function to display usage
usage() {
    echo "Usage: $0 <nodes> <num_servers> <num_clients> <num_messages> <delay_ms> <path-to-kubeconfig> [--no-load-balancer] [--verbose]"
    echo "  --no-load-balancer: Skip deploying the load balancer (use if load balancer is already running)"
    echo "  --verbose:   Verbose output for debugging"
    echo "  The node list is not used in the script, it is just a note."
    exit 1
}

create_server_yaml() {
    cat <<EOF >"$SCRIPT_DIR"/temp-server-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: caf-server
  namespace: ping-pong
spec:
  replicas: $NUM_SERVERS
  selector:
    matchLabels:
      app: caf-server
  template:
    metadata:
      labels:
        app: caf-server
    spec:
      containers:
        - name: caf-server
          image: localhost:5001/ping-pong-lb-server:v2
          command: ["/app/ping_pong_lb_server"]
          ports:
            - containerPort: 4243
          args:
            - "--port=4243"
            - "--lb-address=caf-load-balancer"
            - "--lb-port=4242"
EOF

    if [[ "$1" == "true" ]]; then
        echo "            - \"--verbose\"" >>"$SCRIPT_DIR"/temp-server-deployment.yaml
    fi
}

create_client_yaml() {
    cat <<EOF >"$SCRIPT_DIR"/temp-client-job.yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: caf-client
  namespace: ping-pong
spec:
  completions: $NUM_CLIENTS  # Set number of clients to run
  parallelism: $NUM_CLIENTS  # Set number of clients to run
  template:
    metadata:
      labels:
        app: caf-client
    spec:
      restartPolicy: Never
      containers:
        - name: caf-client
          image: localhost:5001/ping-pong-client:v4
          command: ["/app/ping_pong_client"]
          args:
            - "--server-address=caf-load-balancer"
            - "--server-port=4242"
            - "--messages=$NUM_MESSAGES"
            - "--delay=$DELAY_MS"
EOF
    if [[ "$1" == "true" ]]; then
        echo "            - \"--verbose\"" >>"$SCRIPT_DIR"/temp-client-job.yaml
    fi
}

# Check if the correct number of arguments is provided
if [ "$#" -lt 6 ]; then
    usage
fi

# Export KUBECONFIG
export KUBECONFIG=$6

# Get the directory of the current script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Get script arguments
NUM_SERVERS=$2
NUM_CLIENTS=$3
NUM_MESSAGES=$4
DELAY_MS=$5
NO_LOAD_BALANCER=false
VERBOSE=false

# Parse optional parameters
for arg in "$@"; do
    case "$arg" in
    "--no-load-balancer")
        NO_LOAD_BALANCER=true
        shift
        ;;
    "--verbose")
        VERBOSE=true
        shift
        ;;
    esac
done

shift $((OPTIND - 1)) # Remove parsed options from positional parameters

# Check if required parameters are provided
if [ -z "$NUM_SERVERS" ] || [ -z "$NUM_CLIENTS" ] || [ -z "$NUM_MESSAGES" ] || [ -z "$DELAY_MS" ] || [ -z "$KUBECONFIG" ]; then
    usage
fi

# Create YAML files dynamically with parameters
create_server_yaml $VERBOSE
create_client_yaml $VERBOSE

# Deploy the load balancer and its service if not skipping
kubectl apply -f "$SCRIPT_DIR"/server-service.yaml >>kubectl.log 2>&1
if [ "$NO_LOAD_BALANCER" = false ]; then
    echo "Starting load balancer..."
    kubectl apply -f "$SCRIPT_DIR"/load_balancer/load-balancer-deployment.yaml >>kubectl.log 2>&1

    # Wait for the load balancer to be up and running
    kubectl rollout status --namespace ping-pong deployment/caf-load-balancer >>kubectl.log 2>&1

    echo "Starting $NUM_SERVERS servers..."
    kubectl apply -f "$SCRIPT_DIR"/temp-server-deployment.yaml >>kubectl.log 2>&1

    # Wait for the server to be up and running
    kubectl rollout status --namespace ping-pong deployment/caf-server >>kubectl.log 2>&1

else
    echo "Skipping load balancer and server start as \"--no-load-balancer\" was provided."
    echo "Scaling servers to $NUM_SERVERS..."
    kubectl scale deployment --namespace ping-pong caf-server --replicas $NUM_SERVERS >>kubectl.log 2>&1

    # Wait for the server to be up and running
    kubectl rollout status --namespace ping-pong deployment/caf-server >>kubectl.log 2>&1
fi

echo "Distributing $NUM_CLIENTS clients across nodes to send $NUM_MESSAGES messages each..."
kubectl apply -f "$SCRIPT_DIR"/temp-client-job.yaml >>kubectl.log 2>&1

# Wait for the job to finish
kubectl wait --namespace ping-pong --for=condition=complete job/caf-client >>kubectl.log 2>&1

# Check if the job completed successfully or failed
job_status=$(kubectl get jobs --namespace ping-pong caf-client -o jsonpath='{.status.succeeded}')
if [ "$job_status" -eq 0 ]; then
    echo "Error: Client job failed."
    # Delete client job and exit with failure
    kubectl delete job --namespace ping-pong caf-client >>kubectl.log 2>&1
    exit 1
fi

# Cleanup - Delete the client job and server if needed
echo "All clients finished, terminating servers and load balancer."

# Delete the client job
kubectl delete job --namespace ping-pong caf-client >>kubectl.log 2>&1

# If server was deployed, delete it
if [ "$NO_LOAD_BALANCER" = false ]; then
    kubectl delete deployment --namespace ping-pong caf-server --grace-period=0 --force >>kubectl.log 2>&1
    # Wait for server deletion
    kubectl wait --namespace ping-pong --for=delete deployment/caf-server >>kubectl.log 2>&1

    kubectl delete deployment --namespace ping-pong caf-load-balancer --grace-period=0 --force >>kubectl.log 2>&1
    # Wait for load balancer deletion
    kubectl wait --namespace ping-pong --for=delete deployment/caf-load-balancer >>kubectl.log 2>&1
else
    echo "Skipping load balancer and server stop as \"--no-load-balancer\" was provided."
fi

echo "------------------------------" >>kubectl.log

echo "Terminating."
exit 0
