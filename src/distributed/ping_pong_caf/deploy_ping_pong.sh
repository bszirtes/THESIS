#!/bin/bash

# Function to display usage
usage() {
    echo "Usage: $0 <nodes> <num_clients> <num_messages> <delay_ms> [--no-server] [--verbose] [--kubeconfig /path/to/kubeconfig]"
    echo "           [--server-image SERVER_IMAGE] [--client-image CLIENT_IMAGE]"
    echo "  --no-server: Skip deploying the server (use if server is already running)"
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
  replicas: 1
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
          image: $SERVER_IMAGE
          ports:
            - containerPort: 4242
          args:
            - "--port=4242"
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
          image: $CLIENT_IMAGE
          args:
            - "--server-address=caf-server"
            - "--messages=$NUM_MESSAGES"
            - "--delay=$DELAY_MS"
EOF
    if [[ "$1" == "true" ]]; then
        echo "            - \"--verbose\"" >>"$SCRIPT_DIR"/temp-client-job.yaml
    fi
}

# Check if the correct number of arguments is provided
if [ "$#" -lt 4 ]; then
    usage
fi

# Get the directory of the current script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Get script arguments
NUM_CLIENTS=$2
NUM_MESSAGES=$3
DELAY_MS=$4
shift 4

# Default values for optional flags
NO_SERVER=false
VERBOSE=false
KUBECONFIG="/home/eszibot/.kube/config"
SERVER_IMAGE="localhost:5001/ping-pong-server:v4"
CLIENT_IMAGE="localhost:5001/ping-pong-client:v4"

# Parse optional parameters
while [[ $# -gt 0 ]]; do
    case "$1" in
    --no-server)
        NO_SERVER=true
        shift
        ;;
    --verbose)
        VERBOSE=true
        shift
        ;;
    --kubeconfig)
        if [[ -n "$2" && "$2" != --* ]]; then
            KUBECONFIG="$2"
            shift 2
        else
            echo "Error: --kubeconfig requires a path argument."
            exit 1
        fi
        ;;
    --server-image)
        if [[ -n "$2" && "$2" != --* ]]; then
            SERVER_IMAGE="$2"
            shift 2
        else
            echo "Error: --server-image requires an image argument."
            exit 1
        fi
        ;;
    --client-image)
        if [[ -n "$2" && "$2" != --* ]]; then
            CLIENT_IMAGE="$2"
            shift 2
        else
            echo "Error: --client-image requires an image argument."
            exit 1
        fi
        ;;

    *)
        echo "Unknown option: $1"
        exit 1
        ;;
    esac
done

# Check if required parameters are provided
if [ -z "$KUBECONFIG" ] || [ -z "$NUM_CLIENTS" ] || [ -z "$NUM_MESSAGES" ] || [ -z "$DELAY_MS" ]; then
    usage
fi

export KUBECONFIG

# Create YAML files dynamically with parameters
create_server_yaml $VERBOSE
create_client_yaml $VERBOSE

# Deploy the server and its service if not skipping
if [ "$NO_SERVER" = false ]; then
    # Create the namespace for the deployment
    kubectl create namespace ping-pong >>kubectl.log 2>&1
    kubectl apply -f "$SCRIPT_DIR"/server-service.yaml >>kubectl.log 2>&1

    echo "Starting server..."
    kubectl apply -f "$SCRIPT_DIR"/temp-server-deployment.yaml >>kubectl.log 2>&1

    # Wait for the server to be up and running
    kubectl rollout status --namespace ping-pong deployment/caf-server >>kubectl.log 2>&1
else
    echo "Skipping server start as \"--no-server\" was provided."
fi

echo "Distributing $NUM_CLIENTS clients across nodes to send $NUM_MESSAGES messages each..."
kubectl apply -f "$SCRIPT_DIR"/temp-client-job.yaml >>kubectl.log 2>&1

# Wait for the job to finish
kubectl wait --namespace ping-pong --for=condition=complete job/caf-client >>kubectl.log 2>&1

# Check if the job completed successfully or failed
job_status=""
while [[ "$job_status" != *"Complete"* && "$job_status" != *"Failed"* ]]; do
    job_status=$(kubectl get job caf-client --namespace ping-pong -o jsonpath='{.status.conditions[*].type}')
    sleep 1
done
if [[ "$job_status" != *"Complete"* ]]; then
    echo "Error: Client job failed with job status: $job_status."
    # Delete client job and exit with failure
    kubectl delete job --namespace ping-pong caf-client >>kubectl.log 2>&1
    exit 1
fi

# Cleanup - Delete the client job and server if needed
echo "All clients finished, terminating server."

# Delete the client job
kubectl delete job --namespace ping-pong caf-client >>kubectl.log 2>&1

# If server was deployed, delete it
if [ "$NO_SERVER" = false ]; then
    kubectl delete deployment --namespace ping-pong caf-server --grace-period=0 --force >>kubectl.log 2>&1
    # Wait for server deletion
    kubectl wait --namespace ping-pong --for=delete deployment/caf-server >>kubectl.log 2>&1

    kubectl delete namespace ping-pong >>kubectl.log 2>&1
    # Wait for namespace deletion
    kubectl wait --for=delete namespace ping-pong >>kubectl.log 2>&1
else
    echo "Skipping server stop as \"--no-server\" was provided."
fi

echo "------------------------------" >>kubectl.log

echo "Terminating."
exit 0
