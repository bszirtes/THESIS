%% deploy_ping_pong_lb_prime_prime.erl
%%
%% This module implements a distributed message-passing system using the Actor model in Erlang.
%% It simulates clients sending messages to servers through a load balancer.
%% Servers calculate the number of primes in the received range and respond with the result.
%% The main process ensures orderly termination.
%%
%% Parameters:
%% - Nodes: The nodes to deploy the server and clients on.
%% - NumServers: Number of server processes.
%% - NumClients: Number of client processes.
%% - NumMessages: Number of messages each client sends.
%% - PrimeRange: Range for prime calculation.
%% - no-load-balancer: Deploying with predeployed load balancer and servers.
%% - verbose: Verbose mode for debugging.
%%
%% The system terminates when the clients, the servers and the load balancer finish processing.

-module(deploy_ping_pong_lb_prime).
-export([main/1]).

% Entry point of the program
main(Args) ->
    case parse_args(Args) of
        {ok, Nodes, NumServers, NumClients, NumMessages, PrimeRange, NoLoadBalancer, Verbose} ->
            launch_deployment(Nodes, NumServers, NumClients, NumMessages, PrimeRange, NoLoadBalancer, Verbose); % Start the system with parsed parameters
        {error, Msg} ->
            io:format("Error: ~s~nUsage: deploy \"['node1@host','node2@host',...]\" <num_servers> <num_clients> <num_messages> <prime_range> [no-load-balancer] [verbose]~n", [Msg])
    end.

% Parses command-line arguments and converts them
parse_args([NodesStr, NumServersStr, NumClientsStr, NumMessagesStr, PrimeRangeStr | Rest]) ->
    Nodes = parse_nodes(NodesStr),
    case {string:to_integer(NumServersStr), string:to_integer(NumClientsStr), string:to_integer(NumMessagesStr), string:to_integer(PrimeRangeStr)} of
        {{IntServers, _}, {IntClients, _}, {IntMessages, _}, {IntPrimeRange, _}}
          when IntServers /= error andalso IntClients /= error andalso IntMessages /= error andalso IntPrimeRange /= error ->
            NoLoadBalancer = lists:member("no-load-balancer", Rest), % Check for no-load-balancer flag
            Verbose = lists:member("verbose", Rest), % Check for verbosity flag
            {ok, Nodes, IntServers, IntClients, IntMessages, IntPrimeRange, NoLoadBalancer, Verbose};
        _ ->
            {error, "Invalid arguments."}
    end;
parse_args(_) ->
    {error, "Invalid number of arguments."}.

% Parses the node array
parse_nodes(NodesStr) ->
    Stripped = string:trim(NodesStr, both, "[]"),
    Parts = string:split(Stripped, ",", all),
    [list_to_atom(string:trim(P)) || P <- Parts].

launch_deployment(Nodes, NumServers, NumClients, NumMessages, PrimeRange, NoLoadBalancer, Verbose) ->
    Main = self(), % Store reference to the main process
    LoadBalancerNode = hd(Nodes),
    start_load_balancer(LoadBalancerNode, NoLoadBalancer, Main, Verbose),
    distribute_servers(Nodes, NumServers, Main, Verbose),
    distribute_clients(Nodes, NumClients, NumMessages, PrimeRange, Main, Verbose),
    io:format("All clients finished, terminating servers and load balancer.~n"),
    stop_load_balancer(NoLoadBalancer, Verbose),
    io:format("Terminating.~n"),
    halt(0).

start_load_balancer(_, true, _, _) ->
    io:format("Skipping load balancer and server start as \"no-load-balancer\" was provided.~n");
start_load_balancer(LoadBalancerNode, false, Main, Verbose) ->
    io:format("Starting load balancer on node '~p'...~n", [LoadBalancerNode]),
    spawn(LoadBalancerNode, fun() -> ping_pong_lb_prime_load_balancer:start(Main, Verbose) end).

stop_load_balancer(true, _) ->
    io:format("Skipping load balancer and server stop as \"no-load-balancer\" was provided.~n");
stop_load_balancer(false, true) -> % Verbose mode enabled
    timer:sleep(500),
    global:send(ping_pong_load_balancer, {self(), servers}),
    receive
        {load_balancer, Servers} -> stop_servers(Servers, true)
    end,
    global:send(ping_pong_load_balancer, stop),
    receive
        {load_balancer, Pid, done} ->
            io:format("Load balancer ~p finished.~n", [Pid])
    end;
stop_load_balancer(false, false) -> % Silent mode
    timer:sleep(500),
    global:send(ping_pong_load_balancer, {self(), servers}),
    receive
        {load_balancer, Servers} -> stop_servers(Servers, false)
    end,
    global:send(ping_pong_load_balancer, stop),
    receive
        {load_balancer, _Pid, done} -> done
    end.

stop_servers(Servers, true) -> % Verbose mode enabled
    lists:foreach(fun(Server) -> Server ! stop end, Servers),
    lists:foreach(fun(_) ->
        receive
            {server, Pid, done} -> io:format("Server ~p finished.~n", [Pid])
        end
    end, lists:seq(1, length(Servers)));
stop_servers(Servers, false) -> % Silent mode
    lists:foreach(fun(Server) -> Server ! stop end, Servers),
    lists:foreach(fun(_) ->
        receive
            {server, _Pid, done} -> done
        end
    end, lists:seq(1, length(Servers))).

distribute_servers(Nodes, NumServers, Main, true) -> % Verbose mode enabled
    % Get the list of running servers from the load balancer
    timer:sleep(500),
    spawn(hd(Nodes), fun() ->
        global:send(ping_pong_load_balancer, {self(), servers}),
        receive
            {load_balancer, Servers} -> Main ! {forwarded, Servers}
        end
    end),
    receive
        {forwarded, Servers} -> Servers
    end,
    NumDeployedServers = length(Servers),
    case NumDeployedServers of
        0 -> io:format("Starting ~p servers...~n", [NumServers]);
        _ -> io:format("Scaling servers to ~p...~n", [NumServers])
    end,
    case NumServers > NumDeployedServers of % if the desired server count is bigger than the number of already running instances
        true ->
            NumNodes = length(Nodes),
            lists:foreach(fun(Index) ->
                Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
                io:format("Starting server ~p on node '~p'...~n", [Index, Node]),
                spawn(Node, fun() -> ping_pong_lb_prime_server:start(Main, true) end)
            end, lists:seq(NumDeployedServers + 1, NumServers));
        _ -> ok
    end,
    timer:sleep(500); % Wait for every server to register with the load balancer
distribute_servers(Nodes, NumServers, Main, false) -> % Silent mode
    % Get the list of running servers from the load balancer
    timer:sleep(500),
    spawn(hd(Nodes), fun() ->
        global:send(ping_pong_load_balancer, {self(), servers}),
        receive
            {load_balancer, Servers} -> Main ! {forwarded, Servers}
        end
    end),
    receive
        {forwarded, Servers} -> Servers
    end,
    NumDeployedServers = length(Servers),
    case NumDeployedServers of
        0 -> io:format("Starting ~p servers...~n", [NumServers]);
        _ -> io:format("Scaling servers to ~p...~n", [NumServers])
    end,
    case NumServers > NumDeployedServers of % if the desired server count is bigger than the number of already running instances
        true ->
            NumNodes = length(Nodes),
            lists:foreach(fun(Index) ->
                Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
                spawn(Node, fun() -> ping_pong_lb_prime_server:start(Main, false) end)
            end, lists:seq(NumDeployedServers + 1, NumServers));
        _ -> ok
    end,
    timer:sleep(500). % Wait for every server to register with the load balancer

distribute_clients(Nodes, NumClients, NumMessages, PrimeRange, Main, true) -> % Verbose mode enabled
    NumNodes = length(Nodes),
    io:format("Distributing ~p clients across ~p nodes to send ~p messages each...~n", [NumClients, NumNodes, NumMessages]),

    lists:foreach(fun(Index) ->
        Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
        io:format("Starting client ~p on node '~p'...~n", [Index, Node]),
        spawn(Node, fun() -> ping_pong_lb_prime_client:start(NumMessages, PrimeRange, Main, true) end)
    end, lists:seq(1, NumClients)),

    lists:foreach(fun(_) ->
        receive
            {client, Pid, done} -> io:format("Client ~p finished.~n", [Pid])
        end
    end, lists:seq(1, NumClients));
distribute_clients(Nodes, NumClients, NumMessages, PrimeRange, Main, false) -> % Silent mode
    NumNodes = length(Nodes),
    io:format("Distributing ~p clients across ~p nodes to send ~p messages each...~n", [NumClients, NumNodes, NumMessages]),

    lists:foreach(fun(Index) ->
        Node = lists:nth((Index rem NumNodes) + 1, Nodes),  % Round-robin distribution
        spawn(Node, fun() -> ping_pong_lb_prime_client:start(NumMessages, PrimeRange, Main, false) end)
    end, lists:seq(1, NumClients)),

    lists:foreach(fun(_) ->
        receive
            {client, _Pid, done} -> done
        end
    end, lists:seq(1, NumClients)).

