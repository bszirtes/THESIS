% ping_pong_load_balancer.erl
-module(ping_pong_lb_load_balancer).
-export([start/0, start/1, start/2]).

start() ->
    % Register the load balancer with a global name
    global:register_name(ping_pong_load_balancer, spawn(fun() -> load_balancer([], 0, true) end)).

start(Verbose) ->
    % Register the load balancer with a global name
    global:register_name(ping_pong_load_balancer, spawn(fun() -> load_balancer([], 0, Verbose) end)).

start(Main, Verbose) ->
    % Register the load balancer with a global name
    global:register_name(ping_pong_load_balancer, spawn(fun() -> load_balancer([], 0, Main, Verbose) end)).

% Load balancer process: Keeps track of available servers and distributes messages among them in a round-robin manner
load_balancer(Servers, Index, true) -> % Verbose mode enabled
    receive
        {From, register} ->
            io:format("Registered server ~p, server count: ~p~n", [From, length(Servers) + 1]),
            load_balancer([From | Servers], Index, true); % Add new server to the servers
        {From, servers} ->
            From ! {load_balancer, Servers},
            load_balancer(Servers, Index, true);
        {From, Msg} ->
            % Select a worker using round-robin scheduling
            Server = lists:nth((Index rem length(Servers)) + 1, Servers),
            Server ! {From, Msg}, % Forward message to worker
            load_balancer(Servers, (Index + 1), true); % Continue loop
        stop ->
            io:format("Load balancer ~p terminating.~n", [self()])
    end;
load_balancer(Servers, Index, false) -> % Silent mode
    receive
        {From, register} ->
            load_balancer([From | Servers], Index, false); % Add new server to the servers
        {From, servers} ->
            From ! {load_balancer, Servers},
            load_balancer(Servers, Index, false);
        {From, Msg} ->
            % Select a worker using round-robin scheduling
            Server = lists:nth((Index rem length(Servers)) + 1, Servers),
            Server ! {From, Msg}, % Forward message to worker
            load_balancer(Servers, (Index + 1), false); % Continue loop
        stop -> stop
    end.

% Load balancer process: Keeps track of available servers and distributes messages among them in a round-robin manner
load_balancer(Servers, Index, Main, true) -> % Verbose mode enabled
    receive
        {From, register} ->
            io:format("Registered server ~p, server count: ~p~n", [From, length(Servers) + 1]),
            load_balancer([From | Servers], Index, Main, true); % Add new server to the servers
        {From, servers} ->
            From ! {load_balancer, Servers},
            load_balancer(Servers, Index, Main, true);
        {From, Msg} ->
            % Select a worker using round-robin scheduling
            Server = lists:nth((Index rem length(Servers)) + 1, Servers),
            Server ! {From, Msg}, % Forward message to worker
            load_balancer(Servers, (Index + 1), Main, true); % Continue loop
        stop ->
            io:format("Load balancer ~p terminating.~n", [self()]),
            Main ! {load_balancer, self(), done} % Notify the main process
    end;
load_balancer(Servers, Index, Main, false) -> % Silent mode
    receive
        {From, register} ->
            load_balancer([From | Servers], Index, Main, false); % Add new server to the servers
        {From, servers} ->
            From ! {load_balancer, Servers},
            load_balancer(Servers, Index, Main, false);
        {From, Msg} ->
            % Select a worker using round-robin scheduling
            Server = lists:nth((Index rem length(Servers)) + 1, Servers),
            Server ! {From, Msg}, % Forward message to worker
            load_balancer(Servers, (Index + 1), Main, false); % Continue loop
        stop ->
            Main ! {load_balancer, self(), done} % Notify the main process
    end.

