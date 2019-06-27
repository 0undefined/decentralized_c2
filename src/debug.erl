-module(debug).

-export([start/0, start/1, send_node/2, dump_nodes/1,
         init_nodes/0, init_nodes/2,
         init_nodes_seperated/0, init_nodes_seperated/3,
         build_adjacency_list/0,
         get_node_snames/0,
         find_diameter/0,
         average_clustering/0,
         min_max_nodes/0,
         kill/1,
         generate_data/1
        ]).

send_node(ToNode, Node) ->
    {name_listener, ToNode} ! Node.

kill(Node) -> {main_proc, Node} ! {new_command, die}.

dump_nodes(Node) ->
    {node_dumper, Node} ! self(),
    lists:filter(fun({F, T}) -> not_debug(F) and not_debug(T) end, receive_nodes(Node, [])).

receive_nodes(FromNode, L) ->
    receive
        finished -> L;
        timeout  -> io:fwrite("Node ~w timed out~n", FromNode), L;
        {Node}   -> receive_nodes(FromNode, [{FromNode, Node}|L])
    after
        1500 -> io:fwrite("Timeout on node: ~w~n", [FromNode]), L
    end.

get_random_nodes(L, Min, Max) ->
    N = length(L),
    GetRandom = fun(_) ->
                        X = rand:uniform(N),
                        lists:nth(X, L)
                end,
    Number = rand:uniform(Max-Min) + Min,
    lists:map(GetRandom, lists:seq(1, Number)).

init_nodes() -> init_nodes(2, 20).
init_nodes(Min, Max) ->
    List = get_node_names(),
    SendNodes = fun (ToNode) ->
                        Ns = get_random_nodes(List, Min, Max),
                        lists:foreach(fun(Node) ->
                                              {name_listener, ToNode} ! Node
                                      end, Ns)
                end,
    lists:foreach(SendNodes, List).

init_nodes_seperated() -> init_nodes_seperated(2, 5, 10).
init_nodes_seperated(Min, Max, Clustersize) ->
    List = lists:sort(get_node_names()),
    SendNodes = fun ({PossibleNodes, ToNode}) ->
                        Ns = get_random_nodes(PossibleNodes, Min, Max),
                        lists:foreach(fun(Node) ->
                                              {name_listener, ToNode} ! Node
                                      end, Ns)
                end,
    Length = length(List),
    if Length >= Clustersize ->
           {Fst, Snd} = lists:split(Clustersize, List),
           A = lists:map(fun(X) -> {Fst, X} end, Fst),
           lists:foreach(SendNodes, A),
           Target = lists:nth(1, Fst),
           init_nodes_seperated_rec(Min, Max, Clustersize, Target, Snd);
       Length > 0 ->
           A = lists:map(fun(X) -> {List, X} end, List),
           lists:foreach(SendNodes, A);
       true -> ok
    end.

init_nodes_seperated_rec(Min, Max, Clustersize, Target, RemainingNodes) ->
    List = lists:sort(RemainingNodes),
    SendNodes = fun ({PossibleNodes, ToNode}) ->
                        Ns = get_random_nodes(PossibleNodes, Min, Max),
                        lists:foreach(fun(Node) ->
                                              {name_listener, ToNode} ! Node
                                      end, Ns)
                end,
    Length = length(List),
    if Length >= Clustersize ->
           {Fst, Snd} = lists:split(Clustersize, List),
           A = lists:map(fun(X) -> {Fst, X} end, Fst),
           lists:foreach(SendNodes, A),
           send_node(Target, lists:nth(Clustersize, Fst)),
           send_node(lists:nth(Clustersize, Fst), Target),
           NewTarget = lists:nth(1, Fst),
           init_nodes_seperated_rec(Min, Max, Clustersize, NewTarget, Snd);
       Length > 0 ->
           A = lists:map(fun(X) -> {List, X} end, List),
           lists:foreach(SendNodes, A),
           send_node(Target, lists:nth(1, List)),
           send_node(lists:nth(1, List), Target);
       true -> ok
    end.

build_adjacency_list() ->
    List     = get_node_names(),
    AdjLists = lists:filter(fun(V) -> V /= ok end, lists:map(fun(Node) -> dump_nodes(Node) end, List)),
    AdjList  = lists:append(AdjLists),
    lists:filter(fun({_, To}) -> (To /= node()) and not_debug(To) end, AdjList).

find_diameter() ->
    AdjList = build_adjacency_list(),
    {ok, P} = python:start(),
    R = python:call(P, 'src.test', diameter, [AdjList]),
    python:stop(P),
    R.

average_clustering() ->
    AdjList = build_adjacency_list(),
    {ok, P} = python:start(),
    R = python:call(P, 'src.test', clustering, [AdjList]),
    python:stop(P),
    R.

min_max_nodes() ->
    List = get_node_names(),
    Lengths = lists:filter(fun(V) -> V /= 0 end, lists:map(fun(Node) -> length(dump_nodes(Node)) end, List)),
    Min = lists:min(Lengths),
    Max = lists:max(Lengths),
    {Min, Max}.

% Returns short-names of all nodes
get_node_snames() ->
    {_, Names} = net_adm:names(),
    Tmp = lists:filter(fun({N, _}) -> string:prefix(N, "debug") == nomatch end, Names),
    lists:map(fun({N, _}) -> N end, Tmp).

% Return long-names of all nodes
get_node_names() ->
    Hostname = net_adm:localhost(),
    lists:map(fun(N) -> list_to_atom(N ++ "@" ++ Hostname) end, get_node_snames()).

not_debug(N) -> string:prefix(atom_to_list(N), "debug") == nomatch.

start() -> start([noloop]).
start(Arg) ->
    if node() == nonode@nohost ->
           net_kernel:start([debug, shortnames]);
       true -> ok
    end,
    {ok, N} = net_adm:names(),
    io:fwrite("Online nodes: ~p~n", [lists:flatlength(N) - 1]),
    io:fwrite("Started~n"),
    erlang:set_cookie(node(), 'DNBYOWDTEAGDQMYFWGOE'),
    LoopArg = lists:member(loop, Arg),
    PlotArg = lists:member(plot, Arg),
    if LoopArg ->
           redraw_loop();
       PlotArg ->
           Iterations = get_int(Arg),
           init_nodes_seperated(4, 7, 10),
           spawn(debug, generate_data, [Iterations]);
           %redraw_loop();
        true       -> ok
    end.

generate_data(Iterations) ->
    {Diameter, Clustering} = generate_data_rec(Iterations, []),
    DiameterFormat = "time,diameter\n" ++ lists:map(
                       fun({X, T}) -> lists:flatten(io_lib:format("~p,~p~n", [X, T])) end,
                       lists:zip(lists:seq(1, length(Diameter)), Diameter)),
    ClusteringFormat = "time,clustering\n" ++ lists:map(
                         fun({X, T}) -> lists:flatten(io_lib:format("~p,~p~n", [X, T])) end,
                         lists:zip(lists:seq(1, length(Clustering)), Clustering)),
    ok = file:write_file("diameter.csv", DiameterFormat),
    ok = file:write_file("clustering.csv", ClusteringFormat),
    io:fwrite("~nDone!~n").

generate_data_rec(Iterations, Acc) ->
    if Iterations =< 0 -> {[], []};
       true ->
           timer:sleep(1000),
           if Iterations rem 25 == 0 ->
                  graph:generate(Acc, "network_" ++ integer_to_list(floor(Iterations / 25)) ++ ".gv");
              true ->
                  ok
           end,
           D = find_diameter(),
           C = average_clustering(),
           io:fwrite("i: ~-5w d: ~-5w c: ~-5.2. f        \r", [Iterations, D, C]),
           {Dt, Ct} = generate_data_rec(Iterations - 1, graph:get_edges()),
           {[D] ++ Dt, [C] ++ Ct}
    end.

redraw_loop() -> redraw_loop([]).
redraw_loop(Acc) ->
    timer:sleep(10000),
    graph:generate(Acc, ""),
    redraw_loop(graph:get_edges()).

% Return first element from list which can be converted to an integer
get_int([H|T]) ->
    try list_to_integer(atom_to_list(H)) of
        Res -> Res
    catch
        error:_ -> get_int(T);
        _:_     -> io:fwrite("Unhandled Exception!~n")
    end.
