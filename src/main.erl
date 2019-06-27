-module(main).

-export([start/0, start/1, stop/0,
         get_or_create_id/1,
         issue_command/1,
         log/1,
         reload_code/0, % broken
         % Loops
         dump_nodes/0,
         listen_for_commands/0,
         listen_for_names/1,
         monitor_neighbors/0,
         receive_loop/0,
         reconfigure_loop/0,
         % Database
         close_tables/0,
         dump_table/0,
         init_tables/0,
         nuke_tables/0,
         receive_nodes/1,
         % Node management
         add_node/1,
         delete_node/1,
         get_active_nodes/0,
         get_all_nodes/0,
         lookup/1
        ]).

% Macro definitions
-define(ID_FILE, idfile).
-define(INTERVAL, 2500).
-define(RECONFIGURE_ANYWAY, 0.50).
-define(MIN_NODES, 5).
-define(MAX_NODES, 10).
-define(TIMEOUT, 5000).
-define(DB_AUTOSAVE, 10000). % 10th second

% id           = randomized hash to identify the individual node
% address      = last known ip/dynamic name
% last_contact = last time this node spoke to that node
-record(node_record, {address, last_contact, is_active}).

start() -> start([?ID_FILE]).
start([IdFile|_]) ->
    io:fwrite("IdFile: \"~p\"~n", [IdFile]),
    % (1) Get identity
    case node() of
        nonode@nohost ->
            Id = get_or_create_id(IdFile),
            net_kernel:start([Id, shortnames]);
        _ -> ok
    end,
    io:fwrite("Node ID: ~p~n", [node()]),
    erlang:set_cookie(node(), 'DNBYOWDTEAGDQMYFWGOE'),
    % (1) Start volatile memory
    init_tables(),
    % (1) Start node
    % (3) Check Database for friends
    % (1) Listen for nodes
    register(name_listener, spawn(main, listen_for_names, [self()])),
    % (1) Listen Commands
    register(command_listener, spawn(main, listen_for_commands, [])),
    register(node_dumper, spawn(main, dump_nodes, [])),
    register(main_proc, self()),
    % (1) Gossip
    % (2) Configure
    register(main_thread, spawn(main, reconfigure_loop, [])),

    link(whereis(name_listener)),
    link(whereis(command_listener)),
    link(whereis(node_dumper)),
    link(whereis(main_thread)),
    receive_loop().

receive_nodes() -> receive_nodes([]).
receive_nodes(L) ->
    receive
        finished   -> L;
        {Node}     -> if Node == node() ->
                              receive_nodes(L);
                         true ->
                              receive_nodes([Node|L])
                      end
    after
        ?TIMEOUT -> L
    end.

monitor_neighbors() ->
  AllNodes = get_all_nodes(),
  if length(AllNodes) > 0 ->
       {M, S, _} = os:timestamp(),
       Now = M * 1000000 + S,
       lists:foreach(fun(Node) ->
                         case net_adm:ping(Node) of
                           pong -> dets:insert(node_table, #node_record{address=Node,
                                                                        last_contact=Now,
                                                                        is_active=true});
                           pang -> NodeLookup = lookup(Node),
                                   Timestamp  = if length(NodeLookup) > 0 ->
                                                     (lists:nth(1, NodeLookup))#node_record.last_contact;
                                                   true -> 0
                                                end,
                                   Active = Timestamp > (Now - 600), % 5 mins ago
                                   dets:insert(node_table, #node_record{address=Node,
                                                                        last_contact=Timestamp,
                                                                        is_active=Active})
                         end
                     end, AllNodes);
     true -> ok
  end.

reconfigure_loop() ->
  timer:sleep(?INTERVAL),
  ActiveNodes       = get_active_nodes(),
  HasTooFewNodes    = length(ActiveNodes) =< ?MIN_NODES,
  HasTooManyNodes   = length(ActiveNodes) > ?MAX_NODES,
  ReconfigureAnyway = rand:uniform()      < ?RECONFIGURE_ANYWAY,
  if
    HasTooFewNodes ->
      get_node_from_neighbor();
    HasTooManyNodes ->
      NodeLists = lists:map(fun(Node) -> {node_dumper, Node} ! self(),
                                         {receive_nodes(), Node} end,
                            ActiveNodes),
      NodeCounts = lists:map(fun({List, Node}) -> {length(List), Node} end, NodeLists),
      {_, NodeWithMostConnections} = lists:max(NodeCounts),
      delete_node(NodeWithMostConnections);
    ReconfigureAnyway ->
      RelevantNodesLength = length(ActiveNodes),
      if
        RelevantNodesLength > 0 ->
          Neighbor = lists:nth(rand:uniform(length(ActiveNodes)), ActiveNodes),
          {node_dumper, Neighbor} ! self(),
          Nodes = lists:filter(fun (X) -> not(lists:member(X, [node() | ActiveNodes])) end, receive_nodes()),
          if
            length(Nodes) > 0 ->
              NewNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
              handle_new_node(NewNode),
              erlang:disconnect_node(Neighbor),
              delete_node(Neighbor),
              io:fwrite("Reconfiged anyway, by dropping ~w, and connecting ~w~n", [Neighbor, NewNode]);
            true ->
              ok
          end;
        true ->
          ok
      end;
    true ->
      ok
  end,
  monitor_neighbors(),
  reconfigure_loop().

get_node_from_neighbor() ->
    RelevantNodes = get_active_nodes(),
    NumberOfNeighbors = length(RelevantNodes),
    if NumberOfNeighbors > 0 ->
            N = rand:uniform(NumberOfNeighbors),
            Neighbor = lists:nth(N, RelevantNodes),
            {node_dumper, Neighbor} ! self(),
            Nodes = receive_nodes(),
            if
                length(Nodes) == 0 ->
                    ok;
                true ->
                    NewNodeNumber = rand:uniform(length(Nodes)),
                    handle_new_node(lists:nth(NewNodeNumber, Nodes))
            end;
       true ->
            ok
    end.

stop() ->
    io:fwrite("Reaping all process!~n"),
    dets:sync(command_table),
    dets:sync(node_table),
    erlang:halt().

receive_loop() ->
    receive
        {new_node, Node} ->
            % Handle new node.
            io:fwrite("Got new node: ~w~n", [Node]),
            handle_new_node(Node);
        {new_command, Command} ->
            % Handle Command
            io:fwrite("Got new command: ~w~n", [Command]),
            if
               Command == reload -> reload_code();
               Command == die    -> stop();
               true              -> undefined
            end;
        {Undef, Cmd} ->
            log("receive_loop: received \"~p\" : \"~p\" -- which is not recognized as a command", [Undef, Cmd])
    end,
    receive_loop().

handle_new_node(Node) ->
    case net_adm:ping(Node) of
        pong ->
            % Insert in table
            add_node(Node);
        pang ->
            % Handle error
            io:fwrite("New node: ~w, could not be reached\n", [Node]),
            log("~w could not be reached", [Node])
    end.

dump_table() ->
    io:fwrite("  Commands~n"),
    io:fwrite("~70..-s~n", ["-"]),
    dets:traverse(command_table,
                  fun({N}) -> io:fwrite(" ~s~n", [N]),
                            continue
                  end
    ),
    io:fwrite("~n  Nodes~n"),
    io:fwrite(" ~-15s | ~-25s | ~-25s~n~70..-s~n", ["Address", "Lastcontact", "Active", "-"]),
    dets:traverse(node_table,
                  fun(N) -> io:fwrite(" ~-15w | ~-25w | ~-25w~n", [N#node_record.address,
                                                                   N#node_record.last_contact,
                                                                   N#node_record.is_active]),
                            continue
                  end
    ),
    ok.

listen_for_names(ReturnPID) ->
    receive
        die ->
            stop();
        finished -> io:fwrite("Finished");
        {Node, AckPID} ->
            ReturnPID ! {new_node, Node},
            AckPID ! ok,
            listen_for_names(ReturnPID);
        Node ->
            io:fwrite("Got new node: ~w\n", [Node]),
            ReturnPID ! {new_node, Node},
            listen_for_names(ReturnPID)
    end.

listen_for_commands() ->
    receive
        die -> log("recv die command"), stop();
        {Cmd, Id, Hops, Sender} ->
            io:fwrite("Received command with id: ~s hops: ~p~n", [Id, Hops]),
            Length = length(dets:lookup(command_table, Id)),
            if Length == 0 ->
                   log("Received command '~s' hops: ~p~n", [Id, Hops]),
                   dets:insert(command_table, {Id}),
                   RelevantNodes = lists:filter(fun(N) -> (N /= Sender) end, get_active_nodes()),
                   send_command({Cmd, Id, Hops+1, node()}, RelevantNodes);
               true -> ok
            end;
        _ ->
            io:fwrite("Received malformed command~n", [])
    end,
    listen_for_commands().

dump_nodes() ->
    receive
        FromPid ->
            lists:foreach(fun(Node) -> FromPid ! {Node} end, get_active_nodes()),
            FromPid ! finished
    end,
    dump_nodes().

get_or_create_id(IdFile) ->
    {Status, Id} = file:read_file(IdFile),
    if
        Status == error ->
            {_, File} = file:open(IdFile, [write]),
            {MgSecs, Secs, MsSec} = os:timestamp(),
            Ts = io_lib:format("~p~p~p", [MgSecs,Secs,MsSec]),
            Tz = lists:flatten(Ts),
            % Create hash from concatenated timestamp
            [H|_] = io_lib:format("~s", [[io_lib:format("~2.16.0b",[X]) || <<X:8>> <= crypto:hash(sha256, Tz) ]]),
            {Tmp, _} = lists:split(2, H),
            NewId = string:join(["n"] ++ Tmp, ""),
            file:write(File, NewId),
            file:close(File),
            list_to_atom(NewId);
        true ->
            list_to_atom(lists:flatten(io_lib:format("~s", [Id])))
    end.

init_tables() ->
    {CmdFileStatus, _} = dets:open_file(command_table,
                             [{file, "tmp/" ++ atom_to_list(id_from_address(node())) ++ ".cmd.dat"},
                              {access,    read_write},
                              {auto_save, ?DB_AUTOSAVE},
                              {ram_file,  true},
                              {repair,    true}]),
    {NodeFileStatus, _} = dets:open_file(node_table,
                             [{file, "tmp/" ++ atom_to_list(id_from_address(node())) ++ ".dat"},
                              {keypos,    #node_record.address},
                              {access,    read_write},
                              {auto_save, ?DB_AUTOSAVE},
                              {ram_file,  true},
                              {repair,    true}]),
    Status = (CmdFileStatus == ok) and (NodeFileStatus == ok),
    if not Status ->
           io:fwrite("nuke"),
           nuke_tables(),
           %timer:sleep(500),
           init_tables();
       true -> ok
    end.

close_tables() ->
    dets:close(command_table),
    dets:close(node_table).

nuke_tables() ->
    close_tables(),
    os:cmd("rm -f tmp/" ++ atom_to_list(id_from_address(node())) ++ ".dat"),
    os:cmd("rm -f tmp/" ++ atom_to_list(id_from_address(node())) ++ ".cmd.dat").

id_from_address(Address) ->
    [H|_] = string:split(atom_to_list(Address), "@"),
    list_to_atom(H).

reload_code() ->
    stop(),
    code:purge(?MODULE),
    compile:file(?MODULE),
    code:load_file(?MODULE),
    ?MODULE:start().

issue_command(Str) ->
    Id = base64:encode(crypto:strong_rand_bytes(16)),
    dets:insert(command_table, {Id}),
    Msg = {Str, Id, 0, node()},
    send_command(Msg, get_active_nodes()),
    notify_debug(Msg).

send_command(Cmd, Targets) ->
    Send = fun (Node) -> {command_listener, Node} ! Cmd end,
    lists:foreach(Send, Targets).

notify_debug(Cmd) ->
    {_, Id, Hops, _} = Cmd,
    io:fwrite("Notify debug here, Id: ~s, Hops ~w~n", [Id, Hops]).

log(Message, Args) -> log(io_lib:format(Message, Args)).
log(Message) ->
    FileName = "tmp/" ++ lists:nth(1, string:tokens(atom_to_list(node()), "@")) ++ ".log",
    {Status, File} = file:open(FileName, [append]),
    case Status of
        ok ->
            {_, S, _} = os:timestamp(),
            Timestamp = lists:flatten(io_lib:format("[~p] ", [S])),
            file:write(File, Timestamp ++ Message ++ ["\n"]),
            file:close(File);
        _ ->
            io:fwrite("Error! Failed to open logfile\n", [])
    end,
    Status.

add_node(NodeAddress) ->
    ValidNode = (NodeAddress /= node()) and (string:prefix(atom_to_list(NodeAddress), "debug") == nomatch),
    if ValidNode ->
           {M, S, _} = os:timestamp(),
           Timestamp = M * 1000000 + S,
           dets:insert(node_table, #node_record{address=NodeAddress,
                                                last_contact=Timestamp,
                                                is_active=true}),
           ok;
       true -> invalid_node
    end.

delete_node(NodeAddress) ->
    dets:delete(node_table, NodeAddress),
    erlang:disconnect_node(NodeAddress),
    io:fwrite("Deleted ~w\n", [NodeAddress]).

lookup(NodeId) ->
    dets:lookup(node_table, NodeId).

get_all_nodes() ->
    dets:traverse(node_table,
                  fun({_, Addr, _, _}) ->
                          {continue, Addr}
                  end).

get_active_nodes() ->
    dets:traverse(node_table,
                  fun({_, Addr, _, A}) ->
                          if A ->
                                 {continue, Addr};
                             true ->
                                 continue
                          end
                  end).
