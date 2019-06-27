-module(graph).

-export([generate/0, generate/2, get_edges/0]).

-import(debug, [get_node_snames/0]).

-record(graph_node, {name="noname", color="skyblue"}).

generate() -> generate([], "").
generate(OldList, Filename) ->
    CurrentNetwork = get_edges(),
    DeletedNodes   = sets:to_list(sets:subtract(
                                    sets:from_list(OldList),
                                    sets:from_list(CurrentNetwork))),
    OldNodes       = sets:to_list(sets:intersection(
                                    sets:from_list(OldList),
                                    sets:from_list(CurrentNetwork))),
    NewNodes       = sets:to_list(sets:subtract(
                                    sets:from_list(CurrentNetwork),
                                    sets:from_list(OldList))),
    Out = [DeletedNodes,
           OldNodes,
           NewNodes],
    % Get all nodes as one exlusive list
    AllNodes = lists:sort(get_node_snames()),

    File = if Filename /= "" -> Filename;
              true           -> "network.gv"
           end,
    {ok, F} = file:open(File, [write]),

    file:write(F, "strict graph G {\n"),
    file:write(F, "    start=1;\n"),
    file:write(F, "    edge [constraint=true];\n"),
    file:write(F, "    node [shape=circle,style=filled,width=0.6,fixedsize=true];\n"),
    % We write all nodes sorted first for consistensy
    lists:map(fun(A) ->
                      file:write(F, io_lib:format("    {node [color=skyblue] ~s};~n", [A]))
              end, AllNodes),
    % Then we add relations / edges
    file:write(F, "    edge [color=red];\n"),
    file:write(F, dot_format_node_list(lists:nth(1, Out))), % deleted
    file:write(F, "    edge [color=gray];\n"),
    file:write(F, dot_format_node_list(lists:nth(2, Out))), % old
    file:write(F, "    edge [color=green];\n"),
    file:write(F, dot_format_node_list(lists:nth(3, Out))), % new
    file:write(F, "}\n"),
    file:close(F),
    os:cmd("circo -Tpdf " ++ File ++ " -o " ++ File ++ ".pdf").

dot_format_node_list(List) ->
    lists:flatten(lists:foldl(fun({A, B}, Acc) ->
                                      lists:flatten(
                                        io_lib:format("    ~s -- ~s;~n",
                                                      [A#graph_node.name, B#graph_node.name])) ++ Acc end,
                              "",
                              List)).

get_edges() ->
    List = lists:map(fun({A, B}) ->
                             At = lists:nth(1, string:split(atom_to_list(A), "@")),
                             Bt = lists:nth(1, string:split(atom_to_list(B), "@")),
                             {#graph_node{name=min(At, Bt)},
                              #graph_node{name=max(At, Bt)}}
                     end,
                     debug:build_adjacency_list()),
    lists:sort(fun({A1, A2}, {B1, B2}) ->
                       if A1 == B1 -> A2 =< B2;
                          true     -> A1 =< B1
                       end
               end,
               sets:to_list(sets:from_list(List))).
