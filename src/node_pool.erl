-module(node_pool).


% User interface Exports ...
-export([new_pool/1,
	 delete_pool/1,
	 get_nodes/1, 
	 get_nodes_and_load/1, 
	 get_node/1,
	 attach/2]).

%% User interface 

-spec new_pool(Name::atom()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
new_pool(Name) ->
    supervisor:start_child(node_pool_sup, [Name]).

-spec delete_pool(Name::atom()) -> ok | {error, not_found}.
delete_pool(Name) ->
    case global:whereis_name(Name) of
	        Pid when is_pid(Pid) ->
	            supervisor:terminate_child(node_pool_sup, Pid);
	        undefined ->
	            {error, not_found}
    end.

-spec get_nodes(Name::atom()) -> [node()].
get_nodes(Name) ->
    get_elements(2, get_nodes_and_load(Name)).

-spec attach(Name::atom(),Node) -> already_attached | attached when
      Node :: node().
attach(Name, Node) ->
    gen_server:call({global, Name}, {attach, Name, Node}).

get_nodes_and_load(Name) ->
    gen_server:call({global, Name}, get_nodes).

-spec get_node(Name::atom()) -> node().
get_node(Name) ->
    gen_server:call({global, Name}, get_node).

get_elements(_Pos,[]) -> [];
get_elements(Pos,[E|T]) -> [element(Pos,E) | get_elements(Pos,T)].