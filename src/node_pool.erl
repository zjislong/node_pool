-module(node_pool).


% User interface Exports ...
-export([new_pool/1,
	 delete_pool/1,
	 get_nodes/1,
	 get_nodes_and_load/1,
	 get_node/1,
	 attach/3]).

%% User interface

-spec new_pool(Name::atom()) -> {ok, pid()}.
new_pool(Name) ->
	case global:whereis_name(Name) of
	        Pid when is_pid(Pid) ->
	            {ok, Pid};
	        undefined ->
	            supervisor:start_child(node_pool_sup, [Name])
    end.

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
    get_elements(3, get_nodes_and_load(Name)).

-spec attach(Name::atom(),Node,Weight) -> already_attached | attached when
      Node :: node(),Weight :: non_neg_integer().
attach(Name, Node, Weight) ->
    gen_server:call({global, Name}, {attach, Node, Weight}).

-spec get_nodes_and_load(Name::atom()) -> [{integer(),Weight::integer(),node()}].
get_nodes_and_load(Name) ->
    gen_server:call({global, Name}, get_nodes).

-spec get_node(Name::atom()) -> node().
get_node(Name) ->
    gen_server:call({global, Name}, get_node).

get_elements(_Pos,[]) -> [];
get_elements(Pos,[E|T]) -> [element(Pos,E) | get_elements(Pos,T)].
