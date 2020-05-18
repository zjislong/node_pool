%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(node_pool_srv).
-behaviour(gen_server).

%%修改自OTP_HOME/stdlib/src/pool.erl
%%去掉了sqwan功能和与slave.erl相关联功能
%%使用{global,PoolName}替换{global,pool_master},可以同时存在多个不同的pool.
%%并用node_pool_sup来挂载这些pool_srv.

%% Supplies a computational pool of processors.
%% The chief user interface function here is get_node()
%% Which returns the name of the nodes in the pool
%% with the least load !!!!
%% This function is callable from any node including the master
%% That is part of the pool
%% nodes are scheduled on a per usgae basis and per load basis,
%% Whenever we use a node, we put at the end of the queue, and whenever
%% a node report a change in load, we insert it accordingly

% User interface Exports ...
-export([start_link/1]).

%% Internal Exports
-export([statistic_collector/1,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

%% Start up using the .hosts.erlang fil
-spec start_link(Name::atom()) -> {ok,pid()}.
start_link(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [], []).

%%
%% Interface functions ...
%%
init([]) ->
    process_flag(trap_exit, true),
    {ok,[]}.

handle_call(get_nodes, _From, Nodes)->
    {reply, Nodes, Nodes};
handle_call(get_node, _From, []) ->
    {reply, error, []};
handle_call(get_node, _From, [{_,N}|_]=Nodes) ->
    {reply, N, Nodes};
handle_call({attach, Name, Node}, _From, Nodes) ->
    case lists:keymember(Node, 2, Nodes) of
	    true ->
	        {reply, already_attached, Nodes};
	    false ->
	        erlang:monitor_node(Node, true),
	        spawn_link(Node, ?MODULE, statistic_collector, [Name]),
	        {reply, attached, Nodes++[{999999,Node}]}
    end.

handle_cast(_, Nodes) ->
    {noreply, Nodes}.

handle_info({Node,load,Load}, Nodes) ->
    case lists:keymember(Node, 2, Nodes) of
	    true ->
            {noreply, insert_node({Load,Node}, Nodes)};
	    false ->
	        {noreply, Nodes}
    end;
handle_info({nodedown, Node}, Nodes) ->
    {noreply, lists:keydelete(Node, 2, Nodes)};
handle_info(_, Nodes) ->  %% The EXIT signals etc.etc
    {noreply, Nodes}.

terminate(_Reason, _Nodes) ->
    ok.

insert_node({Load,Node},[{L,Node}|Tail]) when Load > L ->
    %% We have a raised load here
    pure_insert({Load,Node},Tail);
insert_node({Load,Node},[{L,N}|Tail]) when Load =< L ->
    %% Move forward in the list
    T = lists:keydelete(Node,2,[{L,N}|Tail]),
    [{Load,Node} | T];
insert_node(Ln,[H|T]) ->
    [H | insert_node(Ln,T)];
insert_node(X,[]) ->          % Can't happen
    error_logger:error_msg("Pool_master: Bad node list X=~w\n", [X]),
    exit(crash).

pure_insert({Load,Node},[]) ->
    [{Load,Node}];
pure_insert({Load,Node},[{L,N}|Tail]) when Load < L ->
    [{Load,Node}, {L,N} | Tail];
pure_insert(L,[H|T]) -> [H|pure_insert(L,T)].

statistic_collector(Name) ->
    statistic_collector(Name,5).

statistic_collector(_Name, 0)->
    exit(normal);
statistic_collector(Name,I)->
    case global:whereis_name(Name) of
	    undefined ->
            sleep(300),
            statistic_collector(Name, I-1);
        _->
            cpu_usage(),
            stat_loop(Name, 999999)
    end.

%% Do not tell the master about our load if it has not  changed
stat_loop(Name, Old) ->
    sleep(10000),
    case cpu_usage() of
	    Old ->
	        stat_loop(Name, Old);
	    NewLoad ->
            case global:whereis_name(Name) of
	            Pid when is_pid(Pid) ->
	                Pid ! {node(), load, NewLoad}, %% async ,
	                stat_loop(Name, NewLoad);
	            undefined ->
                    %% master nodedown,need apply node_pool:attach/2 again when reconnect successful.
	                exit(normal)
            end
    end.

sleep(I) -> receive after I -> ok end.

cpu_load() ->
    case file:open("/proc/loadavg",[read,raw]) of
        {ok,F} ->
            {ok,D} = file:read_line(F),
            ok = file:close(F),
            {ok,[Load1,_,_,_,_],_} = io_lib:fread("~f ~f ~f ~d/~d", D),
            Load1/erlang:system_info(logical_processors);
        {error,_} ->
            0
    end.

cpu_usage() ->
    case file:open("/proc/stat",[read,raw]) of
        {ok,F} ->
            {ok,D} = file:read_line(F),
            ok = file:close(F),
            {ok,[Us, Sy, Ni, Id, Wa, Hi, Si, St],_} = io_lib:fread("cpu  ~d ~d ~d ~d ~d ~d ~d ~d", D),
            Total = Us + Sy + Ni + Id + Wa + Hi + Si + St,
            case erlang:put(cpu_usage, {Id, Total}) of
                undefined ->
                    0;
                {LastId, LastTotal} ->
                    100 - (Id - LastId) / (Total - LastTotal) * 100
            end;
        {error,_} ->
            0
    end.
