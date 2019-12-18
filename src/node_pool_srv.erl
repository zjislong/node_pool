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
-export([
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
handle_call(get_node, _From, Nodes) ->
    {N, Nodes1} = get_node(Nodes),
    {reply, N, Nodes1};
handle_call({attach, Node, Weight}, _From, Nodes) ->
    {reply, attached, attach_node(Nodes, Node, Weight, [])}.

handle_cast(_, Nodes) ->
    {noreply, Nodes}.

handle_info({nodedown, Node}, Nodes) ->
    {noreply, lists:keydelete(Node, 3, Nodes)};
handle_info(_, Nodes) ->  %% The EXIT signals etc.etc
    {noreply, Nodes}.

terminate(_Reason, _Nodes) ->
    ok.

get_node([{CurWeight, Weight, Node}|Nodes]) ->
    get_node(Nodes, {CurWeight+Weight, Weight, Node}, CurWeight+Weight, []).

get_node([], {CurWeight, Weight, Node}, Total, NodesAcc) ->
    {Node, [{CurWeight-Total, Weight, Node}|NodesAcc]};
get_node([{CurWeight, Weight, Node}|Nodes], {GetCurWeight, _, _} = GetNode, Total, NodesAcc) ->
    CurNode = {CurWeight+Weight, Weight, Node},
    case CurWeight+Weight > GetCurWeight of
        true ->
            get_node(Nodes, CurNode, Total+CurWeight+Weight, [GetNode|NodesAcc]);
        false ->
            get_node(Nodes, GetNode, Total+CurWeight+Weight, [CurNode|NodesAcc])
    end.

attach_node([], undefined, _, NodesAcc) ->
    NodesAcc;
attach_node([], AttachNode, AttachWeight, NodesAcc) ->
    erlang:monitor_node(AttachNode, true),
    [{0, AttachWeight, AttachNode}|NodesAcc];
attach_node([{_, _, AttachNode}|Nodes], AttachNode, 0, NodesAcc) ->
    erlang:monitor_node(AttachNode, false),
    attach_node(Nodes, undefined, 0, NodesAcc);
attach_node([{_, _, AttachNode}|Nodes], AttachNode, AttachWeight, NodesAcc) ->
    attach_node(Nodes, undefined, AttachWeight, [{0, AttachWeight, AttachNode}|NodesAcc]);
attach_node([{_, Weight, Node}|Nodes], AttachNode, AttachWeight, NodesAcc) ->
    attach_node(Nodes, AttachNode, AttachWeight, [{0, Weight, Node}|NodesAcc]).
