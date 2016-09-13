%%% @hidden
%%% @doc spellingci's Application behaviour.
%%%
%%% Copyright 2016 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(spellingci).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(application).

-export([ start/0
        , start/2
        , stop/0
        , stop/1
        , start_phase/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the Application
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(spellingci),
  ok.

%% @doc Stops the Application
-spec stop() -> ok.
stop() ->
  ok = application:stop(spellingci),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type::application:start_type(), Args::any()) -> {ok, pid()}.
start(_Type, _Args) -> {ok, self()}.

-spec stop(State::[]) -> ok.
stop(_State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% START PHASES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec start_phase(atom(), StartType::application:start_type(), []) ->
  ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  Node = node(),
  _ = application:stop(mnesia),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  ok = application:start(mnesia),
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  {ok, Port} = application:get_env(spellingci, http_port),
  {ok, ListenerCount} = application:get_env(spellingci, http_listener_count),

  Handlers = [ spellingci_login_handler
             , spellingci_callback_handler
             , spellingci_repos_handler
             ],

  % Get the trails for each handler
  Trails = [ { "/"
             , cowboy_static
             , {file, filename:join([code:priv_dir(spellingci), "index.html"])}}
           , { "/assets/[...]"
             , cowboy_static
             , {dir, filename:join([code:priv_dir(spellingci), "assets"])}}
           | trails:trails(Handlers)
           ],

  % Store them so Cowboy is able to get them
  trails:store(Trails),
  % Set server routes
  Dispatch = trails:single_host_compile(Trails),
  % Set the options for the TCP layer
  TransOpts = [{port, Port}],
  % Set the options for the HTTP layer
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  % Start Cowboy HTTP server
  case cowboy:start_http( spellingci_server
                        , ListenerCount
                        , TransOpts
                        , ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.
