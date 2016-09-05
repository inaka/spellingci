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
-author("Felipe Ripoll <ferigis@gmail.com>").

-behaviour(application).

-export([ start/0
        , start/2
        ]).
-export([ stop/0
        , stop/1
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
