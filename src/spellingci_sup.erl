-module(spellingci_sup).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(supervisor).

%% API
-export([ start_link/0
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Supervisor callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => one_for_one
              , intensity => 1000
              , period    => 3600
              },

  SessionsGC = #{ id       => spellingci_session_gc
                , start    => { spellingci_session_gc
                              , start_link
                              , []
                              }
                , restart  => permanent
                , shutdown => brutal_kill
                , type     => worker
                , modules  => [sheldon_dictionary]
                },

  {ok, {SupFlags, [SessionsGC]}}.
