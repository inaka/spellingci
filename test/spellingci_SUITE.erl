-module(spellingci_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ cover/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ cover
          ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = spellingci:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = spellingci:stop(),
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cover(config()) -> ok.
cover(_Config) -> ok.
