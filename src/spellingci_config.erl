-module(spellingci_config).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
-export([ default/0
        , normalize/1
        , sheldon_config/1
        , extensions/1
        ]).

-export_type([ config/0
             ]).

-opaque config() :: #{ extensions     := [binary()]
                     , sheldon_config := sheldon_config:config()
                     }.

%%%===================================================================
%%% API
%%%===================================================================

-spec default() -> config().
default() ->
  #{ extensions     => default_extensions()
   , sheldon_config => sheldon_config:default()
   }.

-spec normalize(map()) -> config().
normalize(Config) ->
  #{ extensions     => normalize_extensions(Config)
   , sheldon_config => normalize_sheldon_config(Config)
   }.

-spec sheldon_config(config()) -> sheldon_config:config().
sheldon_config(#{sheldon_config := SheldonConfig}) ->
  SheldonConfig.

-spec extensions(config()) -> [binary()].
extensions(#{extensions := Extensions}) ->
  Extensions.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec normalize_extensions(map()) -> [string()].
normalize_extensions(Config) ->
  maps:get(<<"extensions">>, Config, default_extensions()).

-spec normalize_sheldon_config(map()) -> sheldon_config:config().
normalize_sheldon_config(Config) ->
  SheldonConfig = maps:get(sheldon_config, Config, sheldon_config:default()),
  sheldon_config:normalize(SheldonConfig).

-spec default_extensions() -> [binary()].
default_extensions() -> [<<"md">>, <<"markdown">>].
