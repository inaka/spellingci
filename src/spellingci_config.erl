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
  IgnoreWords = maps:get(<<"ignore_words">>, Config, []),
  IgnoreBlocks = maps:get(<<"ignore_blocks">>, Config, []),
  NormalizeBlocksFun = fun(#{<<"open">> := Open, <<"close">> := Close}) ->
    #{open => Open, close => Close}
  end,
  SheldonConfig = #{ ignore_words    =>
                       lists:map(fun binary_to_list/1, IgnoreWords)
                   , ignore_patterns =>
                       maps:get(<<"ignore_patterns">>, Config, [])
                   , ignore_blocks   =>
                       lists:map(NormalizeBlocksFun, IgnoreBlocks)
                   },
  sheldon_config:normalize(SheldonConfig).

-spec default_extensions() -> [binary()].
default_extensions() -> [<<"md">>, <<"markdown">>].
