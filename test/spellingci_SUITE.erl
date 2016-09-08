-module(spellingci_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ connect/1
        , github_login/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ connect
          , github_login
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

-spec connect(config()) -> ok.
connect(_Config) ->
  ok = test_connection(),
  ok.

-spec github_login(config()) -> ok.
github_login(_Config) ->
  % check the login
  {ok, 302, Result, _} = call("/oauth/login"),
  Location = proplists:get_value(<<"Location">>, Result),
  <<"https://github.com/login/oauth/authorize?", _/binary>> = Location,

  % check the callback
  {ok, 400, _, Client} = call("/oauth/callback?code=1234567890"),
  {ok, <<"Error: ", Body/binary >>} = hackney:body(Client),
  404 =  jiffy:decode(Body, [return_maps]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec test_connection() -> ok.
test_connection() ->
  {ok, 200, Result, _} = call("/"),
  ContentLength = proplists:get_value(<<"content-length">>, Result),
  true = (binary_to_integer(ContentLength) > 0),
  ok.

call(Url) ->
  {ok, Port} = application:get_env(spellingci, http_port),
  Url2 = [<<"http://localhost:">>, integer_to_list(Port), Url],
  hackney:request(Url2).
