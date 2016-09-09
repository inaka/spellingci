-module(spellingci_SUITE).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ connect/1
        , github_login/1
        , user_model/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ connect
          , github_login
          , user_model
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

-spec user_model(config()) -> ok.
user_model(_Config) ->
  User = spellingci_users:new( <<"felipe">>
                            , <<"Felipe Ripoll">>
                            , <<"token1234">>
                            , <<"felipe@inakanetworks.com">>
                            ),
  PersistedUser = sumo:persist(github_users, User),
  Id = spellingci_users:id(PersistedUser),
  true = (Id /= undefined),
  <<"Felipe Ripoll">> = spellingci_users:name(PersistedUser),
  <<"felipe">> = spellingci_users:username(PersistedUser),
  <<"token1234">> = spellingci_users:github_token(PersistedUser),
  <<"felipe@inakanetworks.com">> = spellingci_users:email(PersistedUser),
  undefined = spellingci_users:auth_token(PersistedUser),
  undefined = spellingci_users:auth_expires(PersistedUser),
  undefined = spellingci_users:auth_token(PersistedUser),
  undefined = spellingci_users:synced_at(PersistedUser),
  true = (spellingci_users:updated_at(PersistedUser) /= undefined),

  % updating the user
  Now = calendar:universal_time(),
  User2 = spellingci_users:name(PersistedUser, <<"Felipe Ripoll Gisbert">>),
  User3 = spellingci_users:github_token(User2, <<"1234token">>),
  User4 = spellingci_users:email(User3, <<"ferigis@gmail.com">>),
  User5 = spellingci_users:auth_token(User4, <<"auth_token123">>),
  User6 = spellingci_users:auth_expires(User5, Now),
  User7 = spellingci_users:synced_at(User6, Now),
  User8 = spellingci_users:updated_at(User7, Now),
  UserPersisted2 = sumo:persist(github_users, User8),
  UserPersisted2 = sumo:find(github_users, Id),
  <<"Felipe Ripoll Gisbert">> = spellingci_users:name(UserPersisted2),
  <<"1234token">> = spellingci_users:github_token(UserPersisted2),
  <<"ferigis@gmail.com">> = spellingci_users:email(UserPersisted2),
  <<"auth_token123">> = spellingci_users:auth_token(UserPersisted2),
  Now = spellingci_users:auth_expires(UserPersisted2),
  Now = spellingci_users:synced_at(UserPersisted2),
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
