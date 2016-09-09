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
  User = spellingci_user:new( <<"felipe">>
                            , <<"Felipe Ripoll">>
                            , <<"token1234">>
                            , <<"felipe@inakanetworks.com">>
                            ),
  PersistedUser = sumo:persist(user, User),
  Id = spellingci_user:id(PersistedUser),
  true = (Id /= undefined),
  <<"Felipe Ripoll">> = spellingci_user:name(PersistedUser),
  <<"felipe">> = spellingci_user:username(PersistedUser),
  <<"token1234">> = spellingci_user:github_token(PersistedUser),
  <<"felipe@inakanetworks.com">> = spellingci_user:email(PersistedUser),
  undefined = spellingci_user:auth_token(PersistedUser),
  undefined = spellingci_user:auth_expires(PersistedUser),
  undefined = spellingci_user:auth_token(PersistedUser),
  undefined = spellingci_user:synced_at(PersistedUser),
  true = (spellingci_user:updated_at(PersistedUser) /= undefined),

  % updating the user
  Now = spellingci_utils:now_datetime(),
  User2 = spellingci_user:name(PersistedUser, <<"Felipe Ripoll Gisbert">>),
  User3 = spellingci_user:github_token(User2, <<"1234token">>),
  User4 = spellingci_user:email(User3, <<"ferigis@gmail.com">>),
  User5 = spellingci_user:auth_token(User4, <<"auth_token123">>),
  User6 = spellingci_user:auth_expires(User5, Now),
  User7 = spellingci_user:synced_at(User6, Now),
  User8 = spellingci_user:updated_at(User7, Now),
  UserPersisted2 = sumo:persist(user, User8),
  UserPersisted2 = sumo:find(user, Id),
  <<"Felipe Ripoll Gisbert">> = spellingci_user:name(UserPersisted2),
  <<"1234token">> = spellingci_user:github_token(UserPersisted2),
  <<"ferigis@gmail.com">> = spellingci_user:email(UserPersisted2),
  <<"auth_token123">> = spellingci_user:auth_token(UserPersisted2),
  Now = spellingci_user:auth_expires(UserPersisted2),
  Now = spellingci_user:synced_at(UserPersisted2),
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
