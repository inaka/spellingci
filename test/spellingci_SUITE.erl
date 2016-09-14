-module(spellingci_SUITE).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ connect/1
        , github_login/1
        , user_model/1
        , repo_model/1
        , auth_cookie/1
        , list_repos/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ connect
          , github_login
          , user_model
          , repo_model
          , auth_cookie
          , list_repos
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
  {ok, 302, Result, _} = api_call("/oauth/login"),
  Location = proplists:get_value(<<"Location">>, Result),
  <<"https://github.com/login/oauth/authorize?", _/binary>> = Location,

  % check the callback
  {ok, 400, _, Client} = api_call("/oauth/callback?code=1234567890"),
  {ok, <<"Error: ", Body/binary >>} = hackney:body(Client),
  404 =  jiffy:decode(Body, [return_maps]),
  ok.

-spec user_model(config()) -> ok.
user_model(_Config) ->
  Id = 1,
  User = spellingci_users_repo:create( Id
                                     , <<"felipe">>
                                     , <<"Felipe Ripoll">>
                                     , <<"token1234">>
                                     ),
  Id = spellingci_users:id(User),
  <<"Felipe Ripoll">> = spellingci_users:name(User),
  <<"felipe">> = spellingci_users:username(User),
  <<"token1234">> = spellingci_users:github_token(User),
  undefined = spellingci_users:auth_token(User),
  undefined = spellingci_users:auth_expires(User),
  undefined = spellingci_users:auth_token(User),
  undefined = spellingci_users:synced_at(User),
  true = (spellingci_users:updated_at(User) /= undefined),

  % updating the user
  Now = calendar:universal_time(),
  User2 = spellingci_users:name(User, <<"Felipe Ripoll Gisbert">>),
  User3 = spellingci_users:github_token(User2, <<"1234token">>),
  User4 = spellingci_users:auth_token(User3, <<"auth_token123">>),
  User5 = spellingci_users:auth_expires(User4, Now),
  User6 = spellingci_users:synced_at(User5, Now),
  User7 = spellingci_users:updated_at(User6, Now),
  UserPersisted2 = sumo:persist(github_users, User7),
  UserPersisted2 = spellingci_users_repo:find(Id),
  <<"Felipe Ripoll Gisbert">> = spellingci_users:name(UserPersisted2),
  <<"1234token">> = spellingci_users:github_token(UserPersisted2),
  <<"auth_token123">> = spellingci_users:auth_token(UserPersisted2),
  Now = spellingci_users:auth_expires(UserPersisted2),
  Now = spellingci_users:synced_at(UserPersisted2),
  not_found = spellingci_users_repo:find(2),
  ok.

-spec repo_model(config()) -> ok.
repo_model(_Config) ->
  Id = 1,
  UserId = 2,
  Name = <<"RepoName">>,
  FullName = <<"Full/RepoName">>,
  Url = <<"https://github.com/Full/RepoName">>,
  Private = false,
  Repo = spellingci_repos_repo:create(Id, UserId, Name, FullName, Url, Private),
  Repo = spellingci_repos_repo:find(Id),
  Id = spellingci_repos:id(Repo),
  UserId = spellingci_repos:user_id(Repo),
  Name = spellingci_repos:name(Repo),
  FullName = spellingci_repos:full_name(Repo),
  Url = spellingci_repos:url(Repo),
  Private = spellingci_repos:private(Repo),
  off = spellingci_repos:status(Repo),
  Created = spellingci_repos:created_at(Repo),
  Created = spellingci_repos:updated_at(Repo),
  Repo2 = spellingci_repos:status(Repo, on),
  _Repo3 = spellingci_repos_repo:update(Repo2),
  Repo4 = spellingci_repos_repo:find(Id),
  on = spellingci_repos:status(Repo4),
  not_found = spellingci_repos_repo:find(2),
  ok.

-spec auth_cookie(config()) -> ok.
auth_cookie(_Config) ->
  ok = meck:expect(hackney, request, fun(post, _Url, _Headers, _Body, _Opts) ->
      {ok, 200, [], client}
    end),
  ok = meck:expect(hackney, body, fun(client) ->
      {ok, <<"{ \"access_token\":\"acces_token1234\"
              , \"token_type\":\"bearer\"
              ,\"scope\":\"repo,user:email\"}">>}
  end),
  ok = meck:expect(egithub, oauth, fun(<<"acces_token1234">>) ->
      {oauth, <<"987654321">>}
    end),
  ok = meck:expect(egithub, user, fun({oauth, <<"987654321">>}) ->
      {ok, #{ <<"id">>    => 1234
            , <<"login">> => <<"username">>
            , <<"name">>  => <<"Felipe">>
            }}
    end),
  {ok, 302, RespHeaders, _} = api_call("/oauth/callback?code=12345"),
  true = has_cookie(RespHeaders),

  % test coverage
  {ok, 302, RespHeaders2, _} = api_call("/oauth/callback?code=4321"),
  true = has_cookie(RespHeaders2),
  {ok, 400, _, _} = api_call(<<"/oauth/callback">>),
  ok = meck:expect(hackney, request, fun(post, _Url, _Headers, _Body, _Opts) ->
      {ok, 400, [], client}
    end),
  {ok, 400, _, _} = api_call("/oauth/callback?code=1234"),
  ok = meck:expect(hackney, request, fun(post, _Url, _Headers, _Body, _Opts) ->
      {error, <<"this_is_a_reason">>}
    end),
  {ok, 400, _, _} = api_call("/oauth/callback?code=1234"),
  [_, _] = meck:unload(),
  ok.

-spec list_repos(config()) -> ok.
list_repos(_Config) ->
  User1 = create_user(1),
  User2 = create_user(2),
  User3 = create_user(3),

  ok = meck:expect(egithub, oauth, fun(Token) ->
      {oauth, Token}
    end),
  ok = meck:expect(egithub, repos, fun(_Token, _Opts) ->
      {ok, []}
    end),
  [] = spellingci_repos_repo:repos(User1),
  [] = spellingci_repos_repo:repos(User2),
  Repo1 = create_repo(1, 1),
  Repo2 = create_repo(2, 2),
  User1Persisted = spellingci_users_repo:find(1),
  User2Persisted = spellingci_users_repo:find(2),
  [Repo1] = spellingci_repos_repo:repos(User1Persisted),
  [Repo2] = spellingci_repos_repo:repos(User2Persisted),
  ok = meck:expect(egithub, repos, fun(_Token, _Opts) ->
      {ok, [#{ <<"id">>        => 3
             , <<"name">>      => <<"repofromgithub">>
             , <<"html_url">>  => <<"http://url">>
             , <<"private">>   => false
             , <<"full_name">> => <<"full/Name">>
             }]}
    end),
  [Repo3] = spellingci_repos_repo:repos(User3),
  Repo3 = spellingci_repos_repo:find(3),

  % test thru cowboy
  ok = meck:expect(spellingci_users_repo, valid_auth_token, fun(_) ->
      {true, User3}
    end),
  Headers = [{<<"Cookie">>, <<"token=token3">>}],
  {ok, 200, _, Client} = api_call(get, "/repos", Headers),
  {ok, Json} = hackney:body(Client),
  [RepoDecoded] = jiffy:decode(Json, [return_maps]),
  % Repo3b instead of Repo3 because sometimes the dates vary one second
  {ok, Repo3b} = spellingci_repos:from_json(RepoDecoded),
  true = (spellingci_repos:id(Repo3) == spellingci_repos:id(Repo3b)),

  _ = meck:unload(),

  {ok, 401, _, _} = api_call(get, "/repos", Headers),

  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec test_connection() -> ok.
test_connection() ->
  {ok, 200, Result, _} = api_call("/"),
  ContentLength = proplists:get_value(<<"content-length">>, Result),
  true = (binary_to_integer(ContentLength) > 0),
  ok.

-spec api_call(iodata()) -> {ok, integer(), list(), term()}
                          | {ok, integer(), list()}
                          | {error, term()}.
api_call(Url) ->
  api_call(get, Url).

-spec api_call(atom(), iodata()) -> {ok, integer(), list(), term()}
                                  | {ok, integer(), list()}
                                  | {error, term()}.
api_call(Method, Url) ->
  api_call(Method, Url, []).

-spec api_call(atom(), iodata(), list()) -> {ok, integer(), list(), term()}
                                          | {ok, integer(), list()}
                                          | {error, term()}.
api_call(Method, Url, Headers) ->
  {ok, Port} = application:get_env(spellingci, http_port),
  Url2 = [<<"http://localhost:">>, integer_to_list(Port), Url],
  hackney:request(Method, Url2, Headers).

-spec has_cookie(list()) -> boolean().
has_cookie(Headers) ->
  lists:keyfind(<<"set-cookie">>, 1, Headers) =/= false.

-spec create_repo(spellingci_repos:id(), spellingci_users:id()) ->
  spellingci_repos:repo().
create_repo(RepoId, UserId) ->
  RepoIdBin = integer_to_binary(RepoId),
  spellingci_repos_repo:create( RepoId
                              , UserId
                              , <<"name", RepoIdBin/binary>>
                              , <<"fullname/", RepoIdBin/binary>>
                              , <<"http://url", RepoIdBin/binary>>
                              , false).

-spec create_user(spellingci_users:id()) -> spellingci_users:user().
create_user(UserId) ->
  UserIdBin = integer_to_binary(UserId),
  spellingci_users_repo:create( UserId
                              , <<"UserName", UserIdBin/binary>>
                              , <<"Name", UserIdBin/binary>>
                              , <<"token", UserIdBin/binary>>).
