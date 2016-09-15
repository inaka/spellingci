-module(spellingci_SUITE).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ connect/1
        , github_login/1
        , auth_cookie/1
        , list_repos/1
        , clean_sessions/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ connect
          , github_login
          , auth_cookie
          , list_repos
          , clean_sessions
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
  ok = meck:expect(spellingci_sessions_repo, valid_session, fun(_) ->
      {true, #{user_id => 3}}
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

-spec clean_sessions(config()) -> ok.
clean_sessions(_Config) ->
  ok = spellingci_session_gc:change_frequency(20000),
  % Clean the sessions table first
  ok = delete_sessions(),
  Session1 = spellingci_sessions_repo:create(1),
  Session2 = spellingci_sessions_repo:create(1),
  Session3 = spellingci_sessions_repo:create(2),
  Session4 = spellingci_sessions_repo:create(3),
  Session5 = spellingci_sessions_repo:create(1),
  5 = length(mnesia:dirty_all_keys(spellingci_sessions)),
  ok = expire_session(Session3),
  ok = expire_session(Session5),
  ok = spellingci_session_gc:force_clean(),
  3 = length(mnesia:dirty_all_keys(spellingci_sessions)),
  ok = expire_session(Session1),
  ok = expire_session(Session2),
  ok = spellingci_session_gc:force_clean(),
  1 = length(mnesia:dirty_all_keys(spellingci_sessions)),
  Session4 = spellingci_sessions_repo:find(spellingci_sessions:token(Session4)),
  ok = expire_session(Session4),
  ok = spellingci_session_gc:force_clean(),
  0 = length(mnesia:dirty_all_keys(spellingci_sessions)),
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

-spec delete_sessions() -> ok.
delete_sessions() ->
  Tokens = mnesia:dirty_all_keys(spellingci_sessions),
  [spellingci_sessions_repo:delete(Token) || Token <- Tokens],
  ok.

-spec expire_session(spellingci_sessions:session()) -> ok.
expire_session(Session) ->
  Token = spellingci_sessions:token(Session),
  UserId = spellingci_sessions:user_id(Session),
  CreatedAt = spellingci_sessions:created_at(Session),
  Session1 = spellingci_sessions:new( Token
                                    , UserId
                                    , CreatedAt
                                    , subtract_days(CreatedAt, 1)
                                    ),
  _ = sumo:persist(spellingci_sessions, Session1),
  ok.

-spec subtract_days(calendar:datetime(), integer()) -> calendar:datetime().
subtract_days({Date, Time}, Days) ->
  DaysToDate = calendar:date_to_gregorian_days(Date) - Days,
  NewDate = calendar:gregorian_days_to_date(DaysToDate),
  {NewDate, Time}.
