-module(spellingci_models_SUITE).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([ user_model/1
        , repo_model/1
        , session_model/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->  [ user_model
          , repo_model
          , session_model
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
  undefined = spellingci_users:synced_at(User),
  true = (spellingci_users:updated_at(User) /= undefined),

  % updating the user
  Now = calendar:universal_time(),
  User2 = spellingci_users:name(User, <<"Felipe Ripoll Gisbert">>),
  User3 = spellingci_users:github_token(User2, <<"1234token">>),
  User4 = spellingci_users:synced_at(User3, Now),
  User5 = spellingci_users:updated_at(User4, Now),
  UserPersisted2 = sumo:persist(github_users, User5),
  UserPersisted2 = spellingci_users_repo:find(Id),
  <<"Felipe Ripoll Gisbert">> = spellingci_users:name(UserPersisted2),
  <<"1234token">> = spellingci_users:github_token(UserPersisted2),
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

-spec session_model(config()) -> ok.
session_model(_Config) ->
  UserId = 1,
  Session1 = spellingci_sessions_repo:create(UserId),
  UserId = spellingci_sessions:user_id(Session1),
  Token = spellingci_sessions:token(Session1),
  Session1 = spellingci_sessions_repo:find(Token),
  not_found = spellingci_sessions_repo:find(<<"bad_token">>),
  not_found = spellingci_sessions_repo:delete(<<"bad_token">>),
  ok = spellingci_sessions_repo:delete(Token),
  not_found = spellingci_sessions_repo:find(Token),
  ok.
