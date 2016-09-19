-module(spellingci_repos_repo).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ find/1
        , create/6
        , update/1
        , repos/1
        , sync/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec find(spellingci_repos:id()) -> spellingci_repos:repo() | not_found.
find(Id) ->
  case sumo:find(github_repos, Id) of
    notfound -> not_found;
    User     -> User
  end.

-spec create( spellingci_repos:id()
            , spellingci_users:id()
            , spellingci_repos:name()
            , spellingci_repos:name()
            , spellingci_repos:url()
            , spellingci_repos:private()
            ) -> spellingci_repos:repo().
create(Id, UserId, Name, FullName, Url, Private) ->
  Repo = spellingci_repos:new(Id, UserId, Name, FullName, Url, Private),
  sumo:persist(github_repos, Repo).

-spec update(spellingci_repos:repo()) -> spellingci_repos:repo().
update(Repo) ->
  Now = calendar:universal_time(),
  Repo2 = spellingci_repos:updated_at(Repo, Now),
  sumo:persist(github_repos, Repo2).

-spec repos(spellingci_users:user()) -> [spellingci_repos:repo()].
repos(User) ->
  case spellingci_users:synced_at(User) of
    undefined -> from_github(User);
    _         -> find_by_user(User)
  end.

-spec sync(spellingci_users:user()) -> [spellingci_repos:repo()].
sync(User) ->
  from_github(User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec find_by_user(spellingci_users:user()) -> [spellingci_repos:repo()].
find_by_user(User) ->
  _ = lager:info("fetching from DB..."),
  UserId = spellingci_users:id(User),
  Conditions = [{user_id, UserId}],
  sumo:find_by(github_repos, Conditions).

-spec from_github(spellingci_users:user()) -> [spellingci_repos:repo()].
from_github(User) ->
  _ = lager:info("fetching from github..."),
  Token = spellingci_users:github_token(User),
  Cred = egithub:oauth(Token),
  Opts = #{type => <<"owner">>},
  {ok, GithubRepos} = egithub:repos(Cred, Opts),
  Now = calendar:universal_time(),
  User2 = spellingci_users:synced_at(User, Now),
  User2 = spellingci_users_repo:update(User2),
  ok = delete_by_user(User),
  [create_from_github(User, GR) || GR <- GithubRepos].

-spec create_from_github(spellingci_users:user(), map()) ->
  spellingci_repos:repo().
create_from_github(User, GithubRepo) ->
  Id = maps:get(<<"id">>, GithubRepo),
  UserId = spellingci_users:id(User),
  Name = maps:get(<<"name">>, GithubRepo),
  FullName = maps:get(<<"full_name">>, GithubRepo),
  Url = maps:get(<<"html_url">>, GithubRepo),
  Private = maps:get(<<"private">>, GithubRepo),
  create(Id, UserId, Name, FullName, Url, Private).

-spec delete_by_user(spellingci_users:user()) -> ok.
delete_by_user(User) ->
  Conditions = [{user_id, spellingci_users:id(User)}],
  _ = sumo:delete_by(github_repos, Conditions),
  ok.
