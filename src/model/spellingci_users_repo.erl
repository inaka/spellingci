-module(spellingci_users_repo).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ create/4
        , find/1
        , update/1
        , save_token/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create( spellingci_users:id()
            , spellingci_users:username()
            , spellingci_users:name()
            , spellingci_users:token()
            ) -> spellingci_users:user().
create(Id, Username, Name, GitHubToken) ->
  User = spellingci_users:new(Id, Username, Name, GitHubToken),
  sumo:persist(github_users, User).

-spec find(spellingci_users:id()) -> spellingci_users:user() | not_found.
find(Id) ->
  case sumo:fetch(github_users, Id) of
    notfound -> not_found;
    User     -> User
  end.

-spec save_token(spellingci_users:token()) -> spellingci_users:user().
save_token(GithubToken) ->
  Cred = egithub:oauth(GithubToken),
  {ok, GitHubUser} = egithub:user(Cred),
  Id = maps:get(<<"id">>, GitHubUser, null),
  User = case find(Id) of
    not_found ->
      UserName = maps:get(<<"login">>, GitHubUser, null),
      Name = maps:get(<<"name">>, GitHubUser, null),
      create(Id, UserName, Name, GithubToken);
    FoundUser -> FoundUser
  end,
  User2 = spellingci_users:github_token(User, GithubToken),
  update(User2).

-spec update(spellingci_users:user()) -> spellingci_users:user().
update(User) ->
  sumo:persist(github_users, User).
