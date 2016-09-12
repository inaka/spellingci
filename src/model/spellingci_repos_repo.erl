-module(spellingci_repos_repo).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ find/1
        , create/6
        , update/1
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
            , spellingci_repos:id()
            , spellingci_repos:name()
            , spellingci_repos:name()
            , spellingci_repos:url()
            , boolean()
            ) -> spellingci_repos:repo().
create(Id, UserId, Name, FullName, Url, Private) ->
  Repo = spellingci_repos:new(Id, UserId, Name, FullName, Url, Private),
  sumo:persist(github_repos, Repo).

-spec update(spellingci_repos:repo()) -> spellingci_repos:repo().
update(Repo) ->
  Now = calendar:universal_time(),
  Repo2 = spellingci_repos:updated_at(Repo, Now),
  sumo:persist(github_repos, Repo2).
