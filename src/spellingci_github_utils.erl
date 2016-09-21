-module(spellingci_github_utils).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ already_hooked/2
        , webhook_on/2
        , webhook_off/2
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec already_hooked(spellingci_users:user(), spellingci_repos:name()) ->
  {true, term()} | false.
already_hooked(User, RepoFullName) ->
  {ok, WebhookUrl} = application:get_env(spellingci, webhook_url),
  WebhookUrlBinary = list_to_binary(WebhookUrl),
  Cred = egithub:oauth(spellingci_users:github_token(User)),
  Hooks = case egithub:hooks(Cred, RepoFullName) of
            {ok, HooksResult} ->
              HooksResult;
            {error, _} ->
              []
          end,
  Fun = fun
          (#{<<"config">> := #{<<"url">> := Url}})
            when Url == WebhookUrlBinary ->
            true;
          (_Other) ->
            false
        end,
  case lists:filter(Fun, Hooks) of
    [] ->
      false;
    [Hook] -> {true, Hook}
  end.

-spec webhook_on( spellingci_repos:name()
                , spellingci_users:user()
                ) -> ok | {error, private_repo}.
webhook_on(RepoFullName, User) ->
  Status = on,
  webhook_common(Status, RepoFullName, User).

-spec webhook_off( spellingci_repos:name()
                 , spellingci_users:user()
                 ) -> ok | {error, private_repo}.
webhook_off(RepoFullName, User) ->
  Status = off,
  webhook_common(Status, RepoFullName, User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec webhook_common( spellingci_repos:status()
                    , spellingci_repos:name()
                    , spellingci_users:user()
                    ) -> ok | {error, private_repo}.
webhook_common(Status, RepoFullName, User) ->
  Token = spellingci_users:github_token(User),
  Cred = egithub:oauth(Token),
  {ok, GithubRepo} = egithub:repo(Cred, RepoFullName),
  Repo = spellingci_repos:from_github(GithubRepo),
  Private = spellingci_repos:private(Repo),
  case not Private of
    true ->
      ok = webhook_action(Status, RepoFullName, User),
      Repo2 = spellingci_repos_repo:find(spellingci_repos:id(Repo)),
      Repo3 = spellingci_repos:status(Repo2, Status),
      _Repo4= spellingci_repos_repo:update(Repo3),
      ok;
    false ->
      {error, private_repo}
  end.

-spec webhook_action( spellingci_repos:status()
                    , spellingci_repos:name()
                    , spellingci_users:user()
                    ) -> ok.
webhook_action(on, RepoFullName, User) ->
  create_webhook(User, RepoFullName);
webhook_action(off, RepoFullName, User) ->
 delete_webhook(User, RepoFullName).

-spec create_webhook(spellingci_users:user(), spellingci_repos:name()) -> ok.
create_webhook(User, RepoFullName) ->
 case already_hooked(User, RepoFullName) of
   false ->
     Events = ["pull_request"],
     Token = spellingci_users:github_token(User),
     Cred = egithub:oauth(Token),
     {ok, WebhookUrl} = application:get_env(spellingci, webhook_url),
     {ok, _Result} =
       egithub:create_webhook(Cred, RepoFullName, WebhookUrl, Events),
     ok;
   {true, _Hook} ->
     ok
 end.

-spec delete_webhook(spellingci_users:user(), spellingci_repos:name()) -> ok.
delete_webhook(User, RepoFullName) ->
  case already_hooked(User, RepoFullName) of
    false ->
      ok;
    {true, Hook} ->
      Token = spellingci_users:github_token(User),
      Cred = egithub:oauth(Token),
      #{<<"id">> := Id} = Hook,
      egithub:delete_webhook(Cred, RepoFullName, Id)
  end.
