-module(spellingci_users_repo).
-author("Felipe Ripoll <ferigis@gmail.com>").

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
  case sumo:find(github_users, Id) of
    notfound -> not_found;
    User     -> User
  end.

-spec save_token(binary()) -> spellingci_users:token() | undefined.
save_token(Token) ->
  Cred = egithub:oauth(Token),
  {ok, GitHubUser} = egithub:user(Cred),
  Id = maps:get(<<"id">>, GitHubUser, null),
  UserName = maps:get(<<"login">>, GitHubUser, null),
  Name = maps:get(<<"name">>, GitHubUser, null),
  User = case find(Id) of
    not_found -> create(Id, UserName, Name, Token);
    FoundUser -> FoundUser
  end,
  User2 = spellingci_users:github_token(User, Token),
  User3 = update_auth_token(User2),
  AuthUser = update(User3),
  spellingci_users:auth_token(AuthUser).

-spec update(spellingci_users:user()) -> spellingci_users:user().
update(User) ->
  sumo:persist(github_users, User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add_days(calendar:datetime(), integer()) -> calendar:datetime().
add_days({Date, Time}, Days) ->
  DaysToDate = calendar:date_to_gregorian_days(Date) + Days,
  NewDate = calendar:gregorian_days_to_date(DaysToDate),
  {NewDate, Time}.

-spec update_auth_token(spellingci_users:user()) -> spellingci_users:user().
update_auth_token(User) ->
  Now = calendar:universal_time(),
  Expires = add_days(Now, 1),
  UUID = uuid(),
  User1 = spellingci_users:auth_token(User, UUID),
  spellingci_users:auth_expires(User1, Expires).

-spec uuid() -> binary().
uuid() ->
  State = uuid:new(self()),
  {Uuid, _} = uuid:get_v1(State),
  UuidString = uuid:uuid_to_string(Uuid),
  list_to_binary(UuidString).
