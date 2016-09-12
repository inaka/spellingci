-module(spellingci_users_repo).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ create/4
        , find/1
        , update_auth_token/1
        , update/1
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

-spec update_auth_token(spellingci_users:user()) -> spellingci_users:user().
update_auth_token(User) ->
  Now = calendar:universal_time(),
  Expires = add_days(Now, 1),
  UUID = uuid(),
  User1 = spellingci_users:auth_token(User, UUID),
  User2 = spellingci_users:auth_expires(User1, Expires),
  sumo:persist(github_users, User2).

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

-spec uuid() -> binary().
uuid() ->
  R1 = rand:uniform(round(math:pow(2, 48))) - 1,
  R2 = rand:uniform(round(math:pow(2, 12))) - 1,
  R3 = rand:uniform(round(math:pow(2, 32))) - 1,
  R4 = rand:uniform(round(math:pow(2, 30))) - 1,
  Uuid = uuid(R1, R2, R3, R4),
  to_binary(Uuid).

-spec uuid( pos_integer()
          , pos_integer()
          , pos_integer()
          , pos_integer()
          ) -> binary().
uuid(R1, R2, R3, R4) ->
  <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

-spec to_binary(binary()) -> binary().
to_binary(U) ->
  iolist_to_binary(
    io_lib:format(
      "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
      get_parts(U)
     )
   ).

-spec get_parts(binary()) -> list().
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
  [TL, TM, THV, CSR, CSL, N].
