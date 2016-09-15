-module(spellingci_sessions_repo).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ create/1
        , find/1
        , delete/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create(spellingci_users:id()) -> spellingci_sessions:session().
create(UserId) ->
  Token = uuid(),
  Now = calendar:universal_time(),
  ExpiresAt = add_days(Now, 1),
  Repo = spellingci_sessions:new(Token, UserId, Now, ExpiresAt),
  sumo:persist(spellingci_sessions, Repo).

-spec find(spellingci_sessions:token()) -> spellingci_sessions:session() | not_found.
find(Token) ->
  case sumo:find(spellingci_sessions, Token) of
    notfound -> not_found;
    Session  -> Session
  end.

-spec delete(spellingci_sessions:token()) -> ok | not_found.
delete(Token) ->
  case sumo:delete(spellingci_sessions, Token) of
    true -> ok;
    false -> not_found
  end.


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
  State = uuid:new(self()),
  {Uuid, _} = uuid:get_v1(State),
  UuidString = uuid:uuid_to_string(Uuid),
  list_to_binary(UuidString).
