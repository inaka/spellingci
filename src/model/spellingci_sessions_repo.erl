-module(spellingci_sessions_repo).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ create/1
        , find/1
        , delete/1
        , valid_session/1
        , clean_sessions/0
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

-spec find(spellingci_sessions:token()) ->
  spellingci_sessions:session() | not_found.
find(Token) ->
  case sumo:fetch(spellingci_sessions, Token) of
    notfound -> not_found;
    Session  -> Session
  end.

-spec delete(spellingci_sessions:token()) -> ok | not_found.
delete(Token) ->
  case sumo:delete(spellingci_sessions, Token) of
    true -> ok;
    false -> not_found
  end.

-spec valid_session(spellingci_sessions:token()) ->
  {true, spellingci_sessions:session()} | false.
valid_session(Token) ->
  Now = calendar:universal_time(),
  Conditions = [ {token, Token}
               , {expires_at, '>', Now}
               ],
  case sumo:find_by(spellingci_sessions, Conditions) of
    []        -> false;
    [Session] -> {true, Session}
  end.

-spec clean_sessions() -> ok.
clean_sessions() ->
  _ = lager:info("Cleaning sessions..."),
  Now = calendar:universal_time(),
  Conditions = [{expires_at, '<', Now}],
  _ = sumo:delete_by(spellingci_sessions, Conditions),
  ok.

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
