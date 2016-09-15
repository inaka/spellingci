-module(spellingci_sessions).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behavior(sumo_doc).

-export([ new/4
        , token/1
        , user_id/1
        , created_at/1
        , expires_at/1
        ]).

%%% sumo_db callbacks
-export([ sumo_schema/0
        , sumo_wakeup/1
        , sumo_sleep/1
        ]).

%%% Types
-type token()    :: binary().

-opaque session()     ::
  #{ token      := token()
   , user_id    := spellingci_users:id()
   , created_at := calendar:datetime()
   , expires_at := calendar:datetime()
   }.

-export_type([ session/0
             , token/0
             ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new( token()
         , spellingci_users:id()
         , calendar:datetime()
         , calendar:datetime()
         ) -> session().
new(Token, UserId, CreatedAt, ExpiresAt) ->
  #{ token      => Token
   , user_id    => UserId
   , created_at => CreatedAt
   , expires_at => ExpiresAt
   }.

%% Getters/Setters
-spec token(session()) -> token().
token(Session) ->
  maps:get(token, Session).

-spec user_id(session()) -> spellingci_users:id().
user_id(Session) ->
  maps:get(user_id, Session).

-spec created_at(session()) -> calendar:datetime().
created_at(Session) ->
  maps:get(created_at, Session).

-spec expires_at(session()) -> calendar:datetime().
expires_at(Session) ->
  maps:get(expires_at, Session).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_db callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [ sumo:new_field(token,      string,   [id, not_null])
    , sumo:new_field(user_id,    integer,  [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    , sumo:new_field(expires_at, datetime, [not_null])
    ],
  sumo:new_schema(spellingci_sessions, Fields).

-spec sumo_sleep(session()) -> sumo:model().
sumo_sleep(Session) ->
  Session.

-spec sumo_wakeup(sumo:model()) -> session().
sumo_wakeup(Session) ->
  Session.
