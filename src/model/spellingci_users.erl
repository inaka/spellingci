-module(spellingci_users).
-author("Felipe Ripoll <ferigis@gmail.com>").

-behavior(sumo_doc).

-export([ new/4
        , id/1
        , username/1
        , name/1
        , name/2
        , github_token/1
        , github_token/2
        , auth_token/1
        , auth_token/2
        , auth_expires/1
        , auth_expires/2
        , synced_at/1
        , synced_at/2
        , updated_at/1
        , updated_at/2
        ]).

%%% sumo_db callbacks
-export([ sumo_schema/0
        , sumo_wakeup/1
        , sumo_sleep/1
        ]).

%%% Types
-type id()       :: integer().
-type username() :: binary().
-type name()     :: binary().
-type token()    :: binary().
-type datetime() :: calendar:datetime() | undefined.

-opaque user()     ::
  #{ id           := id()
   , name         := name()
   , username     := username()
   , github_token := token()
   , auth_token   := token() | undefined
   , auth_expires := datetime()
   , synced_at    := datetime()
   , created_at   := calendar:datetime()
   , updated_at   := calendar:datetime()
   }.

-export_type([ user/0
             , name/0
             , token/0
             , username/0
             , id/0
             ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(id(), username(), name(), token()) -> user().
new(Id, Username, Name, GitHubToken) ->
  Now = calendar:universal_time(),
  #{ id           => Id
   , name         => Name
   , username     => Username
   , github_token => GitHubToken
   , auth_token   => undefined
   , auth_expires => undefined
   , synced_at    => undefined
   , created_at   => Now
   , updated_at   => Now
   }.

%% Getters/Setters
-spec id(user()) -> id() | undefined.
id(User) ->
  maps:get(id, User).

-spec username(user()) -> username().
username(User) ->
  maps:get(username, User).

-spec name(user()) -> name().
name(User) ->
  maps:get(name, User).

-spec name(user(), name()) -> user().
name(User, Value) ->
  User#{name => Value}.

-spec github_token(user()) -> token().
github_token(User) ->
  maps:get(github_token, User).

-spec github_token(user(), token()) -> user().
github_token(User, Value) ->
  User#{github_token => Value}.

-spec auth_token(user()) -> token() | undefined.
auth_token(User) ->
  maps:get(auth_token, User).

-spec auth_token(user(), token()) -> user().
auth_token(User, Value) ->
  User#{auth_token => Value}.

-spec auth_expires(user()) -> datetime().
auth_expires(User) ->
  maps:get(auth_expires, User).

-spec auth_expires(user(), calendar:datetime()) -> user().
auth_expires(User, Value) ->
  User#{auth_expires => Value}.

-spec synced_at(user()) -> datetime().
synced_at(User) ->
  maps:get(synced_at, User).

-spec synced_at(user(), calendar:datetime()) -> user().
synced_at(User, Value) ->
  User#{synced_at => Value}.

-spec updated_at(user()) -> calendar:datetime().
updated_at(User) ->
  maps:get(updated_at, User).

-spec updated_at(user(), calendar:datetime()) -> user().
updated_at(User, Value) ->
  User#{updated_at => Value}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_db callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [ sumo:new_field(id,            integer,  [id, not_null])
    , sumo:new_field(username,      string,   [{length, 255}, not_null])
    , sumo:new_field(name,          string,   [{length, 255}])
    , sumo:new_field(github_token,  string,   [{length, 255}])
    , sumo:new_field(auth_token,    string,   [{length, 255}])
    , sumo:new_field(auth_expires,  datetime)
    , sumo:new_field(synced_at,     datetime)
    , sumo:new_field(created_at,    datetime, [not_null])
    , sumo:new_field(updated_at,    datetime, [not_null])
    ],
  sumo:new_schema(github_users, Fields).

-spec sumo_sleep(user()) -> sumo:model().
sumo_sleep(User) ->
  User.

-spec sumo_wakeup(sumo:model()) -> user().
sumo_wakeup(User) ->
  User.
