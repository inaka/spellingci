-module(spellingci_repos).
-author("Felipe Ripoll <ferigis@gmail.com>").

-behavior(sumo_doc).

-export([ new/6
        , id/1
        , user_id/1
        , name /1
        , full_name/1
        , url/1
        , private/1
        , status/1
        , status/2
        , created_at/1
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
-type url()      :: binary().
-type name()     :: binary().
-type status()   :: on | off.

-opaque repo() ::
  #{ id         := id()
   , user_id    := spellingci_users:id()
   , name       := name()
   , full_name  := name()
   , url        := url()
   , private    := boolean()
   , status     := status()
   , created_at := calendar:datetime()
   , updated_at := calendar:datetime()
   }.

 -export_type([ repo/0
              , id/0
              ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(id(), id(), name(), name(), url(), boolean()) -> repo().
new(Id, UserId, Name, FullName, Url, Private) ->
  Now = calendar:universal_time(),
  #{ id         => Id
   , user_id    => UserId
   , name       => Name
   , full_name  => FullName
   , url        => Url
   , private    => Private
   , status     => off
   , created_at => Now
   , updated_at => Now
   }.

%% Getters/Setters
-spec id(repo()) -> id().
id(Repo) ->
  maps:get(id, Repo).

-spec user_id(repo()) -> spellingci_users:id().
user_id(Repo) ->
  maps:get(user_id, Repo).

-spec name(repo()) -> name().
name(Repo) ->
  maps:get(name, Repo).

-spec full_name(repo()) -> name().
full_name(Repo) ->
  maps:get(full_name, Repo).

-spec url(repo()) -> url().
url(Repo) ->
  maps:get(url, Repo).

-spec private(repo()) -> boolean().
private(Repo) ->
  maps:get(private, Repo).

-spec status(repo()) -> status().
status(Repo) ->
  maps:get(status, Repo).

-spec status(repo(), status()) -> repo().
status(Repo, Status) ->
  Repo#{status => Status}.

-spec created_at(repo()) -> calendar:datetime().
created_at(Repo) ->
  maps:get(created_at, Repo).

-spec updated_at(repo()) -> calendar:datetime().
updated_at(Repo) ->
  maps:get(updated_at, Repo).

-spec updated_at(repo(), calendar:datetime()) -> repo().
updated_at(Repo, Value) ->
  Repo#{updated_at => Value}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_db callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [ sumo:new_field(id,         integer,  [id, not_null])
    , sumo:new_field(user_id,    integer,  [not_null])
    , sumo:new_field(name,       string,   [{length, 255}, not_null])
    , sumo:new_field(full_name,  string,   [{length, 255}, not_null])
    , sumo:new_field(url,        string,   [{length, 255}, not_null])
    , sumo:new_field(private,    boolean,  [not_null])
    , sumo:new_field(status,     boolean,  [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    , sumo:new_field(updated_at, datetime, [not_null])
    ],
  sumo:new_schema(github_repos, Fields).

-spec sumo_sleep(repo()) -> sumo:model().
sumo_sleep(Repo) ->
  Repo.

-spec sumo_wakeup(sumo:model()) -> repo().
sumo_wakeup(Repo) ->
  Repo.
