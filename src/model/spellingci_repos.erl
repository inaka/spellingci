-module(spellingci_repos).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behavior(sumo_doc).
-behavior(sumo_rest_doc).

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

%%% sumo_rest callbacks
-export([ to_json/1
        , from_json/1
        , update/2
        , location/2
        ]).

%%% Types
-type id()       :: integer().
-type url()      :: binary().
-type name()     :: binary().
-type status()   :: on | off.
-type private()  :: boolean().
-type json()     :: #{atom() | binary() => atom() | binary()}.

-opaque repo()   ::
  #{ id         := id()
   , user_id    := spellingci_users:id()
   , name       := name()
   , full_name  := name()
   , url        := url()
   , private    := private()
   , status     := status()
   , created_at := calendar:datetime()
   , updated_at := calendar:datetime()
   }.

 -export_type([ repo/0
              , id/0
              , name/0
              , url/0
              , private/0
              ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new( id()
         , spellingci_repos:id()
         , name()
         , name()
         , url()
         , private()
         ) -> repo().
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

-spec private(repo()) -> private().
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_rest callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_json(repo()) -> json().
to_json(Repo) ->
  #{ id         => maps:get(id, Repo)
   , user_id    => maps:get(user_id, Repo)
   , name       => maps:get(name, Repo)
   , full_name  => maps:get(full_name, Repo)
   , url        => maps:get(url, Repo)
   , private    => maps:get(private, Repo)
   , status     => maps:get(status, Repo)
   , created_at => sr_json:encode_date(maps:get(created_at, Repo))
   , updated_at => sr_json:encode_date(maps:get(updated_at, Repo))
   }.

-spec from_json(sumo_rest_doc:json()) -> {ok, repo()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ id         => maps:get(<<"id">>, Json)
       , user_id    => maps:get(<<"user_id">>, Json)
       , name       => maps:get(<<"name">>, Json)
       , full_name  => maps:get(<<"full_name">>, Json)
       , url        => maps:get(<<"url">>, Json)
       , private    => maps:get(<<"private">>, Json)
       , status     => maps:get(<<"status">>, Json)
       , created_at =>
           sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , updated_at =>
           sr_json:decode_date(maps:get(<<"updated_at">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(repo(), sumo_rest_doc:json()) ->
  {ok, repo()} | {error, iodata()}.
update(Repo, Json) ->
  try
    NewStatus = maps:get(<<"status">>, Json),
    UpdatedRepo =
      Repo#{status := NewStatus, updated_at := calendar:universal_time()},
    {ok, UpdatedRepo}
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec location(repo(), sumo_rest_doc:path()) -> binary().
location(Repo, Path) -> iolist_to_binary([Path, "/", spellingci_repos:id(Repo)]).
