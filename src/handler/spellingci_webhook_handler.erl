-module(spellingci_webhook_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ handle_post/2
        , delete_resource/2
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ spellingci_auth_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , is_authorized/2
          ]
        }]).

-export([ trails/0
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trails
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"Repository">>
     , in => body
     , description =>
        <<"Repository user wants to add/remove webhook (as json)">>
     , required => true
     },
  Metadata =
    #{ post =>
       #{ description => "Adds SpellingCI webhook to repository"
        , consumes => ["application/json", "application/json;charset=UTF-8"]
        , parameters => [RequestBody]
        }
     , delete =>
       #{ description => "Removes SpellingCI webhook from repository"
        , consumes => ["application/json", "application/json;charset=UTF-8"]
        , parameters => [RequestBody]
        }
     },
  Path = "/webhook",
  Opts = #{ path => Path
          , model => github_users
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sumo Rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_post(cowboy_req:req(), sr_state:state()) ->
  {halt | true, cowboy_req:req(), sr_state:state()}.
handle_post(Req, State) ->
  case common_handler(on, Req, State) of
    {ok, Result} ->
      Result;
    {error, {Code, Req1, State1}} ->
      {ok, Req2} = cowboy_req:reply(Code, Req1),
      {halt, Req2, State1}
  end.


-spec delete_resource(cowboy_req:req(), sr_state:state()) ->
  {boolean(), cowboy_req:req(), sr_state:state()}.
delete_resource(Req, State) ->
  case common_handler(off, Req, State) of
    {ok, Result} ->
      Result;
    {error, {Code, Req1, State1}} ->
      {ok, Req2} = cowboy_req:reply(Code, Req1),
      {false, Req2, State1}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec common_handler( spellingci_repos:status()
                    , cowboy_req:req()
                    , sr_state:state()) ->
  {true , cowboy_req:req(), sr_state:state()} |
  {error, {non_neg_integer(), cowboy_req:req(), sr_state:state()}}.
common_handler(RepoState, Req, State) ->
  try
    User = sr_state:retrieve(user, State, undefined),
    {ok, Body, Req2} = cowboy_req:body(Req),
    RepoDecoded = sr_json:decode(Body),
    {ok, Repo} = spellingci_repos:from_json(RepoDecoded),
    FullName = spellingci_repos:full_name(Repo),
    ok = webhook_action(RepoState, FullName, User),
    {ok, {true, Req2, State}}
  catch
    _:badjson -> {error, {400, Req, State}}
  end.

-spec webhook_action( spellingci_repos:status()
                    , spellingci_repos:name()
                    , spellingci_users:user()
                    ) -> ok.
webhook_action(on, FullName, User) ->
  spellingci_github_utils:webhook_on(FullName, User);
webhook_action(off, FullName, User) ->
  spellingci_github_utils:webhook_off(FullName, User).
