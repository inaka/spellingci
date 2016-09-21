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
  Metadata =
    #{ post =>
       #{ description => "Adds SpellingCI webhook to repository"
        }
     , delete =>
       #{ description => "Removes SpellingCI webhook from repository"
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

-spec handle_post(cowboy_req:req(), sr_entities_handler:state()) ->
  {halt | true, cowboy_req:req(), sr_entities_handler:state()}.
handle_post(Req, State) ->
  common_handler(on, Req, State).

-spec delete_resource(cowboy_req:req(), sr_single_entity_handler:state()) ->
  {boolean(), cowboy_req:req(), sr_single_entity_handler:state()}.
delete_resource(Req, State) ->
  common_handler(off, Req, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec common_handler( spellingci_repos:status()
                    , cowboy_req:req()
                    , sr_single_entity_handler:state()) ->
  {boolean(), cowboy_req:req(), sr_single_entity_handler:state()}.
common_handler(RepoState, Req, #{user := User} = State) ->
  try
    {ok, Body, Req2} = cowboy_req:body(Req),
    RepoDecoded = sr_json:decode(Body),
    {ok, Repo} = spellingci_repos:from_json(RepoDecoded),
    FullName = spellingci_repos:full_name(Repo),
    case webhook_action(RepoState, FullName, User) of
      ok ->
        {true, Req2, State};
      {error, private_repo} ->
        Error = "Only public repos are allowed",
        _ = lager:error(Error),
        {ok, Req3} = cowboy_req:reply(403, Req2),
        {false, Req3, State}
    end
  catch
    _:badjson ->
    {ok, Req1} = cowboy_req:reply(400, Req),
    {false, Req1, State}
  end.

-spec webhook_action( spellingci_repos:status()
                    , spellingci_repos:name()
                    , spellingci_users:user()
                    ) -> ok | {error, private_repo}.
webhook_action(on, FullName, User) ->
  spellingci_github_utils:webhook_on(FullName, User);
webhook_action(off, FullName, User) ->
  spellingci_github_utils:webhook_off(FullName, User).
