-module(spellingci_repos_sync_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ handle_get/2
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
    #{ get =>
       #{ description => "Synchronizes the list of Github Repositories"
        , produces    => ["application/json"]
        }
     },
  Path = "/repos/sync",
  Opts = #{ path => Path
          , model => github_repos
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sumo Rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_get(cowboy_req:req(), sr_entities_handler:state()) ->
  {iodata(), cowboy_req:req(), sr_entities_handler:state()}.
handle_get(Req, #{user := User} = State) ->
  Repos = spellingci_repos_repo:sync(User),
  Reply = [spellingci_repos:to_json(Repo) || Repo <- Repos],
  JSON  = sr_json:encode(Reply),
  {JSON, Req, State}.
