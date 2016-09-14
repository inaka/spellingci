-module(spellingci_repos_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ is_authorized/2
        , handle_get/2
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_post/2
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
       #{ description => "Returns the list of Github Repositories"
        , produces    => ["application/json"]
        }
     },
  Path = "/repos",
  Opts = #{ path => Path
          , model => github_repos
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_authorized( cowboy_req:req()
                   , sr_entities_handler:state()
                   ) -> { Result
                        , cowboy_req:req()
                        , sr_entities_handler:state()} when
    Result :: true | {false, binary()}.
is_authorized(Req, State) ->
  {AuthToken, _} = cowboy_req:cookie(<<"token">>, Req, undefined),
  case spellingci_users_repo:valid_auth_token(AuthToken) of
    false        -> {{false, <<"Realm=spellingci">>}, Req, State};
    {true, User} -> {true, Req, State#{user => User}}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sumo Rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_get(cowboy_req:req(), sr_entities_handler:state()) ->
  {iodata(), cowboy_req:req(), sr_entities_handler:state()}.
handle_get(Req, #{user := User} = State) ->
  Repos = spellingci_repos_repo:repos(User),
  Reply = [spellingci_repos:to_json(Repo) || Repo <- Repos],
  JSON  = sr_json:encode(Reply),
  {JSON, Req, State}.
