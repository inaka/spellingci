-module(spellingci_logout_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ is_authorized/2
        , delete_resource/2
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
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
    #{ delete =>
       #{ description => "Logout user"
        }
     },
  Path = "/logout",
  Opts = #{ path  => Path
          , model => spellingci_sessions
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_authorized( cowboy_req:req()
                   , sr_entities_handler:state()
                   ) -> { Result
                        , cowboy_req:req()
                        , sr_single_entity_handler:state()} when
    Result :: true | {false, binary()}.
is_authorized(Req, State) ->
  {Token, _} = cowboy_req:cookie(<<"token">>, Req, undefined),
  case spellingci_sessions_repo:valid_session(Token) of
    false           -> {{false, <<"Realm=spellingci">>}, Req, State};
    {true, Session} -> {true, Req, State#{session => Session}}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sumo Rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_resource(cowboy_req:req(), sr_single_entity_handler:state()) ->
  {boolean(), cowboy_req:req(), sr_single_entity_handler:state()}.
delete_resource(Req, #{session := Session} = State) ->
  Token = spellingci_sessions:token(Session),
  Result = spellingci_sessions_repo:delete(Token) =/= not_found,
  {Result, Req, State}.
