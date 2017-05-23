-module(spellingci_logout_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ is_authorized/2
        , delete_resource/2
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
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
                   , sr_state:state()
                   ) -> { Result
                        , cowboy_req:req()
                        , sr_state:state()} when
    Result :: true | {false, binary()}.
is_authorized(Req, State) ->
  {Token, _} = cowboy_req:cookie(<<"token">>, Req, undefined),
  case spellingci_sessions_repo:valid_session(Token) of
    false ->
      {{false, <<"Realm=spellingci">>}, Req, State};
    {true, Session} ->
      State2 = sr_state:set(session, Session, State),
      {true, Req, State2}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sumo Rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_resource(cowboy_req:req(), sr_state:state()) ->
  {boolean(), cowboy_req:req(), sr_state:state()}.
delete_resource(Req, State) ->
  Session = sr_state:retrieve(session, State, undefined),
  Token = spellingci_sessions:token(Session),
  Result = spellingci_sessions_repo:delete(Token) =/= not_found,
  {Result, Req, State}.
