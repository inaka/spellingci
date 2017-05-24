-module(spellingci_auth_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-export([ is_authorized/2
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          ]
        }]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_authorized(cowboy_req:req(), sr_state:state()) ->
  {Result, cowboy_req:req(), sr_state:state()} when
    Result :: true | {false, binary()}.
is_authorized(Req, State) ->
  {Token, _} = cowboy_req:cookie(<<"token">>, Req, undefined),
  case spellingci_sessions_repo:valid_session(Token) of
    false ->
      {{false, <<"Realm=spellingci">>}, Req, State};
    {true, Session} ->
      UserId = spellingci_sessions:user_id(Session),
      User = spellingci_users_repo:find(UserId),
      State2 = sr_state:set(user, User, State),
      {true, Req, State2}
  end.
