-module(spellingci_webhook_callback_handler).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% remove this line after inaka/erlang-github#123
-dialyzer({nowarn_function, [handle/2, process_request/1]}).

-export([ init/3
        , handle/2
        , terminate/3
        ]).

% trails
-export([trails/0]).

% types
-type state() :: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trails
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec trails() -> trails:trails().
trails() ->
  MsgTrailsMetadata =
    #{ get => #{ desc => "Webhook Callback for Pull Requests handling"
               , 'content-type' => "application/json"
               }
     },
  [trails:trail("/webhook/callback", ?MODULE, [], MsgTrailsMetadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), []) ->
  {ok, cowboy_req:req(), state()}.
init(_Transport, Req, []) ->
  {ok, Req, #{}}.

%% @hidden
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  {Headers, Req} = cowboy_req:headers(Req),
  HeadersMap = maps:from_list(Headers),
  {ok, Body, Req2} = cowboy_req:body(Req),
  Request = #{headers => HeadersMap, body => Body},
  ok = process_request(Request),
  {ok, Req2, State}.

%% @hidden
-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_request(map()) -> ok.
process_request(#{body := Body} = Request) ->
  Mod = spellingci_webhook,
  Cred = commenter_credentials(),
  #{<<"pull_request">> := #{<<"user">> := #{<<"id">> := UserId}}} =
    jiffy:decode(Body, [return_maps]),
  case spellingci_users_repo:find(UserId) of
    not_found ->
      egithub_webhook:event(Mod, Cred, Request);
    User ->
      Token = spellingci_users:github_token(User),
      StatusCred = egithub:oauth(Token),
      Name = "Sheldon",
      Context = "SpellingCI",
      egithub_webhook:event(Mod, StatusCred, Name, Context, Cred, Request)
  end.

-spec commenter_credentials() -> egithub:credentials().
commenter_credentials() ->
  User = application:get_env(spellingci, github_user, ""),
  Password = application:get_env(spellingci, github_password, ""),
  egithub:basic_auth(User, Password).
