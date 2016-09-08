-module(spellingci_login_handler).
-author("Felipe Ripoll <ferigis@gmail.com>").

-export([ init/3
        , handle/2
        , terminate/3
        ]).

%trails
-export([trails/0]).

-type state() :: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trails
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec trails() -> trails:trails().
trails() ->
  MsgTrailsMetadata =
    #{ get => #{ desc => "Starts the oauth login process with Github"
               , 'content-type' => "text/plain"
               }
     },
  [trails:trail("/oauth/login", ?MODULE, [], MsgTrailsMetadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, cowboy_req:req(), state()}.
init(_Transport, Req, []) ->
  {ok, Req, #{}}.

%% @hidden
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  {ok, ClientId} = application:get_env(spellingci, github_client_id),
  {ok, Scope} = application:get_env(spellingci, github_scope),
  Url = "https://github.com/login/oauth/authorize?"
  ++ "client_id=" ++ ClientId
  ++ "&scope=" ++ Scope,
  Headers = [{<<"Location">>, Url}],
  Body = [],
  {ok, Req2} = cowboy_req:reply(302, Headers, Body, Req),
  {ok, Req2, State}.

-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
  ok.
