-module(spellingci_callback_handler).
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
    #{ get => #{ desc => "Github oauth callback"
               , 'content-type' => "text/plain"
               }
     },
  [trails:trail("/oauth/callback", ?MODULE, [], MsgTrailsMetadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), atom()}, cowboy_req:req(), term()) ->
  {ok, cowboy_req:req(), state()}.
init(_Type, Req, _Opts) ->
  {ok, Req, #{}}.

%% @hidden
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
  Headers = [{<<"content-type">>, <<"text/plain">>}],
  case cowboy_req:qs_val(<<"code">>, Req) of
    {undefined, Req2} ->
      Body = [<<"Missing 'code' querystring parameter.">>],
      {ok, Req3} = cowboy_req:reply(400, Headers, Body, Req2),
      {ok, Req3, State};
    {Code, Req2} ->
      case access_token(Code) of
        {ok, Token} ->
          AuthToken = save_user(Token),
          Url = "/",
          RedirHeaders = [{<<"Location">>, Url}],
          Req3 = cowboy_req:set_resp_cookie( <<"token">>
                                           , AuthToken
                                           , [{path, <<"/">>}]
                                           , Req2
                                           ),
          {ok, Req4} = cowboy_req:reply(302, RedirHeaders, Req3),
          {ok, Req4, State};
        {error, Reason} ->
          _ = lager:error("~p", [Reason]),
          Body = [<<"Error: ">>, Reason],
          {ok, Req3} = cowboy_req:reply(400, Headers, Body, Req2),
          {ok, Req3, State}
      end
  end.

-spec terminate(term(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec access_token(binary()) -> {ok, spellingci_users:token()}
                              | {error, term()}.
access_token(Code) ->
  {ok, ClientId} = application:get_env(spellingci, github_client_id),
  {ok, ClientSecret} = application:get_env(spellingci, github_client_secret),
  _ = lager:info("~p - ~p", [ClientId, ClientSecret]),
  Url = "https://github.com/login/oauth/access_token",
  Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
             {<<"Accept">>, <<"application/json">>}],
  Body = ["code=", Code,
          "&client_id=", ClientId,
          "&client_secret=", ClientSecret],
  Opts = [{ssl_options, [{depth, 0}]}],
  case hackney:request(post, Url, Headers, Body, Opts) of
    {ok, 200, _RespHeaders, Client} ->
      {ok, RespBody} = hackney:body(Client),
      JsonBody = jiffy:decode(RespBody, [return_maps]),
      case maps:get(<<"access_token">>, JsonBody, undefined) of
        undefined -> {error, RespBody};
        Token     -> {ok, Token}
      end;
    {ok, Status, _, _} ->
      {error, integer_to_list(Status)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec save_user(binary()) -> spellingci_users:token() | undefined.
save_user(Token) ->
  Cred = egithub:oauth(Token),
  {ok, GitHubUser} = egithub:user(Cred),
  Id = maps:get(<<"id">>, GitHubUser, null),
  UserName = maps:get(<<"login">>, GitHubUser, null),
  Name = maps:get(<<"name">>, GitHubUser, null),
  User = case spellingci_users_repo:find(Id) of
    not_found -> spellingci_users_repo:create(Id, UserName, Name, Token);
    FoundUser -> FoundUser
  end,
  User2 = spellingci_users:github_token(User, Token),
  User3 = spellingci_users_repo:update(User2),
  AuthUser = spellingci_users_repo:update_auth_token(User3),
  spellingci_users:auth_token(AuthUser).
