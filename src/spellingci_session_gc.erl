-module(spellingci_session_gc).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(gen_server).

%% API
-export([ start_link/0
        , force_clean/0
        , change_frequency/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%% Types
-type frequency() :: non_neg_integer().
-type state()     :: #{gc_frequency := frequency()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec force_clean() -> ok.
force_clean() ->
  gen_server:call(?MODULE, force_clean).

-spec change_frequency(frequency()) -> ok.
change_frequency(Frequency) ->
  gen_server:cast(?MODULE, {change_frequency, Frequency}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([frequency()]) -> {ok, state(), timeout()}.
init([]) ->
  GcFrequency =
    application:get_env(spellingci, sessions_gc_frequency, default_frequency()),
  {ok, #{gc_frequency => GcFrequency}, to_ms(GcFrequency)}.

-spec handle_call(term(), {pid(), term()}, state()) ->
  {reply, ok, state(), timeout()}.
handle_call(force_clean, _From, #{gc_frequency := GcFrequency} = State) ->
  ok = spellingci_sessions_repo:clean_sessions(),
  {reply, ok, State, to_ms(GcFrequency)};
handle_call(_Request, _From, #{gc_frequency := GcFrequency} = State) ->
  {reply, ok, State, to_ms(GcFrequency)}.

-spec handle_cast(term(), state()) -> {noreply, state(), timeout()}.
handle_cast({change_frequency, NewGcFrequency}, State) ->
  {noreply, State#{gc_frequency => NewGcFrequency}, to_ms(NewGcFrequency)};
handle_cast(_Request, #{gc_frequency := GcFrequency} = State) ->
  {noreply, State, to_ms(GcFrequency)}.

-spec handle_info(timeout() | term(), state()) -> {noreply, state(), timeout()}.
handle_info(timeout, #{gc_frequency := GcFrequency} = State) ->
  ok = spellingci_sessions_repo:clean_sessions(),
  {noreply, State, to_ms(GcFrequency)};
handle_info(_Info, #{gc_frequency := GcFrequency} = State) ->
  {noreply, State, to_ms(GcFrequency)}.

-spec terminate( (normal | shutdown | {shutdown, term()} | term())
               , state()
               ) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change( term() | {down, term()}
                 , state()
                 , term()
                 ) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default_frequency() -> frequency().
default_frequency() -> 86400.

-spec to_ms(frequency()) -> frequency().
to_ms(Frequency) -> Frequency * 1000.
