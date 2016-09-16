-module(spellingci_session_gc).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(gen_server).

%% API
-export([ start_link/0
        , clean/0
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
-type state()     :: #{ gc_frequency := frequency()
                      , timer        := timer:tref()
                      }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec clean() -> ok.
clean() ->
  gen_server:call(?MODULE, clean).

-spec force_clean() -> ok.
force_clean() ->
  gen_server:call(?MODULE, force_clean).

-spec change_frequency(frequency()) -> ok.
change_frequency(Frequency) ->
  gen_server:cast(?MODULE, {change_frequency, Frequency}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([frequency()]) -> {ok, state()}.
init([]) ->
  GcFrequency =
    application:get_env(spellingci, sessions_gc_frequency, default_frequency()),
  Timer = create_timer(GcFrequency),
  {ok, #{ gc_frequency => GcFrequency, timer => Timer}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(clean, _From, #{gc_frequency := GcFrequency} = State) ->
  ok = spellingci_sessions_repo:clean_sessions(),
  Timer = create_timer(GcFrequency),
  {reply, ok, State#{timer => Timer}};
handle_call( force_clean
           , _From
           , #{ gc_frequency := GcFrequency, timer := Timer} = State
           ) ->
  {ok, cancel} = timer:cancel(Timer),
  ok = spellingci_sessions_repo:clean_sessions(),
  NewTimer = create_timer(GcFrequency),
  {reply, ok, State#{timer := NewTimer}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({change_frequency, NewGcFrequency}, #{timer := Timer} = State) ->
  {ok, cancel} = timer:cancel(Timer),
  NewTimer = create_timer(NewGcFrequency),
  {noreply, State#{gc_frequency => NewGcFrequency, timer => NewTimer}};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(timeout() | term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

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

-spec create_timer(frequency()) -> timer:tref().
create_timer(Frequency) ->
  {ok, Timer} = timer:apply_after(Frequency, ?MODULE, clean, []),
  Timer.
