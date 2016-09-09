-module(spellingci_utils).

-export([ now_datetime/0
        ]).

-type datetime() ::
  { datetime,
    { {integer(), integer(), integer()}
    , {integer(), integer(), integer()}
    }
  }.
-export_type([datetime/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec now_datetime() -> datetime().
now_datetime() ->
  {datetime, calendar:universal_time()}.
