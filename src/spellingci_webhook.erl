-module(spellingci_webhook).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

-behaviour(egithub_webhook).

-export([handle_pull_request/3]).

% types
-type file_info() :: #{ content   := binary()
                      , path      := string()
                      , commit_id := string()
                      , patch     := binary()
                      }.
-type words_per_line() :: {non_neg_integer(), [string()]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_pull_request( egithub:credentials()
                         , egithub_webhook:req_data()
                         , [egithub_webhook:file()]
                         ) ->
  {ok, [egithub_webhook:message()]} | {error, term()}.
handle_pull_request(Cred, Data, GithubFiles) ->
  #{<<"repository">> := GithubRepo} = Data,
  Repo = spellingci_repos:from_github(GithubRepo),
  RepoFullName = binary_to_list(spellingci_repos:full_name(Repo)),
  #{<<"pull_request">> := #{<<"head">> := #{<<"ref">> := BranchName}}} = Data,
  Branch = binary_to_list(BranchName),
  Config = get_config(Cred, RepoFullName, Branch),
  GithubFiles2 = filter_files(GithubFiles, Config),
  FileInfoFun = fun (File) -> file_info(Cred, RepoFullName, File) end,
  FilesInfo = lists:map(FileInfoFun, GithubFiles2),
  Result = check_files(FilesInfo, [], Config),
  {ok, Result}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec check_files( [file_info()]
                 , [egithub_webhook:message()]
                 , spellingci_config:config()
                 ) -> [egithub_webhook:message()].
check_files([], Comments, _Config) ->
  lists:flatten(Comments);
check_files([#{ content := Content
              , path := Path
              , commit_id := Commit
              , patch := Patch
              } | Rest], Comments, Config) ->
  SheldonConfig = spellingci_config:sheldon_config(Config),
  Comments2 = case sheldon:check(Content, SheldonConfig) of
    ok ->
      Comments;
    #{misspelled_words := MisspelledWords} ->
      C = create_comments(Commit, Path, MisspelledWords, Patch),
      [C | Comments]
  end,
  check_files(Rest, Comments2, Config).

-spec create_comments( string()
                     , string()
                     , [sheldon_result:misspelled_word()]
                     , binary()) -> list().
create_comments(Commit, Path, MisspelledWords, Patch) ->
  WordsPerLine = get_words_per_line(MisspelledWords),
  [create_comment(Commit, Path, Line, create_text(Words), Patch)
    || {Line, Words} <- WordsPerLine].

-spec create_comment(string(), string(), pos_integer(), binary(), binary()) ->
  list().
create_comment(Commit, Path, Line, Text, Patch) ->
  case relative_position(Patch, Line) of
    {ok, Position} ->
      [#{ commit_id => Commit
        , path      => Path
        , position  => Position
        , text      => Text
        }];
    not_found ->
      _ = lager:info("Line ~p does not belong to file's diff.", [Line]),
      []
  end.

-spec get_words_per_line([sheldon_result:misspelled_word()]) ->
  [words_per_line()].
get_words_per_line(MisspelledWords) ->
  LinesWords = [{L, W} || #{word := W, line_number := L} <- MisspelledWords],
  {Lines, _Words} = lists:unzip(LinesWords),
  UniqueLines = sets:to_list(sets:from_list(Lines)),
  F = fun(L, LW) -> [W || {L1, W} <- LW, L == L1] end,
  [{L1, F(L1, LinesWords)} || L1 <- UniqueLines].

-spec create_text([sheldon_result:misspelled_word()]) -> binary().
create_text(MisspelledWords) ->
  Word = case length(MisspelledWords) of
    1 -> "word";
    _ -> "words"
  end,
  create_text(MisspelledWords, [ "I found "
                               , integer_to_list(length(MisspelledWords))
                               , " misspelled "
                               , Word
                               , " in this sentence: \n"
                               ]).

-spec create_text([sheldon_result:misspelled_word()], iodata()) -> binary().
create_text([], Text) ->
  list_to_binary(Text);
create_text([MisspelledWord | Rest], Text) ->
  Word = ["- ", MisspelledWord, "\n"],
  create_text(Rest, Text ++ Word).

-spec filter_files([egithub_webhook:file()], spellingci_config:config()) ->
  [egithub_webhook:file()].
filter_files(GithubFiles, Config) ->
  Extensions = spellingci_config:extensions(Config),
  filter_files(GithubFiles, Extensions, []).

filter_files([], _Extensions, GithubFiles) ->
  GithubFiles;
filter_files( [#{<<"filename">> := FileName} = GithubFile | Rest]
            , Extensions
            , GithubFiles) ->
  case check_extension(FileName, Extensions) of
    true  -> filter_files(Rest, Extensions, [GithubFile | GithubFiles]);
    false -> filter_files(Rest, Extensions, GithubFiles)
  end.

-spec file_info(egithub:credentials(), string(), egithub_webhook:file()) ->
  file_info().
file_info( Cred, Repo, #{ <<"filename">> := Filename
                        , <<"raw_url">>  := RawUrl
                        , <<"patch">>    := Patch
                        }) ->
  CommitId = get_commit_id(RawUrl, Filename),
  {ok, Content} = egithub:file_content(Cred, Repo, CommitId, Filename),
  #{ path => Filename
   , content => Content
   , commit_id => CommitId
   , patch => Patch
   }.

-spec check_extension(binary(), [string()]) -> boolean().
check_extension(_FileName, []) ->
  false;
check_extension(FileName, [Extension | Rest]) ->
  Result = re:split(FileName, "[.]"),
  case lists:nth(length(Result), Result) of
    Extension -> true;
    _         -> check_extension(FileName, Rest)
  end.

-spec get_config(egithub:credentials(), string(), string()) ->
  spellinci_config:config().
get_config(Cred, Repo, Branch) ->
  case egithub:file_content(Cred, Repo, Branch, "spellingci.json") of
    {ok, ConfigContent} ->
      ct:print("~p~n", [ConfigContent]),
      Config = jiffy:decode(ConfigContent, [return_maps]),
      spellingci_config:normalize(Config);
    {error, _} ->
      spellingci_config:default()
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Github functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @todo delete and refactor after inaka/erlang-github#122
-spec get_commit_id(string(), string()) -> string().
get_commit_id(Url, Filename) ->
  UrlString = unicode:characters_to_list(Url),
  Regex = ".+/raw/(.+)/" ++ Filename,
  {match, [_, {Pos, Len} | _]} = re:run(UrlString, Regex),
  string:substr(UrlString, Pos + 1, Len).

%% @todo delete and refactor after inaka/erlang-github#122
-spec relative_position(binary(), integer()) ->
  {ok, integer()} | not_found.
relative_position(Patch, LineNum) ->
  Lines = binary:split(Patch, <<"\n">>, [global]),
  relative_position(Lines, LineNum, {-1, undefined}).

%% @todo delete and refactor after inaka/erlang-github#122
-spec relative_position(list(), integer(), term()) ->
  not_found | {ok, pos_integer()}.
relative_position([], _Num, _Positions) ->
  not_found;
relative_position([Line | Lines], Num, Positions) ->
  Type = patch_line_type(Line),
  case new_position(Line, Positions) of
    {NewLocal, NewGlobal} when
    NewGlobal == Num,
    Type =/= patch,
    Type =/= deletion ->
      {ok, NewLocal};
    NewPositions ->
      relative_position(Lines, Num, NewPositions)
  end.

%% @todo delete and refactor after inaka/erlang-github#122
-spec new_position(binary(), term()) -> {integer(), integer()}.
new_position(Line, {Local, Global}) ->
  case patch_line_type(Line) of
      patch ->
          NewGlobal = patch_position(Line),
          {Local + 1, NewGlobal - 1};
      deletion ->
          {Local + 1, Global};
      _ -> %% addition or same
          {Local + 1, Global + 1}
  end.

%% @todo delete and refactor after inaka/erlang-github#122
-spec patch_line_type(binary()) -> patch | addition | deletion | same.
patch_line_type(Line) ->
  [Head | _] = unicode:characters_to_list(Line),
  case Head of
    $@  -> patch;
    $+  -> addition;
    $-  -> deletion;
    $\\ -> same;
    $   -> same %space
  end.

%% @todo delete and refactor after inaka/erlang-github#122
-spec patch_position(binary()) -> integer().
patch_position(Line) ->
  Regex = "^@@ .*? \\+(\\d+),.*$",
  Opts = [{capture, all_but_first, list}],
  {match, [PositionStr | _ ]} = re:run(Line, Regex, Opts),
  list_to_integer(PositionStr).
