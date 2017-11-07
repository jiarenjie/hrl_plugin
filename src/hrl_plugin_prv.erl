-module(hrl_plugin_prv).
-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, echo).
-define(DEPS, [{default, compile}]).


-define(DOCROOT, "priv/templates/").
-define(CONFIG, "priv/templates/hrl_plugin.config").
-define(DTLNAME, "_build/default/plugins/hrl_plugin/priv/templates/repo_hrl.dtl").
%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},            % The 'user friendly' name of the task
    {module, ?MODULE},            % The module implementation of the task
    {namespace, hrl_plugin},
    {bare, false},                 % The task can be run by the user, always true
    {deps, ?DEPS},                % The list of dependencies
    {example, "rebar3 hrl_plugin"}, % How to use the plugin
    {opts, []},                   % list of options understood by the plugin
    {short_desc, "A rebar plugin"},
    {desc, "A rebar plugin"}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_api:info("Running hrl_plugin...", []),

  try
    Lists = case filelib:is_regular(?CONFIG) of
              true -> {ok, [PV]} = file:consult(?CONFIG),
                PV;
              _ ->
                [
                  {outDir, "/tmp"}
                  , {repoDir, "src/repo/"}
                  , {hrlFileName, "src/include/store_new.hrl"}
                  , {repoName, [repo_mcht_txn_log_pt
                  , repo_up_txn_log_pt
                  , repo_mchants_pt
                  , repo_ums_reconcile_result_pt
                  , repo_mcht_txn_acc_pt]}
                ]
            end,
    RepoDir = proplists:get_value(includeDir, Lists, "src/repo/"),
    OutDir = proplists:get_value(outDir, Lists, "/tmp"),
    HrlFileName = proplists:get_value(hrlFileName, Lists, "src/include/store_new.hrl"),
    RepoName = proplists:get_value(repoName, Lists, [repo_mcht_txn_log_pt
      , repo_up_txn_log_pt
      , repo_mchants_pt
      , repo_ums_reconcile_result_pt
      , repo_mcht_txn_acc_pt]),
    io:format("RepoDir:~p ~n", [RepoDir]),
    {ok,repo_hrl_dtl} = erlydtl:compile_file(?DTLNAME, repo_hrl_dtl),
    true = code:add_path(OutDir),
    Result = create_new_repo_record(RepoName, [RepoDir, OutDir], []),
%%  file:write_file("src/include/test.hrl",Result),
    ok = file:write_file(HrlFileName, Result),
    true = code:del_path(OutDir),
    rebar_api:info("Running hrl_plugin...end", [])
  catch
      _:_  ->
        rebar_api:error("hrl_plugin creat ~p error ...", [HrlFileName])
  end ,

  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

create_new_repo_record([], _Option, Acc) ->
  Acc;
create_new_repo_record([Table | RestTable], Option, Acc) ->
  List = get_record(Table, Option),
  create_new_repo_record(RestTable, Option, [List | Acc]).

get_record(M, [RepoDir, OutDir]) ->
  Options = [
    debug_info
    , {parse_transform, exprecs}
    , {outdir, OutDir}
  ],
  io:format("FileName:~p,filexit:~p ~n", [RepoDir ++ atom_to_list(M), filelib:is_regular(RepoDir ++ atom_to_list(M))]),
  {ok, _} = compile:file(RepoDir ++ atom_to_list(M), Options),
  [TableName] = M: '#exported_records-'(),
  Fields = M: '#info-'(TableName, fields),
  Fields2 = lists:map(fun(X) -> atom_to_binary(X, utf8) end, Fields),
  Option = [{tableName, atom_to_binary(TableName, utf8)}, {fields, Fields2}],
  {ok, Result} = repo_hrl_dtl:render(Option),
  Result.


