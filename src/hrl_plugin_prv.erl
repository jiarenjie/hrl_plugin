-module(hrl_plugin_prv).
-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, echo).
-define(DEPS, [{default, compile}]).

-define(OUTDIR, "/tmp").
-define(REPODIR, "src/repo/").
-define(DEPREPODIR, "_build/default/lib/pg_store/src/repos").
-define(INCLODEODIR, "src/include/").
-define(TABLELISTS, [
  repo_mcht_txn_log_pt
  , repo_up_txn_log_pt
  , repo_mchants_pt
  , repo_ums_reconcile_result_pt
  , repo_mcht_txn_acc_pt
]).

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
  RepoDir = case filelib:is_dir(?REPODIR) of
              true -> ?REPODIR;
              false -> ?DEPREPODIR
            end,
  io:format("RepoDir:~p ~n",[RepoDir]),
  erlydtl:compile_file("priv/templates/repo_hrl.dtl", repo_hrl_dtl),
  true = code:add_path(?OUTDIR),
  Result = create_new_repo_record(?TABLELISTS,RepoDir, []),
%%  file:write_file("src/include/test.hrl",Result),
  case filelib:is_dir(?INCLODEODIR) of
    true -> ok;
    false -> file:make_dir(?INCLODEODIR)
  end,
  ok = file:write_file(?INCLODEODIR ++ "/store_new.hrl", Result),
  true = code:del_path(?OUTDIR),
  rebar_api:info("Running hrl_plugin...end", []),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

create_new_repo_record([],RepoDir, Acc) ->
  Acc;
create_new_repo_record([Table | RestTable],RepoDir, Acc) ->
  List = get_record(Table,RepoDir),
  create_new_repo_record(RestTable,RepoDir, [List | Acc]).

get_record(M,RepoDir) ->
  Options = [
    debug_info
    , {parse_transform, exprecs}
    , {outdir, ?OUTDIR}
  ],
  io:format("FileName:~p,filexit:~p ~n",[RepoDir ++ atom_to_list(M),filelib:is_regular(RepoDir ++ atom_to_list(M))]),
  {ok, _} = compile:file(RepoDir ++ atom_to_list(M), Options),
  [TableName] = M: '#exported_records-'(),
  Fields = M: '#info-'(TableName, fields),
  Fields2 = lists:map(fun(X) -> atom_to_binary(X, utf8) end, Fields),
  Option = [{tableName, atom_to_binary(TableName, utf8)}, {fields, Fields2}],
  {ok, Result} = repo_hrl_dtl:render(Option),
  Result.
