-module(hrl_plugin_prv).
-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, echo).
-define(DEPS, [{default, compile}]).

-define(OUTDIR,"/tmp").
-define(REPODIR,"src/repo/").
-define(INCLODEODIR,"src/include/").
-define(TABLELISTS,[
  repo_mcht_txn_log_pt
  , repo_up_txn_log_pt
  ,repo_mchants_pt
  ,repo_ums_reconcile_result_pt
  ,repo_mcht_txn_acc_pt
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
  erlydtl:compile_file("priv/templates/repo_hrl.dtl",repo_hrl_dtl),
  true = code:add_path(?OUTDIR),
  Result = create_new_repo_record(?TABLELISTS,[]),
%%  file:write_file("src/include/test.hrl",Result),
  ok = file:write_file( ?INCLODEODIR ++ "/store_new.hrl",Result),
  true = code:del_path(?OUTDIR),
  rebar_api:info("Running hrl_plugin...end", []),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

create_new_repo_record([], Acc) ->
  Acc;
create_new_repo_record([Table|RestTable], Acc) ->
  List = get_record(Table),
  create_new_repo_record(RestTable,[List|Acc]).

get_record(M)->
  Options = [
    debug_info
    ,{parse_transform, exprecs}
    ,{outdir,?OUTDIR}
  ],
  {ok,_} = compile:file(?REPODIR ++ atom_to_list(M) ,Options),
  [TableName] = M: '#exported_records-'(),
  Fields = M: '#info-'(TableName, fields),
  Fields2 = lists:map(fun(X)-> atom_to_binary(X,utf8)  end,Fields),
  Option = [{tableName,atom_to_binary(TableName,utf8)},{fields,Fields2}],
  {ok,Result} = repo_hrl_dtl:render(Option),
  Result.
