%% @author Vinod Talapa on 13-05-2013
%% @doc Supervises <i>store_gen_server</i>.
-module(store_supervisor).
-behaviour(supervisor).
-include("../include/macros.hrl").
-export([init/1]).
 
%% ====================================================================
%% API functions
%% ====================================================================
 -export([start_link/0,stop/0]).

%% start_link/0
%% ====================================================================
%% @doc Creates a supervisor process as part of a supervision tree. The function will,
%% among other things, ensure that the supervisor is linked to the calling process (its supervisor).
-spec start_link() -> Result when
		  Result :: {ok, pid()}
				  | ignore
                  | {error, Reason},
		  Reason :: {already_started, pid()}
				  | shutdown
				  | term().
%% ====================================================================

start_link() ->
	Ret = supervisor:start_link({local,?MODULE}, ?MODULE, {one_for_one,3,60}),
	Ret.


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
%% one_for_one - If a child process terminates, all other child processes are terminated and
%% then all child processes, including the terminated one, are restarted.

-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================

init({RestartStrategy, MaxRestart, MaxTime}) ->
process_flag(trap_exit,true),
Store_gen_server = {?ASSOC_SERVER,{?ASSOC_SERVER,start_link,[]},permanent, 5000, worker, [?ASSOC_SERVER]},					 
{ok, {{RestartStrategy, MaxRestart, MaxTime},
[Store_gen_server]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================



%% stop_user/0
%% ===================================================================
%% @doc stop server 

% -spec stop() -> Result when
% 			Result :: lists().
%% ===================================================================
stop()->
		Ret=gen_server:call(assoc_server,terminate),
		io:format("~p~n",[Ret]),
		supervisor:terminate_child(?MODULE,?ASSOC_SERVER),
		% store_gen_event:terminate(normalhai,[]),
	    exit(shutdown).
