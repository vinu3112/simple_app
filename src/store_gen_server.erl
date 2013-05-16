%% @author Vinod Talapa on 13-05-2013
%% @doc It is a generic server call back module which maintains list of registered user and
%% also calls event server to handle/record deletion and registration of users. Here 
%%  is an <a href="http://www.erlang.org/doc/man/lists.html">erlang-lists</a>.

-module(store_gen_server).
-behaviour(gen_server).
-include("../include/macros.hrl").
-export([init/1,handle_call/3,handle_cast/2,
		 handle_info/2,code_change/3,terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/0,add_user/2,remove_user/1,
		 find_user/1,show_all/0]).

%% start_link/0
%% ====================================================================
%% @doc Creates a gen_server process as part of a supervision tree for handling lists of users.
%% The function should be called, directly or indirectly, by the supervisor. It will,
%% among other things, ensure that the gen_server is linked to the supervisor.
%% It also registers the process with the local name <b>assoc_server</b>.
%% It returns {ok, Pid} if the process is created successfully, otherwise returns {error, Reason}
%% if the process is already created or if there is error in creating the process.

-spec start_link() -> Result when
		  Result :: {ok, pid()}
			  | ignore
			  | {error, Reason},
Reason :: {already_started, pid()}
| shutdown
| term().
%% ====================================================================

start_link()->
Ret = gen_server:start_link({local,assoc_server},?MODULE,[],[{timeout, 20}]),
Ret.

%% add_user/2
%% ===================================================================
%% @doc Call server to register user

-spec add_user(Name::term(),Id :: term()) -> Result when
Result :: term(). 
%% ===================================================================
add_user(Name,Id)->
gen_server:call(assoc_server,{add_user,Name,Id}).


%% remvoe_user/1
%% ===================================================================
%% @doc Call server to remove user from list 

-spec remove_user(Name::term()) -> Result when
Result :: term().
%% ===================================================================

remove_user(Name)->
gen_server:call(assoc_server,{Name,remove}).


%% find_user/1
%% ===================================================================
%% @doc call server to find user and give details of it.
-spec find_user(Name::term()) -> Result when
Result :: term().
%% ===================================================================
find_user(Name)->
gen_server:call(assoc_server,{Name,find}).


%% show_all/0
%% ===================================================================
%% @doc Call server to register User 

-spec show_all() -> Result when
Result :: list().
%% ===================================================================		
show_all()->
gen_server:cast(assoc_server,show_all).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
-spec init([]) -> Result when
		  Result :: {ok,{Arg1::list(),Arg2::term()}}.
%% ====================================================================		
init([]) ->
	gen_event:start({local,log_handle}),
	gen_event:add_handler(log_handle,?ASSOC_EVENT,[logfile]),
	{ok,{[],0}}.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: list()) -> Result when
		  Result :: {noreply, NewState},
NewState :: list().

%% ====================================================================
handle_cast(show_all,State) ->
	io:format("~p~n",[element(1,State)]),
	{noreply,State}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: {Arg1::list(),Arg2::term()}) -> Result when
		  Result :: {reply, Reply, NewState},
Reply :: term(),
NewState :: {Arg1::list(),Arg2::term()}.
%% ====================================================================

handle_call({add_user,Name,Id}, _From, State)->
	New_number=element(2,State)+1,
	User = #user_detail{name=Name, id=Id},
	gen_event:notify(log_handle,{add_user,Name,Id}),		        
	{reply,registered,{[User|element(1,State)],New_number}};

handle_call({Name,remove}, _From,{List,Number}) ->
	User = lists:keyfind(Name,2,List),
	New_List =lists:delete(User,List),
	NewState={New_List,Number-1},
	gen_event:notify(log_handle,{remove_user,Name}),
	{reply, User, NewState};

handle_call({Name,find},_From,State) ->
	User = lists:keyfind(Name,2,element(1,State)),
	{reply, User, State};

handle_call(terminate,_From,State) ->
	Ret=gen_event:stop(log_handle),
	{reply,Ret,State};
	

handle_call(_Blah,_From,State) ->
	{stop,om,State}.	

%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: {List::list(),Count::term()}) -> Result when
		  Result :: {noreply, NewState},
NewState :: term().

%% ====================================================================
handle_info(_Info, State) ->
{noreply, State}.

%% terminate/2
%% ====================================================================
% %% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(normal, State :: {Arg1::list(),Arg2::term()}) -> Any :: term().
%% ====================================================================
terminate(shutdown, _State) ->
	gen_server_terminate;

terminate(_Reason, _State) ->
	ok.

%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
Result :: {ok, NewState :: term()} | {error, Reason :: term()},
OldVsn :: Vsn | {down, Vsn},
Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
%% No change planned. The function is there for the behaviour
{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================