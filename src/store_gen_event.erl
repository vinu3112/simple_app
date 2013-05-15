%% @author Vinod Talapa on 13-05-2013
%% @doc It is a generic event call back module which handles log file for application

-module(store_gen_event).
-behaviour(gen_event).
-include("../include/macros.hrl").
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc Open logfile to store log for user 

-spec init(Name :: term()) -> Result when
          Result :: {ok,Args :: term()}.
%% ====================================================================     

init(Name) ->
    {ok,FD} = file:open(Name,write),
    {ok, FD}.


%% handle_event/2
%% ====================================================================
%% @doc Add log to the log file
%% <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_event-2">gen_event:handle_event/2</a>
-spec handle_event(Event, State) -> Result when
          Event::term(),
State::term(),
Result::{ok,NewState} | {ok,NewState,hibernate}
| {swap_handler,Args1,NewState,Handler2,Args2} | remove_handler,
NewState::term(),
Args1::term(),
Args2::term(),
Handler2::Module2 | {Module2,Id},
Module2::atom(),
Id::term().
%% ====================================================================     
handle_event({add_user, Name, Email}, Fd) ->
    io:format(Fd, "Added user:~s email: ~s~n", [Name, Email]),
    {ok, Fd};
handle_event({remove_user, Name}, Fd) ->
    io:format(Fd, "Removed user:~s~n", [Name]),
    {ok, Fd}.


%% handle_call/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_call-2">gen_event:handle_call/2</a>

-spec handle_call(Request, State) -> Result when
          Request::term(),
State::term(),
Result::{ok,Reply,NewState} | {ok,Reply,NewState,hibernate}
| {swap_handler,Reply,Args1,NewState,Handler2,Args2}
| {remove_handler, Reply},
Reply::term(),
NewState::term(),
Args1::term(),
Args2::term(),
Handler2::Module2 | {Module2,Id},
Module2::atom(),
Id::term().
%% ====================================================================

handle_call(terminate, Fd) ->
    {stop,shutdown,Fd};

handle_call(_, Fd) ->
    {ok, ok, Fd}.



%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_info-2">gen_event:handle_info/2</a>

%% ====================================================================

handle_info(_, Fd) ->
{ok, Fd}.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:code_change-3">gen_event:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
Result :: {ok, NewState :: term()} | {error, Reason :: term()},
OldVsn :: Vsn | {down, Vsn},
Vsn :: term().
%% ====================================================================
code_change(_OldVsn, Fd, _Extra) ->
{ok, Fd}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:terminate-2">gen_event:terminate/2</a>

%% ====================================================================
terminate(Reason, State) ->
file:close(State),
Reason.
