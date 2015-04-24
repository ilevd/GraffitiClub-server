%%%-------------------------------------------------------------------
%%% Created : 4 ќкт€брь 2011 г.
%%%-------------------------------------------------------------------
-module( main ).

-behaviour(application).

-define( LOG_PATH, "/var/web/www/graffiti_log.txt" ).

-export([
	 start/2,
	 stop/1
]).



%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%--------------------------------------------------------------------
start( _Type, _StartArgs) ->
    %case 'TopSupervisor':start_link(StartArgs) of
	%{ok, Pid} ->
	%    {ok, Pid};
	%Error ->
	%    Error
    %end.
	% tcp_server:start(),
	% user_manager:start().
	error_logger:logfile( {open, ?LOG_PATH } ),
	supervisor:start_link( {local, main_sup}, main_sup, []).


%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

