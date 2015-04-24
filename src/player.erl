-module( player ).

-behaviour( gen_server ).

%-include( "include/records.hrl" ).
-include("records.hrl").



-export( [start_link/1, stop/1, get_user/1, set_user/2, send/2] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).


start_link( SocketPid ) ->
	gen_server:start_link( ?MODULE, [SocketPid], [] ).

stop( ClientPid ) ->
	gen_server:cast( ClientPid, stop ).




set_user( ClientPid, Player ) ->
	gen_server:call( ClientPid, {set, Player} ).

get_user( ClientPid ) ->
	gen_server:call( ClientPid, {get} ).


send( ClientPid, Data ) ->
	gen_server:cast( ClientPid, {send, Data} ).

init( [SocketPid] ) -> {ok, #player{ socket_pid = SocketPid }}.



handle_call( {set, NewPlayer}, _From, _Player ) ->
	{reply, ok, NewPlayer};

handle_call( {get}, _From, Player )  ->
	{reply, Player, Player};

handle_call( Req, _From, Players ) ->
	{reply, {unknown_request, Req}, Players}.




handle_cast( {send, Data}, State ) ->
	tcp_client:send( State#player.socket_pid, Data),
	% io:format("Player send data: ~p~n", [Data]),
	{noreply, State};

handle_cast( stop, Player ) ->
%	case is_process_alive( Player#player.socket_pid ) of
%		true ->
%			tcp_client:stop( Player#player.socket_pid );
	%	false -> ok
	%end,
	{stop, normal, Player};

handle_cast( _Msg, State ) -> {noreply, State}.




handle_info( timeout, State ) ->
	% io:format("Player TIMEOUT occured!~n"),
	{noreply, State};


handle_info( _Info, State ) -> {noreply, State}.
terminate( _Reason, _State ) -> ok.
code_change( _OldVsn, State, _Extra) -> {ok, State}.
