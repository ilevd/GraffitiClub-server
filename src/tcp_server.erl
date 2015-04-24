-module( tcp_server ).
-behaviour( gen_server ).

-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
-export( [start/0, stop/0]).

-define( TCP_OPTIONS, [list, {active, false}, {reuseaddr, true}, {packet, 0} ]).
-define( PORT, 4334 ).

-record( state, {listenSocket, acceptor} ).

start() -> gen_server:start_link( {local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call( ?MODULE, stop ).

init( _Args ) ->
	error_logger:info_msg("TcpServer start!", []),
	case gen_tcp:listen( ?PORT, ?TCP_OPTIONS ) of
		{ok, ListenSocket} ->
			{ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
			{ok, #state{listenSocket = ListenSocket, acceptor = Ref} } ;
		Error ->
			{stop, Error}
	end.

handle_info( {inet_async, ListenSocket, Ref, {ok, CliSocket} },
		   #state{ listenSocket = ListenSocket, acceptor = Ref } = State ) ->
	case set_sockopt( ListenSocket, CliSocket ) of
		ok -> ok;
		{error, Reason} ->
			exit( {set_sockopt, Reason} )
	end,

	{ok, SocketPid} = tcp_client:start_link( CliSocket ),
	{ok, PlayerPid} = player:start_link( SocketPid ),
	tcp_client:set_socket( SocketPid, PlayerPid ),

	case prim_inet:async_accept( ListenSocket, -1 ) of
		{ok, NewRef} -> ok;
		{error, NewRef} ->
			exit( {async_accept, inet:format_error(NewRef)})
	end,

	{noreply, State#state{acceptor = NewRef}};

handle_info({inet_async, ListenSocket, Ref, Error}, #state{listenSocket = ListenSocket,
														  acceptor = Ref} = State) ->
   error_logger:error_msg("TcpServer: Error in ListenSocket: ~p~n", [Error]),
   {stop, Error, State};

handle_info( _Req, State ) ->
	error_logger:error_msg( "TcpServer: Listen Unknown handle_info req: ~p~n", [_Req]),
	{noreply, State}.

 
set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
		case prim_inet:setopts(CliSocket, Opts) of
			ok    -> ok;
			Error -> gen_tcp:close(CliSocket), Error
		end;
	Error ->
		gen_tcp:close(CliSocket), Error
	end.


handle_call( Req, _From, State ) ->
	error_logger:error_msg( "TcpServer: Listen Unknown handle_call req: ~p~n", [Req]),
	{stop, {unknown_call, Req}, State}.

handle_cast( Req, State ) ->
	error_logger:error_msg( "TcpServer: Listen Unknown handle_cast req: ~p~n", [Req]),
	{noreply, State}.

terminate( _Reason, State ) ->
	gen_tcp:close( State#state.listenSocket ),
	ok.

code_change( _OldVsn, State, _Extra ) ->
	{ok, State}.