-module( tcp_client ).
-behaviour( gen_server ).

-export( [init/1, terminate/2, code_change/3, handle_info/2, handle_cast/2, handle_call/3] ).
-export( [start_link/1, stop/1, send/2, set_socket/2] ).

-define( TIMEOUT, 20000 ).
-define( TCP_OPTIONS, [{active, once}, {packet, 0}, {nodelay, true}, binary] ).

-record( state, {socket, client_pid} ).

start_link( Socket ) ->
	% io:format("TcpClient start link socket:~p~n",[Socket]),
	{ok, Pid} = gen_server:start_link(?MODULE, [Socket], []),
	gen_tcp:controlling_process( Socket, Pid ),
	% inet:setopts( Socket, [ {packet, 0}, {packet_size, 100}, list] ),
	% inet:setopts( Socket, [{active, once}, {packet, 0}, list] ),
	{ok, Pid}.

stop( Pid ) -> gen_server:cast( Pid, stop ).

send( Pid, Data ) ->
	% io:format("TcpClient want send! Pid: ~p, data:~p~n",[Pid, Data]),
	gen_server:cast( Pid, {send, Data} ).
	%%gen_server:call( Pid, {send, <<"message is bad O_O">>} ).

set_socket( SocketPid, ClientPid ) ->
	gen_server:call( SocketPid, {set_socket, ClientPid}).
	



init( [Socket] ) ->
	process_flag( trap_exit, true ),
	{ok, #state{socket = Socket}, ?TIMEOUT}.



handle_info( timeout, State ) ->
	% error_logger:error_msg( "TcpClient: TimeOut!~n" ),
	command_controller:command( socket_close, self(), State#state.client_pid ),
	{stop, normal, State};

handle_info( {tcp, Socket, Data}, State) ->
	inet:setopts( Socket, ?TCP_OPTIONS),
	%% ok = gen_tcp:send( Socket, Data ),
	% error_logger:info_msg("TcpClient: Data: ~p~n", [Data]),
	% io:format("Tcpclient new data: ~p, socket:~p~n",[Data, Socket]),

	DecodeData = utils:decode( Data ),
	% error_logger:info_msg("TcpClient: JSON: ~p~n", [DecodeData]),
	% io:format("TcpClient JSON:~p~n", [DecodeData]) ,
	command_controller:command( DecodeData, self(), State#state.client_pid ),
	% send( self(), Data),
	%% client_manager:data( self(), Data ),
	%%io:format("New data in client socket2: ~p~n",[Data]),
	{noreply, State, ?TIMEOUT};

handle_info( {tcp_closed, _Socket}, State ) ->
	% client_manager:disconnect( self() ),
	% io:format("TcpClient SOCKET CLOSE!!!!!!~n", []),
	command_controller:command( socket_close, self(), State#state.client_pid ),
	{stop, normal, State};

handle_info( {tcp_error, _Socket, Reason}, State ) ->
	% client_manager:disconnect( self() ),
	error_logger:error_msg( "TcpClient: tcp_error: ~p~n", [Reason]),
	command_controller:command( socket_close, self(), State#state.client_pid ),
	{stop, normal, State};

handle_info( _Req, State ) ->
	error_logger:error_msg( "TcpClient: unknown handle_info req: ~p~n", [_Req]),
	{noreply, State}.


handle_call( {set_socket, ClientPid}, _From, #state{ socket = Socket } ) ->
	inet:setopts( Socket, ?TCP_OPTIONS ),
	{reply, ok, #state{ socket = Socket, client_pid = ClientPid}};

handle_call( _Req, _From, State ) ->
	error_logger:error_msg( "TcpClient: unknown handle_call req: ~p~n", [_Req]),
	{noreply, State}.



handle_cast( stop, State ) ->
	{stop, normal, State };

handle_cast( {send, Data}, State ) ->
	gen_tcp:send( State#state.socket, list_to_binary([Data, "\0"]) ),
	% gen_tcp:send( State#state.socket, Data ),
	% io:format("TcpClient send data: ~p~n", [Data]),
	{noreply, State, ?TIMEOUT};

handle_cast( _Req, State ) ->
	error_logger:error_msg( "TcpClient: unknown handle_cast req: ~p~n", [_Req]),
	{noreply, State}.



terminate( _Reason, State ) ->
	gen_tcp:close( State#state.socket ),
	ok.

code_change( _OldVsn, State, _Extra ) ->
	{ok, State}.

