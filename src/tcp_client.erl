-module( tcp_client ).
-behaviour( gen_server ).

-export( [init/1, terminate/2, code_change/3, handle_info/2, handle_cast/2, handle_call/3] ).
-export( [start_link/1, stop/1, send/2, set_socket/2] ).

-define( TIMEOUT, 20000 ).
-define( TCP_OPTIONS, [{active, once}, {packet, 0}, {nodelay, true}, binary] ).

-record( state, {socket, client_pid} ).

start_link( Socket ) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [Socket], []),
	gen_tcp:controlling_process( Socket, Pid ),
	{ok, Pid}.

stop( Pid ) -> gen_server:cast( Pid, stop ).

send( Pid, Data ) ->
	gen_server:cast( Pid, {send, Data} ).

set_socket( SocketPid, ClientPid ) ->
	gen_server:call( SocketPid, {set_socket, ClientPid}).


init( [Socket] ) ->
	process_flag( trap_exit, true ),
	{ok, #state{socket = Socket}, ?TIMEOUT}.


handle_info( timeout, State ) ->
	command_controller:command( socket_close, self(), State#state.client_pid ),
	{stop, normal, State};

handle_info( {tcp, Socket, Data}, State) ->
	inet:setopts( Socket, ?TCP_OPTIONS),
	DecodeData = utils:decode( Data ),
	command_controller:command( DecodeData, self(), State#state.client_pid ),
	{noreply, State, ?TIMEOUT};

handle_info( {tcp_closed, _Socket}, State ) ->
	command_controller:command( socket_close, self(), State#state.client_pid ),
	{stop, normal, State};

handle_info( {tcp_error, _Socket, Reason}, State ) ->
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
	{noreply, State, ?TIMEOUT};

handle_cast( _Req, State ) ->
	error_logger:error_msg( "TcpClient: unknown handle_cast req: ~p~n", [_Req]),
	{noreply, State}.


terminate( _Reason, State ) ->
	gen_tcp:close( State#state.socket ),
	ok.

code_change( _OldVsn, State, _Extra ) ->
	{ok, State}.

