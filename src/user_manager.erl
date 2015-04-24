-module( user_manager ).
-behaviour( gen_server ).

-include("records.hrl").

-export( [start/0, stop/0, add_user/3, remove_user/1, get_user_by_Pid/1, get_user_by_ID/1, get_users/0, get_room_users/1] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).


start() ->
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).
	
stop() ->
	gen_server:call( ?MODULE, stop ).


add_user( PlayerPid, ID, RoomID ) ->
	gen_server:call( ?MODULE, {add, PlayerPid, ID, RoomID} ).

remove_user( PlayerPid ) ->
	gen_server:call( ?MODULE, {remove, PlayerPid} ).
	
get_user_by_Pid( ClientPid ) ->
	gen_server:call( ?MODULE, {get_user, ClientPid, #user_manager_player.client_pid } ).

get_user_by_ID( ID ) ->
	gen_server:call( ?MODULE, {get_user, ID, #user_manager_player.id }).

get_room_users( RoomID ) ->
	gen_server:call( ?MODULE, {get_room_users, RoomID}).

get_users() ->
	gen_server:call( ?MODULE, {get_users}).

	
init( _Args ) ->
	error_logger:info_msg("UserManager start!", []),
	{ok, []}.

handle_call( {add, ClientPid, ID, RoomID}, _From, Players ) ->
	Player = #user_manager_player{ client_pid = ClientPid, id = ID, room_id = RoomID},
	NewPlayers = [ Player | Players ],
	{reply, ok, NewPlayers};

handle_call( {remove, ClientPid}, _From, Players ) ->
	NewPlayers = lists:keydelete( ClientPid, #user_manager_player.client_pid, Players ),
	{reply, ok, NewPlayers};
	
handle_call( {get_user, ClientPid, RecordPos }, _From, Players ) ->
	Response = case lists:keysearch( ClientPid, RecordPos, Players ) of
		{value, Player} -> Player;
		false -> false
	end,
	{reply, Response, Players};

handle_call( {get_room_users, RoomID}, _From, Players ) ->
	RoomPlayers = lists:filter( fun( P ) -> P#user_manager_player.room_id == RoomID end, Players),
	{reply, RoomPlayers, Players};

handle_call( {get_users}, _From, Players ) ->
	{reply, Players, Players};

handle_call( Req, _From, Players ) ->
	{reply, {unknown_request, Req}, Players}.

	
handle_cast( _Msg, State ) -> {noreply, State}.
handle_info( _Info, State ) -> {noreply, State}.
terminate( _Reason, _State ) -> ok.
code_change( _OldVsn, State, _Extra) -> {ok, State}.	