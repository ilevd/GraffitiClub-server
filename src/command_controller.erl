-module( command_controller ).

-include( "records.hrl" ).

-export( [command/3]).



command( [authorize, AuthKey, ID, Fio, Sex, Photo, Link], SocketPid, ClientPid ) ->
	case utils:check_user( ID, AuthKey, user_manager:get_users() ) of
		true ->
			NewPlayer = #player{ socket_pid = SocketPid, client_pid = ClientPid, id = ID, fio = Fio, sex = Sex,
										photo = Photo, link = Link, room_id = 0 },
			player:set_user( ClientPid, NewPlayer ),
			user_manager:add_user( ClientPid, ID, 0 );
			% data_sender:send( player_connect, player:get_user( ClientPid ), user_manager:get_wait_players() );
		false ->
			tcp_client:send( SocketPid, ?ERROR_AUTH ),
			player:stop( ClientPid ),
			tcp_client:stop( SocketPid )
	end,
	ok;


%------- Смена комнаты -----------------------------------------------
command( [changeroom, RoomID], SocketPid, ClientPid ) ->
	Player = user_manager:get_user_by_Pid( ClientPid ),
	user_manager:remove_user( ClientPid ),
	case Player#user_manager_player.room_id of
		0 -> ok;
		OldRoomID ->
			RoomPlayers = user_manager:get_room_users( OldRoomID ),
			data_sender:send( users_info, RoomPlayers)
	end,
	user_manager:add_user( ClientPid, Player#user_manager_player.id, RoomID),
	data_sender:send( users_info, user_manager:get_room_users( RoomID )),
	ok;


%------- Сообщения в общую комнату-------------------------------------
command( [Type, Message], SocketPid, ClientPid ) when Type == chat orelse Type == draw ->
	UPlayer = user_manager:get_user_by_Pid( ClientPid ),
	Player = player:get_user( UPlayer#user_manager_player.client_pid ),
	RoomID = UPlayer#user_manager_player.room_id,
	RoomPlayers = user_manager:get_room_users( RoomID ),
	data_sender:send( Type, Message, RoomPlayers, Player ),
	ok;


%------- Сообщения в привату--------------------------------------------
command( [Type, ReceiverID, Message], SocketPid, ClientPid ) when Type == privatchat orelse Type == privatdraw ->
	URecvPlayer = user_manager:get_user_by_ID( ReceiverID ),
	case URecvPlayer of
		false ->
			player:send( ClientPid, ?ERROR_USER_OFFLINE );
		_ ->
			% io:format("URecvPlayer: ~p~n", [URecvPlayer]),
			RecvPlayer = player:get_user( URecvPlayer#user_manager_player.client_pid ),
			% io:format("RecvPlayer: ~p~n", [RecvPlayer]),
			SendPlayer = player:get_user( ClientPid ),
			data_sender:send( Type, Message, RecvPlayer, SendPlayer ),
			ok
	end;


command( [ping], SocketPid, ClientPid) ->
	ok;


command( socket_close, SocketPid, ClientPid ) ->
	UPlayer = user_manager:get_user_by_Pid( ClientPid ),
	case UPlayer of
		false -> ok;
		_ ->
			user_manager:remove_user( ClientPid ),
			RoomID = UPlayer#user_manager_player.room_id,
			case UPlayer#user_manager_player.room_id /= 0 of
				true ->
					RoomPlayers = user_manager:get_room_users( RoomID ),
					data_sender:send( users_info, RoomPlayers);
				false -> ok
			end
	end,
	player:stop( ClientPid ),
	tcp_client:stop( SocketPid),
	ok;


command( Command, SocketPid, ClientPid ) ->
	error_logger:error_msg("CommandController: Unknown command: ~p, SocketPid:~p, ClientPid:~p~n", [Command, SocketPid, ClientPid]),
	ok.