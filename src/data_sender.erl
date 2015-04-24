-module( data_sender ).
-export( [send/2, send/3, send/4]).

-include( "records.hrl").


send( Type, Message, RoomPlayers, SendPlayer ) when Type == chat orelse Type == draw ->
	Info = SendPlayer,
	Data = {[
				{<<"id">>, Info#player.id},
				{<<"fio">>, Info#player.fio},
				{<<"photo">>, Info#player.photo},
				{<<"sex">>, Info#player.sex},
				{<<"link">>, Info#player.link},
				{<<"message">>, Message}
		   ]},
   	SendData = {[{  list_to_binary (atom_to_list( Type )), Data}]},
	EncodeData = ejson:encode( SendData ),
	send_data( RoomPlayers, EncodeData);


send( Type, Message, RecvPlayer, SendPlayer ) when Type == privatchat orelse Type == privatdraw ->
	Info = SendPlayer,
	Data = {[
				{<<"id">>, Info#player.id},
				{<<"fio">>, Info#player.fio},
				{<<"photo">>, Info#player.photo},
				{<<"sex">>, Info#player.sex},
				{<<"link">>, Info#player.link},
				{<<"message">>, Message}
		   ]},
	SendData = {[{ list_to_binary (atom_to_list( Type )), Data}]},
	EncodeData = ejson:encode( SendData ),
	player:send( RecvPlayer#player.client_pid, EncodeData).



send( users_info, RoomPlayers ) ->
	F = fun( P ) ->
			Info = player:get_user( P#user_manager_player.client_pid ),
			{[
				{<<"id">>, Info#player.id},
				{<<"fio">>, Info#player.fio},
				{<<"photo">>, Info#player.photo},
				{<<"sex">>, Info#player.sex},
				{<<"link">>, Info#player.link}
		   ]}
	end,
	Data = lists:map( F, RoomPlayers ),
	SendData = {[{<<"usersInfo">>, Data}]},
	EncodeData = ejson:encode( SendData ),
	send_data( RoomPlayers, EncodeData).



send( users_info, RoomPlayers, ClientPid ) ->
	F = fun( P ) ->
			Info = player:get_user( P#user_manager_player.client_pid ),
			{[
				{<<"id">>, Info#player.id},
				{<<"fio">>, Info#player.fio},
				{<<"photo">>, Info#player.photo},
				{<<"sex">>, Info#player.sex},
				{<<"link">>, Info#player.link}
		   ]}
	end,
	Data = lists:map( F, RoomPlayers ),
	SendData = {[{<<"usersInfo">>, Data}]},
	EncodeData = ejson:encode( SendData ),
	player:send( ClientPid, EncodeData ).


send_data( Players, SendData ) ->
	lists:foreach( fun( Player ) -> player:send( Player#user_manager_player.client_pid, SendData ) end, Players ).

%send_data( Players, SendData ) ->
%	lists:foreach( fun( Player ) -> tcp_client:send( Player#player.socket_pid, SendData ) end, Players ).