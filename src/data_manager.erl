-module( data_manager ).
-include( "include/records.hrl" ).

-export( [recv/3] ).

recv( Socket, Data, Players ) ->
	DecodeData = players_manager:get_decode( Data ),
	case players_manager:find_player( Socket, Players ) of
		false -> NewPlayers = Players;
		Player ->
		
			case DecodeData of
			
				[<<"authorize">>, AuthKey, ID, Fio, Sex, Photo, Link] ->
					%% error_logger:error_msg("Rec Data authorize\n", [] ),
					%% io:format("Try Authorize ~p ~p ~n",[ID, AuthKey]),				
					case players_manager:check_user( ID, AuthKey, Players ) of
						true -> 
							NewPlayer = Player#player{ room = 0, id = ID, fio = Fio, sex = Sex, 
														photo = Photo, link = Link },
							NewPlayers = [NewPlayer | players_manager:delete_player( Socket, Players ) ];
							%% io:format("Authorize~p~n",[NewPlayer#player.id]);
						false ->
							players_manager:send_socket_data( Socket, "{\"error\":\"not authorize\"}\0" ),
							NewPlayers = Players, 
							tcp_client:stop( Socket )
							%% Socket ! close
							%% gen_tcp:close( Socket )
							%% io:format("Bad Authorize~n",[])
					end;
				
				_DecodeData ->
					
					case Player#player.room == -1 of
						true ->
							players_manager:send_socket_data( Socket, "{\"error\":\"not authorize\"}\0" ),
							NewPlayers = players_manager:delete_player( Socket, Players ),
							tcp_client:stop( Socket );
							%%Socket ! close;
							%% gen_tcp:close( Socket );
							%% io:format("Bad Authorize~n",[]);
							
						false ->		
							
							case DecodeData of
								
								[<<"changeroom">>, Room] ->
									io:format("Change room 0!~n",[]),
									%% error_logger:error_msg("Rec Data changeromm\n", [] ),								
									%% io:format("Change room ~p to ~p ~n",[Player#player.room, NewPlayer#player.room]),
									NewPlayer = Player#player{ room = list_to_integer(binary_to_list(Room)) },
									NewPlayers = [NewPlayer | players_manager:delete_player( Socket, Players ) ],
									io:format("Change room 1!~n",[]),
									if
										Player#player.room /= 0 andalso Player#player.room /= NewPlayer#player.room ->
											players_manager:send_usersinfo( Player, NewPlayers );
										true -> ok
									end,
									io:format("Change room 2!~n",[]),
									if
										NewPlayer#player.room /= 0 andalso Player#player.room /= NewPlayer#player.room ->
											players_manager:send_usersinfo( NewPlayer, NewPlayers );
										true -> ok
									end;
								
								[Type, Message] when Type == <<"chat">> orelse Type == <<"draw">>  ->
									players_manager:send_message( Type, Players, Player, Message ),
									NewPlayers = Players;					

								[Type, ID, Message] when Type == <<"privatchat">> orelse Type == <<"privatdraw">>  ->
									players_manager:send_privat_message( Type, Players, Player, ID, Message ),
									NewPlayers = Players;
								
								[<<"ping">>] -> 
									NewPlayers = Players;
											
								_Error -> 
									players_manager:send_socket_data( Socket, "{\"error\":\"wrong format\"}\0" ),
									NewPlayers = Players,
									tcp_client:stop( Socket )
									%% Socket ! close
									%% gen_tcp:close( Socket )
							end
						
					end
			end
				
	end, NewPlayers.





