-module( utils ).

%%-include_lib("eunit/include/eunit.hrl").


-include( "records.hrl").
% -record( info, {client_pid, id}).



-export( [decode/1, check_user/3, get_dir/1, file/0]).

%%length_test() -> 3 = length([1,2,3]), ok.

%%------------------ decode json------------------------
decode( Data ) ->
	try ejson:decode( Data ) of
		DecodeData ->
			% error_logger:info_msg("Utils decode: ~p~n", [DecodeData]),
			% io:format("Utils decode: ~p~n", [DecodeData]) ,
			bin_to_atom( DecodeData )
	catch
		Er1:Er2 ->
			error_logger:info_msg("Utils decode: ~p, ~p, ~p~n", [Data, Er1, Er2]),
			% io:format("Utils decode: ~p, ~p~n", [Er1, Er2]),
			error
	end.

bin_to_atom( [H|T]) ->	[ convert(H) | bin_to_atom (T)];
bin_to_atom( [] ) -> [];
bin_to_atom( Some) ->
	error_logger:info_msg("Utils bin_to_atom: ~p~n", [Some]),
	Some.

convert(Bin) ->
	try binary_to_list( Bin ) of
		List ->
			try list_to_integer( List ) of
				Int -> Int
			catch
				_:_ ->
					try list_to_atom( List ) of
						Atom -> Atom
					catch
						_:_ -> List
					end
			end
		catch
			_:_ -> Bin
	end.
	% List = binary_to_list( Bin ),






%%-------------- auth user --------------------------------
check_user( UserID, AuthKey, Players ) ->
	case lists:keysearch( UserID, #user_manager_player.id, Players ) of
		{value, _Player} -> false;
		false ->
			true
			%% ApiID = "46529",
			%% SecretKey = "XKEBwvKSiOmeCHZRSOMc",
			%% md5_hex( iolist_to_binary(ApiID ++ "_" ++ binary_to_list(UserID) ++ "_" ++ SecretKey ) ) == binary_to_list( AuthKey )
	end.

md5_hex( Bin ) ->
		binary_to_hex( erlang:md5( Bin ) ).

binary_to_hex( Bin ) ->
	lists:foldl( fun( E, Acc ) ->
		[hex(E bsr 4) | [hex( E band 16#F) | Acc]] end, [], lists:reverse( binary_to_list( Bin ) )).

hex( V ) ->
		if
			V < 10 -> $0 + V;
			true -> $a + (V - 10)
		end.




get_dir( left ) -> {0, -1};
get_dir( right ) -> {0, 1};
get_dir( top ) -> {1, 0};
get_dir( bottom ) -> {-1, 0}.


-define( FILE_PATH, "E:\\oval.jpg" ).

file() ->
	Ans = file:read_file( ?FILE_PATH ),
	case Ans of
		{ok, Bin} -> binary_to_hex( Bin );
		_ -> error
	end.

