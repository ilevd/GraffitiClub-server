-module( main_sup ).
-behaviour( supervisor ).
-export( [init/1 ]).

-define( MAX_RESTART, 1 ).
-define( MAX_TIME, 1 ).

init( [] ) ->
	TcpServerSpec = {
		tcp_server,					% id
		{tcp_server, start, []},	% MFA
		permanent,					% restart strategy
		2000,
		worker,						% type
		[tcp_server]				% modules
	},
	UserManagerSpec = {
		user_manager,
		{user_manager, start, []},
		permanent,
		2000,
		worker,
		[user_manager]
	},
	{ok, {{one_for_all, ?MAX_RESTART, ?MAX_TIME}, [UserManagerSpec, TcpServerSpec]}}.

