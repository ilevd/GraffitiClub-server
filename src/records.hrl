-define( ERROR_AUTH, "{\"error\":\"auth error\"}").
-define( ERROR_USER_OFFLINE, "{\"error\":\"user offline\"}" ).


-record( player, {
	% app
	socket_pid,
	client_pid,

	% social
	id,
	fio,
	city,
	sex,
	photo,
	link,

	% room
	room_pid,
	room_id
}).


-record( user_manager_player, {
	client_pid,		% Пид пользовательского процесса
	id,				% Ид игрока для аутентификации
	room_id
}).
