

clue :- start_dialog.

:- dynamic num_players/1.

start_dialog :-
	new(Dialog, dialog('Startup Menu')),

	% Create slider
	new(Slider, slider(num_players_slider, 2, 6, 3)),
	send(Slider, label('Number of Players: ')),
	send(Dialog, append(Slider)),

	% Create menus
	new(M1, menu(player1, cycle)),
	send_list(M1, append, [peacock, green]),
	send(Dialog, append(M1)),

	% Create okay button
	new(Ok_button, button('OK', message(@prolog, set_num_players, Slider?selection))),
	send(Dialog, append(Ok_button)),

	send(Dialog, open).

set_num_players(N) :- assert(num_players(N)).