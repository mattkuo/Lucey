clue :- get_startup_data, main_menu.

% Dynamic database
:- dynamic num_players/1.    % represents # of human players playing
:- dynamic my_character/1.   % The user's character
:- dynamic player/1.         % players are represented by the character they are playing as
:- dynamic in_hand/1.        % Cards in the user's hand; 

% Assigns a "status" to each card.
% cards_data(Card, Player, Status)
% Card can be any character, weapon or room
% Player is which player the data belongs to
% Status is a number (0, 1, 2, 3):  
%				 0 - Player does not have card in their hand.
%				 1 - Player might have card in their hand.
%				 2 - Player has card in their hand.
%				 3 - This is my card and player has seen it
%				 4 - Player may have seen this card

:- dynamic cards_data/3.

% Cards
character(green).
character(white).
character(peacock).
character(mustard).
character(scarlet).
character(plum).

weapon(rope).
weapon(pipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(revolver).

room(kitchen).
room(ballroom).
room(dining).
room(conservatory).
room(billiard).
room(library).
room(lounge).
room(hall).
room(study).

% Obtain startup data from player
get_startup_data :-
	clear_database,
	input_num_players,
	input_me,
	input_hand.

% Input how many human players are playing the game
input_num_players :-
	write_ln('Input # of players: '),
	read(N),
	nl,
	assert(num_players(N)),
	input_num_players_helper(1, N).

% Asks user to input each player's character
input_num_players_helper(Count, N) :- Count > N.
input_num_players_helper(Count, N) :-
	Count =< N,
	writef("Input player %t's character", [Count]), nl,
	read(Character),
	assert(player(Character)),
	I is Count + 1,
	input_num_players_helper(I, N).

input_me :-
	write_ln('Which character are you playing as? :'),
	read(Character),
	assert(my_character(Character)),
	nl.

% Input cards I have in my hand
input_hand :-
	write_ln('Enter a card in your hand. (When you are done, input end): '),
	read(Card),
	nl,
	(Card = end -> nl ;
	is_valid_card(Card) -> 
		assert(in_hand(Card)), input_hand;
		write_ln('That is an invalid card. Please try again.'), input_hand).

% Displays main menu. All procedures should return here.
main_menu :-
	can_accuse(_,_,_),
	write_ln('#### MAIN MENU ####'),
	write_ln('1) Record my suggestion'),
	write_ln('2) Record opponent suggestion'),
	write_ln('3) View Database'),
	write_ln('4) Exit program'),
	read(Option), nl,
	(
	 	Option = 1 -> record_suggestion;
	 	Option = 2 -> record_opponent_suggestion;
	 	Option = 3 -> view_database;
	 	Option = 4 -> halt;
	 	write_ln('Please choose a valid option.'), main_menu
 	).

% Record player suggestions here
record_suggestion :-
	write_ln('Who did you suspect?'),
	read(Suspect), nl,
	(not(character(Suspect)) -> write_ln('Invalid character'), record_suggestion;
	write_ln('What weapon did the suspect use?')),
	read(Weapon), nl,
	(not(weapon(Weapon)) -> write_ln('Invalid weapon'), record_suggestion;
	write_ln('What room are you in?')),
	read(Room), nl,
	(not(room(Room)) -> write_ln('Invalid room'), record_suggestion;
	write_ln('Was a card shown to you?')),
	read(Shown), nl,
	(
		 Shown = no -> card_not_shown(Suspect), card_not_shown(Weapon), card_not_shown(Room);
		 Shown = yes -> card_shown;
		 write_ln('Please choose a valid option.'), record_suggestion
	),
	main_menu.

% Updates database for the case that no card was shown after self suggestion
% Card_data set to 0 for all opponents 
card_not_shown(Card) :-
	player(Others),
	not(my_character(Others)),
	retractall(cards_data(Card, Others, _)),
	assert(cards_data(Card, Others, 0)).

% Updates what player now knows and the opponent player who showed that card
card_shown :-
	write_ln('Which card was shown?'),
	read(Card), nl,
	(not(is_valid_card(Card)) -> write_ln('Invalid Card.'), card_shown;
	write_ln('Which player showed you the card?')),
	read(Player), nl,
	(
		not(player(Player)) -> write_ln('Invalid player.'), card_shown;
		retract(cards_data(Card, Player, _)), assert(cards_data(Card, Player, 2)), assert(in_hand(Card))
	).

% TODO: Record opponent suggesitons and make inferences using what we know
record_opponent_suggestion :- 
	writeln('Whose turn is it?'),
	read(Player), nl,
	(not(player(Player)) -> write_ln('Invalid player'), record_opponent_suggestion;
	write_ln('Who did they suspect?')),
	read(Suspect), nl,
	(not(character(Suspect)) -> write_ln('Invalid character'), record_opponent_suggestion;
	write_ln('What weapon did they suspect?')),
	read(Weapon), nl,
	(not(weapon(Weapon)) -> write_ln('Invalid weapon'), record_opponent_suggestion;
	write_ln('What room did they suspect?')),
	read(Room), nl,
	(not(room(Room)) -> write_ln('Invalid room'), record_opponent_suggestion;
	write_ln('Was a card shown to them?')),
	read(Shown), nl,
	(
		 Shown = no -> card_not_shown(Suspect), card_not_shown(Weapon), card_not_shown(Room);
		 Shown = yes -> opponent_saw_card(Player, Suspect, Weapon, Room);
		 write_ln('Please choose a valid option.'), record_opponent_suggestion
	),
	main_menu.

%an opponent was shown a card
opponent_saw_card(Player, Suspect, Weapon, Room) :-
	write_ln('Did you show them the card?'),
	read(Answer), nl,
	(
		Answer = no -> another_opponent_has_card(Player, Suspect, Weapon, Room);
		Answer = yes -> i_showed_card(Player, Suspect, Weapon, Room);
		write_ln(''), record_opponent_suggestion
	),
	main_menu.

%a player showed their card therefore they may have any of the following cards
another_opponent_has_card(Player, Suspect, Weapon, Room) :- 
	write_ln('Who showed their card?'),
	read(Reveal), nl,
	(
		not(player(Reveal)) -> write_ln('Invalid player.'), another_opponent_has_card;
		assert(cards_data(Suspect, Reveal, 1)),
		assert(cards_data(Weapon, Reveal, 1)),
		assert(cards_data(Room, Reveal, 1)),
		assert(cards_data(Suspect, Player, 4)),
		assert(cards_data(Weapon, Player, 4)),
		assert(cards_data(Room, Player, 4))
	),
	main_menu.

%i showed a player my card
i_showed_card(Player, Suspect, Weapon, Room) :- 
	write_ln('Which card did you show?'),
	read(Card), nl,
	(
		Card = Suspect -> assert(cards_data(Suspect, Player, 3));
		Card = Weapon -> assert(cards_data(Weapon, Player, 3));
		Card = Room -> assert(cards_data(Room, Player, 3));
		write_ln('Please choose a valid option.'), i_showed_card
	),
	main_menu.

% View database/what opponents know as well
view_database :-
	write_ln('Cards that could have been at the murder scene:'), nl,
	write_ln('Characters'), nl,
	view_remaining_characters, nl,
	write_ln('Weapons'), nl,
	view_remaining_weapons, nl,
	write_ln('Rooms'), nl,
	view_remaining_rooms,nl,
	write_ln('Cards that you know that were not at murder scene: '),
	forall(in_hand(Card), writef("- %t\n", [Card])), nl,
	write_ln('Opponents Knowledge:'),
	print_player_card_status, nl,
	main_menu.

% Prints what cards/status opponents have
% TODO: Maybe instead of outputting status as a number, output text
print_player_card_status :-
	player(Player), not(my_character(Player)),
	writef("\nPlayer %t:", [Player]), nl,
	cards_data(Card, Player, Status),
	format('- ~w~t~25|~w', [Card, Status]), nl, % pretty formatting. fugly code.
	fail.
print_player_card_status :- true.

% View Remaining Items
view_remaining_characters :- forall(remaining_character(Card), writef("- %t\n", [Card])).
view_remaining_weapons :- forall(remaining_weapon(Card), writef("- %t\n", [Card])).
view_remaining_rooms :- forall(remaining_room(Card), writef("- %t\n", [Card])).

% gives remaining items
remaining_character(Card) :- character(Card), not(in_hand(Card)).
remaining_weapon(Card) :- weapon(Card), not(in_hand(Card)).
remaining_room(Card) :- room(Card), not(in_hand(Card)).

% checks when there is one card left in each category
one_character_left(Card) :- findall(1,remaining_character(Card),L), length(L,1), remaining_character(Card).
one_weapon_left(Card) :- findall(1,remaining_weapon(Card),L), length(L,1), remaining_weapon(Card).
one_room_left(Card) :- findall(1,remaining_room(Card),L), length(L,1), remaining_room(Card). 

% Check if a card is a valid card
is_valid_card(Card) :- character(Card);weapon(Card);room(Card).

% Check if it is possible to accuse
can_accuse(Character, Weapon, Room) :-
	one_character_left(Character), one_weapon_left(Weapon), one_room_left(Room),
	write_ln('You can make an accusation!'),
	writef("%t killed Mr. Bobby with a %t in the %t.\n\n", [Character, Weapon, Room]).
can_accuse(_,_,_) :- true.


% Removes everything from database so we don't need
clear_database :-
	retractall(cards_data(_,_,_)),
	retractall(num_players(_)),
	retractall(my_character(_)),
	retractall(player(_)),
	retractall(in_hand(_)).

% Clear screen function taken from 
% http://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows
clear_screen :- write('\e[H\e[2J').