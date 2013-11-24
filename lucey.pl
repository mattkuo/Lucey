clue :- get_startup_data, main_menu.

% Dynamic database
:- dynamic num_players/1.    % represents # of human players playing
:- dynamic my_character/1.   % The user's character
:- dynamic player/1.         % players are represented by the character they are playing as
:- dynamic in_hand/1.        % Cards in the user's hand

% temporary global variable (might not be a good idea)
:- dynamic suspect/1.

% Assigns a "status" to each card.
% cards_data(Card, Player, Status)
% Card can be any character, weapon or room
% Player is which player the data belongs to
% Status is a number (0, 1, 2):  0 - Player does not have card.
%				 1 - Player might have card.
%				 2 - Player has card.

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
	write_ln('#### MAIN MENU ####'),
	write_ln('1) Record my suggestion'),
	write_ln('2) View Database'),
	write_ln('3) Exit program'),
	read(Option),
	(
	 	Option = 1 -> record_suggestion;
	 	Option = 2 -> view_database;
	 	Option = 3 -> halt;
	 	write_ln('Please choose a valid option.'), main_menu
 	).

% TODO: Record player suggestions here
record_suggestion :-
	write_ln('Who did you suspect?'),
	read(Suspect),assert(suspect(Suspect)), nl,
	not(character(Suspect)) -> write_ln('Invalid character'), record_suggestion;
	write_ln('What weapon did the suspect use?'),
	read(Weapon), assert(suspect(Weapon)), nl,
	not(weapon(Weapon)) -> write_ln('Invalid weapon'), record_suggestion;
	write_ln('What room are you in?'),
	read(Room), assert(suspect(Room)), nl,
	not(room(Room)) -> write_ln('Invalid room'), record_suggestion;
	write_ln('Was a card shown to you?'),
	read(Shown), nl,
	(Shown = no -> suspect(Card), card_not_shown(Card), retract(suspect(Card));
	 Shown = yes -> card_shown, main_menu;
	 write_ln('Please choose a valid option.'), record_suggestion),
	main_menu.

% Updates database for the case that no card was shown after user suggestion
% Card_data set to 0 for all opponents 
card_not_shown(Card) :-
	player(Others),
	not(my_character(Others)),
	retractall(cards_data(Card, Others, _)),
	assert(cards_data(Card, Others, 0)).

card_shown :-
	write_ln('Which card was shown?'),
	read(Card),
	
% TODO: display database (just display cards_data?)
view_database :- true.

is_valid_card(Card) :- character(Card);weapon(Card);room(Card).

clear_database :-
	retractall(cards_data(_,_,_)),
	retractall(num_players(_)),
	retractall(my_character(_)),
	retractall(player(_)),
	retractall(in_hand(_)).

% Clear screen function taken from 
% http://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows
clear_screen :- write('\e[2J').