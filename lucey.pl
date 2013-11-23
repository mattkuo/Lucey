clue :- get_startup_data.

% Dynamic database
:- dynamic num_players/1.
:- dynamic my_number/1.
:- dynamic player_num/1.
:- dynamic in_hand/1.


% Cards
character(mustard).
character(scarlet).
character(plum).
character(green).
character(white).
character(peacock).

weapon(rope).
weapon(pipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(revolver).

room(kitchen).
room(ballroom).
room(conservatory).
room(dining).
room(billiard).
room(library).
room(lounge).
room(hall).
room(study).

% Program entry point
get_startup_data :-
	clear_database,
	input_num_players,
	input_turn,
	input_hand.

% Input how many human players are playing the game
input_num_players :-
	write_ln('Input # of players: '),
	read(N),
	nl,
	assert(num_players(N)),
	input_num_players_helper(N).

% Input players into database
input_num_players_helper(0). % Need this so program flow continues
input_num_players_helper(N) :-
	N > 0,
	assert(player_num(N)),
	I is N - 1,
	input_num_players_helper(I).

input_turn :-
	write_ln('What is your player number? :'),
	read(Number),
	assert(my_number(Number)),
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

is_valid_card(Card) :- character(Card);weapon(Card);room(Card).

clear_database :-
	retractall(num_players(_)),
	retractall(my_number(_)),
	retractall(player_num(_)),
	retractall(in_hand(_)).

% Clear screen function taken from 
% http://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows
clear_screen :- write('\e[2J').