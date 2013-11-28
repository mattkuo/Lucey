LUCEY: A Clue Board Game Assistant
By Matthew Kuo and Tricia Jose


A. A FEW THINGS TO KEEP IN MIND

•	All commands in this program must be terminated with a "." and entered with the Enter key on your keyboard
•	This program assumes that game pieces are the standard/default Clue board game pieces


B. STARTING THE PROGRAM
** (When using the program, please do not enter any quotation marks, they are in this instruction manual simply for ease of reading) **

•	Compile program by entering "[lucey]."
 
•	Start the program by entering "clue."
 
•	You will then be prompted to enter necessary information for starting the game

C. HOW IT WORKS: THE MAIN MENU

The user is presented with 5 options after all the necessary start-up information has been input.
 
Each time the menu is instantiated, it also checks whether or not a final accusation can be made, that is, when only one item from each category remains unknown

The menu options are as follows:


 1. Record my suggestion

	A.	If a card is shown to me by player X, it is added to my database in two areas:
			I.	My list of known cards
			II.	The list of cards player X has in his hand
	B.	If a card is not shown to me, and I have the remaining ones in my hand, all the other cards of that category are moved to my list of known cards

 2. Record my opponent's suggestion

	A.	If a card is shown by me to player X, it is recorded in my database as a card that player X has seen that I have shown
	B.	If a card is shown by another player Y to player X, it is added to my database in one of 2 ways:
			1)	If I have or know of the remaining cards:
				I.	My list of known cards
				II.	The list of cards player Y has
			2)	If I don’t have or know of the remaining cards:
				I.	The list of cards player X may have seen
				II.	The list of cards player Y may have
	C.	If no card is shown to person X, and either i have the remaining ones in my hand, or person X does, all the other cards of that category are moved to my list of known cards

 3. Suggest next move

	Makes a suggestion based on what room I’m in, will use two known cards to learn something about an unknown card

 4. View Database

	This displays the entire database and is split into sections:
			A.	Cards that remain unknown to me, divided into suspect/weapon/room
			B.	Cards that cannot be in the envelope (I hold or have seen them or have deduced them)
			C.	Knowledge about my opponents cards
				Each card is assigned a numbered "status" in this section: 
						0 - Player does not have card in their hand.
						1 - Player might have card in their hand.
						2 - Player has card in their hand.
						3 - This is my card and player has seen it
						4 - This may be someone else's card and player may have seen this card

 5. Exit program 

	This exits the program
