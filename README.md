# AnimalGame
Code for the "20 Questions" project in my expert systems course.

Plays "20 Questions" with the user where the user thinks of an animal (out of a given list of options) and
the program will guess the animal by asking a series of yes/no questions (no more than 20) and apply the
rule engine and backward chaining to determine which animal they are thinking of. If the engine cannot guess the animal, will let the user know.
 
Each time you would like to play the animal game, call the playGame function to clear out the rule engine. The system will
continue asking questions until it exhausts the available possibilities, exceeds the allowed number of questions (20), or 
determines what animal the user is thinking of.
