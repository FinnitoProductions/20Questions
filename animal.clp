/*
* Finn Frankis
* March 16, 2019
*
* Plays "20 Questions" with the user where the user thinks of an animal (out of a given list of options) and
* the program will guess the animal by asking a series of yes/no questions (no more than 20).
*/

(clear)
(reset)

(batch util/utilities.clp)

(defglobal ?*questionNumber* = 1) ; question number will start at #1 and be incremented each time
(defglobal ?*VALID_YES_CHARACTER* = "y") ; will accept any string starting with this as indicating "yes"
(defglobal ?*VALID_NO_CHARACTER* = "n") ; will accept any string starting with this as indicating "no"
(defglobal ?*VALID_UNCERTAIN_CHARACTER* = "?") ; will accept any string starting with this as indicating uncertainty
(defglobal ?*INVALID_INPUT_MESSAGE* = "Your input was invalid. Please try again.")

(do-backward-chaining landBased)
(do-backward-chaining warmBlooded)
(do-backward-chaining legs)
(do-backward-chaining canSurviveOnLand)
(do-backward-chaining isEaten)
(do-backward-chaining smallerThanAHuman)
(do-backward-chaining endemicToAfrica)
(do-backward-chaining isDark)
(do-backward-chaining isMulticolored)
(do-backward-chaining hasShell)

(do-backward-chaining findSolution)

/*
* Starts up the game and presents the detailed instructions to the user.
*/
(defrule startup "Starts up the game and presents the instructions to the user."
   (declare (salience 100)) ; guarantees that this rule will be run before all others by giving it a very high weight
   =>
   (printline "Welcome to the Think of an Animal Game!
               Choose one of the following animals: dolphin, dog, snake, elephant, sea lion,
               penguin, bee, camel, pig, zebra, bear, monkey, snail, armadillo, shrimp, parrot, water buffalo,
               bat, and tortoise. I will ask you a series of questions about your animal, not exceeding 20 questions.
               I will use the information from these questions to guess which animal you are thinking of once I have 
               enough information. Good luck!")
)

/*
* Asks the user whether the animal they are thinking of is land-based. Triggers when the system
* needs to determine whether the animal is land-based to narrow down the possibilities of the given animal.
*/
(defrule askLandBased "Ask if the animal the user is thinking of lives on land."
   (need-landBased ?)
   =>
   (bind ?userResponse (askForFact "Is the animal exclusively land-based (cannot fly and would rarely be found in water)"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (landBased yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (landBased no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (landBased unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of is warm-blooded. Triggers when the system
* needs to determine whether the animal is warm-blooded to narrow down the possibilities of the given animal.
*/
(defrule askWarmBlooded "Ask if the animal the user is thinking of is warm-blooded."
   (need-warmBlooded ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal warm-blooded"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (warmBlooded yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (warmBlooded no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (warmBlooded unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of is warm-blooded. Triggers when the system
* needs to determine how many legs the animal has to narrow down the possibilities of the given animal.
*/
(defrule askLegs "Determines how many legs the animal the user is thinking of has."
   (need-legs ?)
   =>
   (bind ?legOptions (create$ 4 0 2 6))
   (bind ?didSucceed FALSE) ; whether or not the user has answered YES or UNSURE to any of the leg questions

   (foreach ?option ?legOptions
      (if (not ?didSucceed) then
         (bind ?userResponse (askForFact (str-cat "Does the given animal have " ?option " legs (excluding flippers)")))

         (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then 
            (assert (legs ?option)) 
            (bind ?didSucceed TRUE)
         elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (bind ?didSucceed FALSE)
         elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then
            (assert (legs unsure)) 
            (bind (?didSucceed TRUE))
         )
      )
   )

   (if (not ?didSucceed) then
      (assert (legs unsure))
   )
) ; askLegs (need-legs ?)

/*
* Asks the user whether the animal they are thinking of can survive on land. Triggers when the system
* needs to determine whether the animal can survive on land to narrow down the possibilities of the given animal.
*/
(defrule askCanSurviveOnLand "Ask if the animal the user is thinking of can survive on land."
   (need-canSurviveOnLand ?)
   =>
   (bind ?userResponse (askForFact "Can the given animal survive on land"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (canSurviveOnLand yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (canSurviveOnLand no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (canSurviveOnLand unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of can be eaten in Western culture. Triggers when the system
* needs to determine whether the animal can be eaten to narrow down the possibilities of the given animal.
*/
(defrule askIsEaten "Ask if the animal the user is thinking of can be eaten."
   (need-isEaten ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal commonly eaten in Western culture"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (isEaten yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (isEaten no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (isEaten unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of is smaller than a human when both are adults. Triggers when the system
* needs to determine whether the animal is smaller than a human to narrow down the possibilities of the given animal.
*/
(defrule askSmallerThanAHuman "Ask if the animal the user is thinking of is smaller than a human, as an adult."
   (need-smallerThanAHuman ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal, when fully grown, smaller than an adult human"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (smallerThanAHuman yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (smallerThanAHuman no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (smallerThanAHuman unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of is endemic to Africa. Triggers when the system
* needs to determine whether the animal is endemic to Africa to narrow down the possibilities of the given animal.
*/
(defrule askEndemicToAfrica "Ask if the animal the user is thinking of is endemic to Africa."
   (need-endemicToAfrica ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal endemic to Africa"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (endemicToAfrica yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (endemicToAfrica no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (endemicToAfrica unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of is dark. Triggers when the system
* needs to determine whether the animal is dark to narrow down the possibilities of the given animal.
*/
(defrule askIsDark "Ask if the animal the user is thinking of is dark."
   (need-isDark ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal very dark in color throughout its coat (brown or black, but not grey)"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (isDark yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (isDark no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (isDark unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of is multicolored. Triggers when the system
* needs to determine whether the animal is multicolored to narrow down the possibilities of the given animal.
*/
(defrule askIsMulticolored "Ask if the animal the user is thinking of is multicolored."
   (need-isMulticolored ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal multicolored"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (isMulticolored yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (isMulticolored no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (isMulticolored unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of has a shell. Triggers when the system
* needs to determine whether the animal has a shell to narrow down the possibilities of the given animal.
*/
(defrule askHasShell "Ask if the animal the user is thinking of has a shell"
   (need-hasShell ?)
   =>
   (bind ?userResponse (askForFact "Does the given animal have a shell"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (hasShell yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (hasShell no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (hasShell unsure))
   )
)

/*
* Defines the characteristics representative of a dolphin. If all these are met, 
* will print that the animal is a dolphin.
*/
(defrule dolphinRule "Defines the unique characteristics of a standard dolphin."
   (landBased no)
   (warmBlooded yes)
   (endemicToAfrica no)
   (canSurviveOnLand no)
   (legs 0)
   (hasShell no)
   =>
   (printout t "The animal is a dolphin." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a dog. If all these are met, 
* will print that the animal is a dog.
*/
(defrule dogRule "Defines the unique characteristics of a standard dog."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman yes)
   (endemicToAfrica no)
   (isEaten no)
   (hasShell no)
   =>
   (printout t "The animal is a dog." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a camel. If all these are met, 
* will print that the animal is a camel.
*/
(defrule camelRule "Defines the unique characteristics of a standard camel."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman no)
   (endemicToAfrica yes)
   (isEaten no)
   (isDark yes)
   (isMulticolored no)
   =>
   (printout t "The animal is a camel." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a pig. If all these are met, 
* will print that the animal is a pig.
*/
(defrule pigRule "Defines the unique characteristics of a standard pig."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman yes)
   (endemicToAfrica no)
   (isEaten yes)
   (isMulticolored yes)
   =>
   (printout t "The animal is a pig." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a zebra. If all these are met, 
* will print that the animal is a zebra.
*/
(defrule zebraRule "Defines the unique characteristics of a standard zebra."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman no)
   (endemicToAfrica yes)
   (isEaten no)
   (isDark no)
   (isMulticolored yes)
   =>
   (printout t "The animal is a zebra." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a bear. If all these are met, 
* will print that the animal is a bear.
*/
(defrule bearRule "Defines the unique characteristics of a standard bear."
   (landBased yes)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (endemicToAfrica no)
   (legs 4)
   (isEaten no)
   =>
   (printout t "The animal is a bear." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a monkey. If all these are met, 
* will print that the animal is a monkey.
*/
(defrule monkeyRule "Defines the unique characteristics of a standard monkey."
   (landBased yes)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (endemicToAfrica yes)
   (isEaten no)
   (isDark yes)
   (legs 2)
   (isMulticolored yes)
   =>
   (printout t "The animal is a monkey." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a armadillo. If all these are met, 
* will print that the animal is a armadillo.
*/
(defrule armadilloRule "Defines the unique characteristics of a standard armadillo."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman yes)
   (endemicToAfrica no)
   (isEaten no)
   (hasShell yes)
   =>
   (printout t "The animal is a armadillo." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a penguin. If all these are met, 
* will print that the animal is a penguin.
*/
(defrule penguinRule "Defines the unique characteristics of a standard penguin."
   (landBased yes)
   (warmBlooded yes)
   (endemicToAfrica no)
   (hasShell no)
   (legs 2)
   (isMulticolored yes)
   =>
   (printout t "The animal is a penguin." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a parrot. If all these are met, 
* will print that the animal is a parrot.
*/
(defrule parrotRule "Defines the unique characteristics of a standard parrot."
   (landBased no)
   (warmBlooded yes)
   (legs 2)
   =>
   (printout t "The animal is a parrot." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a water buffalo. If all these are met, 
* will print that the animal is a water buffalo.
*/
(defrule waterBuffaloRule "Defines the unique characteristics of a standard water buffalo."
   (landBased no)
   (warmBlooded yes)
   (legs 4)
   (endemicToAfrica no)
   (canSurviveOnLand yes)
   =>
   (printout t "The animal is a water buffalo." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a tortoise. If all these are met, 
* will print that the animal is a tortoise.
*/
(defrule tortoiseRule "Defines the unique characteristics of a standard tortoise."
   (landBased no)
   (warmBlooded no)
   (legs 4)
   (canSurviveOnLand yes)
   (hasShell yes)
   =>
   (printout t "The animal is a tortoise." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a elephant. If all these are met, 
* will print that the animal is a elephant.
*/
(defrule elephantRule "Defines the unique characteristics of a standard elephant."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman no)
   (endemicToAfrica yes)
   (isEaten no)
   (isDark no)
   (isMulticolored no)
   =>
   (printout t "The animal is a elephant." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a shrimp. If all these are met, 
* will print that the animal is a shrimp.
*/
(defrule shrimpRule "Defines the unique characteristics of a standard shrimp."
   (landBased no)
   (warmBlooded no)
   (canSurviveOnLand no)
   (legs 0)
   (hasShell yes)
   =>
   (printout t "The animal is a shrimp." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a snake. If all these are met, 
* will print that the animal is a snake.
*/
(defrule snakeRule "Defines the unique characteristics of a standard snake."
   (landBased yes)
   (warmBlooded no)
   (legs 0)
   (isEaten no)
   =>
   (printout t "The animal is a snake." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a snail. If all these are met, 
* will print that the animal is a snail.
*/
(defrule snailRule "Defines the unique characteristics of a standard snail."
   (landBased yes)
   (warmBlooded ?x &~yes) ; accounts for potential uncertainty in the snail's bloodedness (unsure or no are both acceptable)
   (legs 0)
   (isEaten yes)
   (hasShell yes)
   =>
   (printout t "The animal is a snail." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a bat. If all these are met, 
* will print that the animal is a bat.
*/
(defrule batRule "Defines the unique characteristics of a standard bat."
   (landBased no)
   (warmBlooded ?x &~yes) ; accounts for potential uncertainty in the bat's bloodedness (unsure or no are both acceptable)
   (legs 2)
   =>
   (printout t "The animal is a bat." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a bee. If all these are met, 
* will print that the animal is a bee.
*/
(defrule beeRule "Defines the unique characteristics of a standard bee."
   (landBased no)
   (legs 6)
   => 
   (printout t "The animal is a bee." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a elephant. If all these are met, 
* will print that the animal is a elephant.
*/
(defrule seaLionRule "Defines the unique characteristics of a standard sea lion."
   (landBased no)
   (warmBlooded yes)
   (canSurviveOnLand yes)
   (legs 0)
   =>
   (printout t "The animal is a sea lion." crlf)
   (assert (solutionFound))
)

/*
* Requests the user for a response to a given question. If it is valid (starts with either "Y", "N", or "?"), 
* returns the starting character. Otherwise returns FALSE.
*/
(deffunction requestValidatedInput (?questionVal)
   (bind ?userInput (askQuestion (str-cat ?*questionNumber* ". " ?questionVal)))
   (bind ?firstCharacter (lowcase (sub-string 1 1 ?userInput)))

   (bind ?isYesChar (eq ?firstCharacter ?*VALID_YES_CHARACTER*))
   (bind ?isNoChar (eq ?firstCharacter ?*VALID_NO_CHARACTER*))
   (bind ?isUncertainChar (eq ?firstCharacter ?*VALID_UNCERTAIN_CHARACTER*))
   (bind ?isValid (or ?isYesChar ?isNoChar ?isUncertainChar))

   (if ?isValid then (bind ?returnVal ?firstCharacter)
    else (bind ?returnVal FALSE)
   )

   (return ?returnVal)
)

/*
* Asks the user whether a given fact is true or false (or if they are unsure). Valid input include any string starting 
* with "Y" to indicate yes, "N" to indicate no, and "?" to indicate uncertainty.
*
* Returns "Y" if the user specified yes, "N" if the user specified no, and "?" if the user specified uncertainty.
*/
(deffunction askForFact (?questionVal)
   (bind ?userInput (requestValidatedInput ?questionVal))

   (while (eq ?userInput FALSE) 
      (printline ?*INVALID_INPUT_MESSAGE*)
      (bind ?userInput (requestValidatedInput ?questionVal))
   )

   (++ ?*questionNumber*)

   (return ?userInput)
)

/*
* If the expert system was unable to determine the solution to the problem based on the user's answers to the questions,
* will inform the user that there was no solution.
*/
(defrule noSolution "Informs the user that the solution to the problem was not found."
   (need-findSolution)
   =>
   (printline "Sorry! I was unable to determine what animal you were thinking of.")
)

/*
* Triggers when an animal has been guessed, stopping the system from trying to guess any more animals. 
*/
(defrule foundSolution "Shuts off the system after the solution has been guessed."
   (solutionFound)
   =>
   (clear) 
   (reset)
)

(reset)
(run)
(return)