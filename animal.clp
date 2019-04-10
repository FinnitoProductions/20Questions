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
(defglobal ?*FOUND_SOLUTION* = FALSE) ; whether or not the game has reached a solution

(do-backward-chaining canFly)
(do-backward-chaining swimsOften)
(do-backward-chaining warmBlooded)
(do-backward-chaining legs)
(do-backward-chaining canSurviveOnLand)
(do-backward-chaining isEaten)
(do-backward-chaining smallerThanAHuman)
(do-backward-chaining endemicToAfrica)
(do-backward-chaining isDark)
(do-backward-chaining isMulticolored)
(do-backward-chaining hasShell)
(do-backward-chaining hotEnvironment)

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

               Respond \"yes\" (or any phrase beginning with \"y\" or \"Y\") to indicate affirmation, 
                  \"no\" (or any phrase beginning with \"n\" or \"N\") to indicate refutation,
                  and \"?\" (or any phrase beginning with \"?\") to indicate uncertainty.
                  
               I will use the information from these questions to guess which animal you are thinking of once I have 
                  enough information. Good luck!")
)

/*
* Asks the user whether the animal they are thinking of can fly. Triggers when the system
* needs to determine whether the animal can fly to narrow down the possibilities of the given animal.
*/
(defrule askCanFly "Ask if the animal the user is thinking of can fly."
   (need-canFly ?)
   =>
   (bind ?userResponse (askForFact "Can the given animal fly"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (canFly yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (canFly no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (canFly unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of swims often. Triggers when the system
* needs to determine whether the animal can fly to narrow down the possibilities of the given animal.
*/
(defrule askSwimsOften "Ask if the animal the user is thinking of swims often."
   (need-swimsOften ?)
   =>
   (bind ?userResponse (askForFact "Does the given animal swim often"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (swimsOften yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (swimsOften no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (swimsOften unsure))
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
* Determines how many legs the given animal has by asking whether it has 4, 0, 2, or 6 legs. Triggers when the system
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
   (bind ?userResponse (askForFact "Is the given animal either brown or black"))
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
* Asks the user whether the animal they are thinking of lives in a hot environment. Triggers when the system
* needs to determine whether the animal can survive in hot environments to narrow down the possibilities of the given animal.
*/
(defrule askHotEnvironment "Ask if the animal the user is thinking of lives in a hot environment"
   (need-hotEnvironment ?)
   =>
   (bind ?userResponse (askForFact "Does the given animal live exclusively in warm environments"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (hotEnvironment yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (hotEnvironment no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (hotEnvironment unsure))
   )
)

/*
* Defines the characteristics representative of a dolphin. If all these are met, 
* will print that the animal is a dolphin.
*/
(defrule dolphinRule "Defines the unique characteristics of a standard dolphin."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (legs 0)
   (canSurviveOnLand no)
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
   (canFly no)
   (swimsOften no)
   (legs 4)
   (smallerThanAHuman yes)
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
   (canFly no)
   (swimsOften no)
   (legs 4)
   (smallerThanAHuman no)
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
   (canFly no)
   (swimsOften no)
   (legs 4)
   (smallerThanAHuman yes)
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
   (canFly no)
   (swimsOften no)
   (legs 4)
   (smallerThanAHuman no)
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
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (legs 4)
   (isDark ?x &~no) ; accounts for potential uncertainty in the bear's fur color (unsure or yes are both acceptable)
   (hotEnvironment no)
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
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (isEaten no)
   (isDark yes)
   (legs 2)
   (hotEnvironment yes)
   (isMulticolored ?x &~no) ; accounts for potential uncertainty in monkey's multicoloredness (will accept unsure or yes)
   =>
   (printout t "The animal is a monkey." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a armadillo. If all these are met, 
* will print that the animal is a armadillo.
*/
(defrule armadilloRule "Defines the unique characteristics of a standard armadillo."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman yes)
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
   (canFly no)
   (swimsOften ?x) ; will accept any value for swims often because penguins could be seen as land-based or non-land-based
   (warmBlooded yes)
   (legs 2)
   (hotEnvironment no)
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
   (canFly yes)
   (swimsOften no)
   (warmBlooded yes)
   (legs 2)
   (hotEnvironment yes)
   (isMulticolored yes)
   =>
   (printout t "The animal is a parrot." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a water buffalo. If all these are met, 
* will print that the animal is a water buffalo.
*/
(defrule waterBuffaloRule "Defines the unique characteristics of a standard water buffalo."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (legs 4)
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
   (canFly no)
   (swimsOften yes)
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
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman no)
   (isEaten no)
   (isDark no)
   (hotEnvironments yes)
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
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (legs ?x &~4 &~6) ; accounts for potential uncertainty in the number of legs of a shrimp (allowing unsure or any number that is not 4 or 6)
   (canSurviveOnLand no)
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
   (canFly no)
   (swimsOften no)
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
   (canFly no)
   (swimsOften no)
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
   (canFly yes)
   (swimsOften no)
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
   (canFly yes)
   (swimsOften no)
   (warmBlooded ?x &~yes) ; accounts for potential uncertainty in the bee's bloodedness (unsure or no are both acceptable)
   (legs 6)
   (isMulticolored yes)
   => 
   (printout t "The animal is a bee." crlf)
   (assert (solutionFound))
)

/*
* Defines the characteristics representative of a elephant. If all these are met, 
* will print that the animal is a elephant.
*/
(defrule seaLionRule "Defines the unique characteristics of a standard sea lion."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (legs 0)
   (canSurviveOnLand yes)
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
* Triggers when an animal has been guessed, stopping the system from trying to guess any more animals. 
*/
(defrule foundSolution "Shuts off the system after the solution has been guessed."
   (solutionFound)
   =>
   ; (clear) 
   ; (reset)
   (bind ?*FOUND_SOLUTION* TRUE)
)

(reset)
(run)
(if (not ?*FOUND_SOLUTION*) then (printout t "Sorry! I was unable to determine what animal you were thinking of." crlf))
(return)