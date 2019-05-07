/*
* Finn Frankis
* March 16, 2019
*
* Plays "20 Questions" with the user where the user thinks of an animal (out of a given list of options) and
* the program will guess the animal by asking a series of yes/no questions (no more than 20) and apply the
* rule engine to determine which animal they are thinking of. If the engine cannot guess the animal, will let the user know.
* 
* Each time you would like to play the animal game, call the playGame function to clear out the rule engine. The system will
* continue asking questions until it exhausts the available possibilities, exceeds the allowed number of questions (20), or 
* determines what animal the user is thinking of.
*/

(clear)
(reset)

(batch util/utilities.clp)

(defglobal ?*questionNumber* = 1) ; question number will start at #1 and be incremented each time
(defglobal ?*VALID_YES_CHARACTER* = "y") ; will accept any string starting with this as indicating "yes"
(defglobal ?*VALID_NO_CHARACTER* = "n") ; will accept any string starting with this as indicating "no"
(defglobal ?*VALID_UNCERTAIN_CHARACTER* = "?") ; will accept any string starting with this as indicating uncertainty
(defglobal ?*INVALID_INPUT_MESSAGE* = "Your input was invalid. Please try again.")
(defglobal ?*foundSolution* = FALSE) ; whether or not the game has reached a solution
(defglobal ?*ANIMAL_RULE_SUFFIX* = "Rule") ; the suffix which will follow each rule defining the characteristics of a given animal
(defglobal ?*TOTAL_ALLOWED_QUESTIONS* = 20) ; the game never reaches 20 questions, but this can be tested by assigning the variable to a lower number (> 1)

(set-reset-globals TRUE) ; reset global variables when a (reset) is called to allow ?*foundSolution* and ?*questionNumber* to be reset with each new game

/*
* Define all the traits which will be backward-chained, meaning if they have not been asserted but are needed
* to determine whether a rule should be triggered, a separate backward-chained rule will be fired to ask the user 
* about the given trait.
*/
(do-backward-chaining canFly)
(do-backward-chaining swimsOften)
(do-backward-chaining warmBlooded)
(do-backward-chaining legs)
(do-backward-chaining canSurviveOnLand)
(do-backward-chaining isEaten)
(do-backward-chaining smallerThanAHuman)
(do-backward-chaining isDark)
(do-backward-chaining isMulticolored)
(do-backward-chaining hasShell)
(do-backward-chaining coldEnvironment)
(do-backward-chaining hasHeadProtrusions)
(do-backward-chaining hasTrunk)

/*
* Starts up the game and presents the detailed instructions to the user.
*/
(defrule startup "Starts up the game and presents the instructions to the user."
   (declare (salience 100)) ; guarantees that this rule will be run before all others by giving it a very high weight
   =>
   (printline "Welcome to the Think of an Animal Game!
I will ask you a series of questions about your animal, not exceeding 20 questions.

Respond \"yes\" (or any phrase beginning with \"y\" or \"Y\") to indicate affirmation, 
\"no\" (or any phrase beginning with \"n\" or \"N\") to indicate refutation,
and \"?\" (or any phrase beginning with \"?\") to indicate uncertainty.

Call the (playGame) function to play again.
                  
I will use the information from these questions to guess which animal you are thinking of once I have 
enough information. Good luck!")
   (bind ?*foundSolution* FALSE)
) ; startup

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
* needs to determine whether the animal swims often to narrow down the possibilities of the given animal.
*/
(defrule askSwimsOften "Ask if the animal the user is thinking of swims often."
   (need-swimsOften ?)
   =>
   (bind ?userResponse (askForFact "Can the given animal often be found immersed in water"))
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
* Determines how many legs the given animal has by asking whether it has 4, 0, 2, 6, or 8 legs. Triggers when the system
* needs to determine how many legs the animal has to narrow down the possibilities of the given animal.
*/
(defrule askLegs "Determines how many legs the animal the user is thinking of has."
   (need-legs ?)
   =>
   (bind ?legOptions (create$ 4 0 2 6 8)) ; the list of all possible values of legs the system will ask about
   (bind ?didSucceed FALSE)               ; whether or not the user has answered YES or UNSURE to any of the leg questions

   (foreach ?option ?legOptions
      (if (not ?didSucceed) then ; only continue to ask if the user has not answered YES or UNSURE to any of the questions
         (bind ?userResponse (askForFact (str-cat "Does the given animal have " ?option " legs (excluding flippers or fins)")))

         (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then ; the user has responded with affirmation; no need to ask anything more
            (assert (legs ?option)) 
            (bind ?didSucceed TRUE)
         elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (bind ?didSucceed FALSE) ; keep asking until the list is exhausted or they answer YES or UNSURE
         elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then
            (assert (legs unsure)) 
            (bind ?didSucceed TRUE) ; if they are unsure about the number of legs, stop asking further leg questions
         )
      )
   )

   (if (not ?didSucceed) then ; the user answered NO to every question
      (assert (legs unsure))
   )
) ; askLegs 

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
* Asks the user whether the animal they are thinking of is dark. Triggers when the system
* needs to determine whether the animal is dark to narrow down the possibilities of the given animal.
*/
(defrule askIsDark "Ask if the animal the user is thinking of is dark."
   (need-isDark ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal entirely either brown or black"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then 
      (assert (isDark yes))
      (assert (isMulticolored no)) ; if the animal is entirely brown/black, it cannot be multicolored
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
* Asks the user whether the animal they are thinking of lives in a cold environment. Triggers when the system
* needs to determine whether the animal can survive in cold environments to narrow down the possibilities of the given animal.
*/
(defrule askColdEnvironment "Ask if the animal the user is thinking of lives in a cold environment"
   (need-coldEnvironment ?)
   =>
   (bind ?userResponse (askForFact "Does the given animal live far more often in either cold or temperate environments"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (coldEnvironment yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (coldEnvironment no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (coldEnvironment unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of has any protrusions out of the top of its head (like antlers, horns, or tusks, but not ears).
* Triggers when the system needs to determine whether the animal has protrusions to narrow down the possibilities of the given animal.
*/
(defrule askHasHeadProtrusions "Ask if the animal the user is thinking of has protrusions from its head"
   (need-hasHeadProtrusions ?)
   =>
   (bind ?userResponse (askForFact "Does the given animal have protrusions from its head (like tusks, horns, antlers, or trunks, but not ears)"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (hasHeadProtrusions yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (hasHeadProtrusions no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (hasHeadProtrusions unsure))
   )
)

/*
* Asks the user whether the animal they are thinking of has a trunk. Triggers when the system
* needs to determine whether the animal has a trunk to narrow down the possibilities of the given animal.
*/
(defrule askHasTrunk "Ask if the animal the user is thinking of has a trunk"
   (need-hasTrunk ?)
   =>
   (bind ?userResponse (askForFact "Does the given animal have a trunk"))
   (if (eq ?userResponse ?*VALID_YES_CHARACTER*) then (assert (hasTrunk yes))
    elif (eq ?userResponse ?*VALID_NO_CHARACTER*) then (assert (hasTrunk no))
    elif (eq ?userResponse ?*VALID_UNCERTAIN_CHARACTER*) then (assert (hasTrunk unsure))
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
   (hasHeadProtrusions no)
   =>
   (printSolution "dolphin")
) ; dolphinRule

/*
* Defines the characteristics representative of a narwhal. If all these are met, 
* will print that the animal is a narwhal.
*/
(defrule narwhalRule "Defines the unique characteristics of a standard narwhal."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (legs 0)
   (canSurviveOnLand no)
   (hasShell no)
   (hasHeadProtrusions yes)
   =>
   (printSolution "narwhal")
) ; narwhalRule

/*
* Defines the characteristics representative of a goldfish. If all these are met, 
* will print that the animal is a goldfish.
*/
(defrule goldfishRule "Defines the unique characteristics of a standard goldfish."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (legs 0)
   (canSurviveOnLand no)
   (isEaten no)
   (hasShell no)
   =>
   (printSolution "goldfish")
) ; goldfishRule

/*
* Defines the characteristics representative of a salmon. If all these are met, 
* will print that the animal is a salmon.
*/
(defrule salmonRule "Defines the unique characteristics of a standard salmon."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (legs 0)
   (canSurviveOnLand no)
   (isEaten yes)
   (hasShell no)
   =>
   (printSolution "salmon")
) ; salmonRule

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
   (coldEnvironment no)
   (hasHeadProtrusions no)
   (hasShell no)
   =>
   (printSolution "dog")
) ; dogRule

/*
* Defines the characteristics representative of a rabbit. If all these are met, 
* will print that the animal is a rabbit.
*/
(defrule rabbitRule "Defines the unique characteristics of a standard rabbit."
   (canFly no)
   (swimsOften no)
   (legs 2)
   (smallerThanAHuman yes)
   (isEaten no)
   (hasShell no)
   =>
   (printSolution "rabbit")
) ; rabbitRule

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
   (coldEnvironment no)
   (isDark yes)
   (isMulticolored no)
   =>
   (printSolution "camel")
) ; camelRule

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
   =>
   (printSolution "pig")
) ; pigRule

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
   (isMulticolored yes)
   (hasHeadProtrusions no)
   =>
   (printSolution "zebra")
) ; zebraRule

/*
* Defines the characteristics representative of a black bear. If all these are met, 
* will print that the animal is a black bear.
*/
(defrule blackBearRule "Defines the unique characteristics of a standard black bear."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (legs 4)
   (isDark yes)
   (coldEnvironment yes)
   (hasHeadProtrusions no)
   (isEaten no)
   =>
   (printSolution "black bear")
) ; blackBearRule

/*
* Defines the characteristics representative of a polar bear. If all these are met, 
* will print that the animal is a polar bear.
*/
(defrule polarBearRule "Defines the unique characteristics of a standard polar bear."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (legs 4)
   (isDark no)
   (coldEnvironment yes)
   (hasHeadProtrusions no)
   (isEaten no)
   =>
   (printSolution "polar bear")
) ; polarBearRule

/*
* Defines the characteristics representative of a moose. If all these are met, 
* will print that the animal is a moose.
*/
(defrule mooseRule "Defines the unique characteristics of a standard moose."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (smallerThanAHuman no)
   (legs 4)
   (isDark yes)
   (coldEnvironment yes)
   (hasHeadProtrusions yes)
   (isEaten no)
   =>
   (printSolution "moose")
) ; mooseRule

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
   (legs 2)
   (coldEnvironment no)
   (isMulticolored ?multicolored &~no) ; accounts for potential uncertainty in monkey's multicoloredness (will accept unsure or yes)
   (hasHeadProtrusions no)
   =>
   (printSolution "monkey")
) ; monkeyRule

/*
* Defines the characteristics representative of an armadillo. If all these are met, 
* will print that the animal is an armadillo.
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
   (printSolution "armadillo")
) ; armadilloRule

/*
* Defines the characteristics representative of a penguin. If all these are met, 
* will print that the animal is a penguin.
*/
(defrule penguinRule "Defines the unique characteristics of a standard penguin."
   (canFly no)
   (swimsOften ?doesSwimOften &~no) ; accounts for potential uncertainty in whether a penguin swims often (will accept yes or unsure)
   (warmBlooded yes)
   (legs 2)
   (coldEnvironment yes)
   (isMulticolored yes)
   =>
   (printSolution "penguin")
) ; penguinRule

/*
* Defines the characteristics representative of a puffin. If all these are met, 
* will print that the animal is a puffin.
*/
(defrule puffinRule "Defines the unique characteristics of a standard puffin."
   (canFly yes)
   (swimsOften ?doesSwimOften &~no) ; accounts for potential uncertainty in whether a puffin swims often (will accept yes or unsure)
   (warmBlooded yes)
   (legs 2)
   (coldEnvironment yes)
   (isMulticolored yes)
   =>
   (printSolution "puffin")
) ; puffinRule

/*
* Defines the characteristics representative of an arctic squirrel. If all these are met, 
* will print that the animal is an arctic squirrel.
*/
(defrule arcticSquirrelRule "Defines the unique characteristics of a standard arctic squirrel."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (legs ?legNum &~0 &~6 &~8) ; accounts for potential uncertainty in an arctic squirrel's number of legs (will accept 2, 4, or unsure)
   (smallerThanAHuman yes)
   (coldEnvironment yes)
   =>
   (printSolution "arctic squirrel")
) ; arcticSquirrelRule 

/*
* Defines the characteristics representative of a parrot. If all these are met, 
* will print that the animal is a parrot.
*/
(defrule parrotRule "Defines the unique characteristics of a standard parrot."
   (canFly yes)
   (swimsOften no)
   (warmBlooded yes)
   (legs 2)
   (coldEnvironment no)
   (isMulticolored yes)
   =>
   (printSolution "parrot")
) ; parrotRule

/*
* Defines the characteristics representative of a crow. If all these are met, 
* will print that the animal is a crow.
*/
(defrule crowRule "Defines the unique characteristics of a standard crow."
   (canFly yes)
   (swimsOften no)
   (warmBlooded yes)
   (legs 2)
   (isDark yes)
   (isMulticolored no)
   =>
   (printSolution "crow")
) ; crowRule

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
   (coldEnvironment no)
   =>
   (printSolution "water buffalo")
) ; waterBuffaloRule

/*
* Defines the characteristics representative of a turtle. If all these are met, 
* will print that the animal is a turtle.
*/
(defrule turtleRule "Defines the unique characteristics of a standard turtle."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (legs 4)
   (isEaten no)
   (canSurviveOnLand yes)
   (hasShell yes)
   =>
   (printSolution "turtle")
) ; turtleRule

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
   (coldEnvironment no)
   (isMulticolored no)
   (hasHeadProtrusions yes)
   (hasTrunk yes)
   =>
   (printSolution "elephant")
) ; elephantRule

/*
* Defines the characteristics representative of an anteater. If all these are met, 
* will print that the animal is an anteater.
*/
(defrule anteaterRule "Defines the unique characteristics of a standard anteater."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman yes)
   (isEaten no)
   (isDark no)
   (coldEnvironment no)
   (isMulticolored yes)
   (hasHeadProtrusions yes)
   (hasTrunk yes)
   =>
   (printSolution "anteater")
) ; anteaterRule

/*
* Defines the characteristics representative of a giraffe. If all these are met, 
* will print that the animal is a giraffe.
*/
(defrule giraffeRule "Defines the unique characteristics of a standard giraffe."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman no)
   (isEaten no)
   (coldEnvironment no)
   (isMulticolored yes)
   (hasHeadProtrusions yes)
   =>
   (printSolution "giraffe")
) ; giraffeRule

/*
* Defines the characteristics representative of a rhino. If all these are met, 
* will print that the animal is a rhino.
*/
(defrule rhinoRule "Defines the unique characteristics of a standard rhino."
   (canFly no)
   (swimsOften no)
   (warmBlooded yes)
   (legs 4)
   (smallerThanAHuman no)
   (isEaten no)
   (isDark no)
   (coldEnvironment no)
   (isMulticolored no)
   (hasHeadProtrusions yes)
   (hasTrunk no)
   =>
   (printSolution "rhino")
) ; rhinoRule

/*
* Defines the characteristics representative of a shrimp. If all these are met, 
* will print that the animal is a shrimp.
*/
(defrule shrimpRule "Defines the unique characteristics of a standard shrimp."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (canSurviveOnLand no)
   (legs ?legNum &~0) ; allows for potential uncertainty in the shrimp's number of legs (can be anything but 0 legs)
   (hasShell yes)
   =>
   (printSolution "shrimp")
) ; shrimpRule

/*
* Defines the characteristics representative of a crab. If all these are met, 
* will print that the animal is a crab.
*/
(defrule crabRule "Defines the unique characteristics of a standard crab."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (isEaten yes)
   (canSurviveOnLand yes)
   (legs ?legNum &~0) ; allows for potential uncertainty in the crab's number of legs (can be anything but 0 legs)
   (hasShell yes)
   =>
   (printSolution "crab")
) ; crabRule

/*
* Defines the characteristics representative of a clam. If all these are met, 
* will print that the animal is a clam.
*/
(defrule clamRule "Defines the unique characteristics of a standard clam."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (canSurviveOnLand no)
   (legs 0)
   (hasShell yes)
   =>
   (printSolution "clam")
) ; clamRule

/*
* Defines the characteristics representative of an octopus. If all these are met, 
* will print that the animal is an octopus.
*/
(defrule octopusRule "Defines the unique characteristics of a standard octopus."
   (canFly no)
   (swimsOften yes)
   (warmBlooded no)
   (canSurviveOnLand no)
   (legs 8)
   (hasShell no)
   =>
   (printSolution "octopus")
) ; octopusRule

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
   (printSolution "snake")
) ; snakeRule

/*
* Defines the characteristics representative of a lizard. If all these are met, 
* will print that the animal is a lizard.
*/
(defrule lizardRule "Defines the unique characteristics of a standard lizard."
   (canFly no)
   (swimsOften no)
   (warmBlooded no)
   (legs 4)
   (isEaten no)
   =>
   (printSolution "lizard")
) ; lizardRule

/*
* Defines the characteristics representative of a snail. If all these are met, 
* will print that the animal is a snail.
*/
(defrule snailRule "Defines the unique characteristics of a standard snail."
   (canFly no)
   (swimsOften no)
   (warmBlooded ?isWarmBlooded &~yes) ; accounts for potential uncertainty in the snail's bloodedness (unsure or no are both acceptable)
   (legs 0)
   (isEaten yes)
   (hasShell yes)
   =>
   (printSolution "snail")
) ; snailRule

/*
* Defines the characteristics representative of a bat. If all these are met, 
* will print that the animal is a bat.
*/
(defrule batRule "Defines the unique characteristics of a standard bat."
   (canFly yes)
   (swimsOften no)
   (warmBlooded ?isWarmBlooded &~yes) ; accounts for potential uncertainty in the bat's bloodedness (unsure or no are both acceptable)
   (legs 2)
   =>
   (printSolution "bat")
) ; batRule

/*
* Defines the characteristics representative of a bee. If all these are met, 
* will print that the animal is a bee.
*/
(defrule beeRule "Defines the unique characteristics of a standard bee."
   (canFly yes)
   (swimsOften no)
   (warmBlooded ?isWarmBlooded &~yes) ; accounts for potential uncertainty in the bee's bloodedness (unsure or no are both acceptable)
   (legs 6)
   (hasShell no)
   (isMulticolored yes)
   => 
   (printSolution "bee")
) ; beeRule

/*
* Defines the characteristics representative of an ant. If all these are met, 
* will print that the animal is an ant.
*/
(defrule antRule "Defines the unique characteristics of a standard ant."
   (canFly no)
   (swimsOften no)
   (warmBlooded ?isWarmBlooded &~yes) ; accounts for potential uncertainty in the ant's bloodedness (unsure or no are both acceptable)
   (legs 6)
   (isDark yes)
   => 
   (printSolution "ant")
) ; antRule

/*
* Defines the characteristics representative of a spider. If all these are met, 
* will print that the animal is a spider.
*/
(defrule spiderRule "Defines the unique characteristics of a standard spider."
   (canFly no)
   (swimsOften no)
   (warmBlooded ?isWarmBlooded &~yes) ; accounts for potential uncertainty in the spider's bloodedness (unsure or no are both acceptable)
   (legs 8)
   (hasShell no)
   => 
   (printSolution "spider")
) ; spiderRule

/*
* Defines the characteristics representative of a scorpion. If all these are met, 
* will print that the animal is a scorpion.
*/
(defrule scorpionRule "Defines the unique characteristics of a standard scorpion."
   (canFly no)
   (swimsOften no)
   (warmBlooded ?isWarmBlooded &~yes) ; accounts for potential uncertainty in the scorpion's bloodedness (unsure or no are both acceptable)
   (legs 8)
   (hasShell yes)
   => 
   (printSolution "scorpion")
) ; scorpionRule

/*
* Defines the characteristics representative of a sea lion. If all these are met, 
* will print that the animal is a sea lion.
*/
(defrule seaLionRule "Defines the unique characteristics of a standard sea lion."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (legs 0)
   (canSurviveOnLand yes)
   (hasHeadProtrusions no)
   =>
   (printSolution "sea lion")
) ; seaLionRule

/*
* Defines the characteristics representative of a walrus. If all these are met, 
* will print that the animal is a walrus.
*/
(defrule walrusRule "Defines the unique characteristics of a standard walrus."
   (canFly no)
   (swimsOften yes)
   (warmBlooded yes)
   (legs 0)
   (canSurviveOnLand yes)
   (hasHeadProtrusions yes)
   =>
   (printSolution "walrus")
) ; walrusRule


/*
* Requests the user for a response to a given question. If it is valid (starts with either "Y", "N", or "?"), 
* returns the starting character. Otherwise returns FALSE.
*/
(deffunction requestValidatedInput (?questionVal)
   /*
   * Asks the user a question formatted with the question number (1-20) followed by a period and space, followed by the request.
   * For example: 
   * 1. Can the given animal fly?
   */
   (bind ?userInput (askQuestion (str-cat ?*questionNumber* ". " ?questionVal))) 
   (bind ?firstCharacter (lowcase (sub-string 1 1 ?userInput))) ; extracts the first character from the user's response

   (bind ?isYesChar (eq ?firstCharacter ?*VALID_YES_CHARACTER*))
   (bind ?isNoChar (eq ?firstCharacter ?*VALID_NO_CHARACTER*))
   (bind ?isUncertainChar (eq ?firstCharacter ?*VALID_UNCERTAIN_CHARACTER*))
   (bind ?isValid (or ?isYesChar ?isNoChar ?isUncertainChar)) ; valid only if the character starts with "y", "n", or "?", not case-sensitive

   (if ?isValid then (bind ?returnVal ?firstCharacter) ; returns the first character if the input is valid
    else (bind ?returnVal FALSE)                       ; returns FALSE if the input is invalid
   )

   (return ?returnVal)
) ; requestValidatedInput (?questionVal)

/*
* Asks the user whether a given fact is true or false (or if they are unsure). Valid input includes any string starting 
* with "Y" to indicate yes, "N" to indicate no, and "?" to indicate uncertainty, not case-sensitive.
*
* Returns "Y" if the user specified yes, "N" if the user specified no, and "?" if the user specified uncertainty. If the user has exceeded
* the total allowed questions, simply returns an empty string.
*/
(deffunction askForFact (?questionVal)
   (bind ?returnVal "")

   (if (not ?*foundSolution*) then                             ; as long as a solution hasn't been guessed, will ask the question
      (if (> ?*questionNumber* ?*TOTAL_ALLOWED_QUESTIONS*) then ; the user has exceeded the total number of questions: game over!
         (endGame)
         (printline (str-cat "I could not determine the answer within " ?*TOTAL_ALLOWED_QUESTIONS* " questions."))
       else
         (bind ?returnVal (requestValidatedInput ?questionVal))

         (while (eq ?returnVal FALSE) ; while the input is invalid, continually asks for new input until it becomes valid
            (printline ?*INVALID_INPUT_MESSAGE*)
            (bind ?returnVal (requestValidatedInput ?questionVal))
         )

         (++ ?*questionNumber*)
      )
   )

   (return ?returnVal)
) ; askForFact (?questionVal)

/*
* Triggers when no questions remain, meaning the animal could not be guessed.
*/
(defrule noSolution "Warns the user if the animal could not be guessed."
   (declare (salience -50)) ; make the rule a very low salience to guarantee it is fired near the end
   (not (solutionFound))
   =>
   (printline "Sorry! I was unable to determine what animal you were thinking of.")
) ; noSolution

/*
* Triggers when the animal has been guessed, shutting off the rule engine.
*/
(defrule foundSolution "Halts if the solution has been found."
   (solutionFound)
   =>
   (endGame)
) ; foundSolution


/* 
* Prints out the solution to the problem and asserts that the game is complete. If the given animal's name
* starts with a vowel, will print using "an." The solution must be a non-empty string.
*
* For example, if the animal guessed is a spider, will print: The animal is a spider.
* If the animal guessed is an elephant, for example, will print: The animal is an elephant.
*/ 
(deffunction printSolution (?solution)
   (bind ?prefixMessage "The animal is a") ; regardless of whether the animal name starts with a vowel, the message will always start with this prefix

   (if (startsWithVowel ?solution) then (bind ?prefixMessage (str-cat ?prefixMessage "n ")) ; does start with vowel, so change "a" to "an" and add a space
    else (bind ?prefixMessage (str-cat ?prefixMessage " "))                                 ; does not start with a vowel, so simply add a space after "a"
   )

   (printline (str-cat ?prefixMessage ?solution ".")) ; adds a period to the end of the message
   
   (assert (solutionFound))
   (return)
) ; printSolution (?solution)

/*
* Determines whether a given non-empty string value starts with a vowel ("a", "e", "i", "o", or "u", but not "y"), 
* not case-sensitive.
*/
(deffunction startsWithVowel (?string)
   (bind ?firstChar (lowcase (sub-string 1 1 ?string))) ; convert first character to lower case to ignore case
   (return (or (eq ?firstChar "a") (eq ?firstChar "e") (eq ?firstChar "i") (eq ?firstChar "o") (eq ?firstChar "u")))
) ; startsWithVowel (?string)

/*
* Ends the animal game by resetting and stopping the rule engine. This is only to be called if the system guesses the animal successfully
* or if the number of questions is exceeded.
*/
(deffunction endGame ()
   (reset)
   (halt) ; stops the rule engine from running to ensure no more questions are asked
   (return)
) ; endGame ()

/*
* Begins the animal game by clearing out the rule engine and running it. If a solution is not found, lets the user know
* that the system could not guess the animal.
*/
(deffunction playGame ()
   (reset)
   (run)
   (endGame)
   (return)
) ; playGame ()

(playGame) ; begins the game for the user to play