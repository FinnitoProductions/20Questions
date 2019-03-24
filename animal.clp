/*
* Finn Frankis
* March 16, 2019
*
* Plays "20 Questions" with the user where the user thinks of an animal (out of a given list of options) and
* the program will guess the animal by asking a series of yes/no questions (no more than 20).
*/

(clear)

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

(deffunction setup ()
   (printline 
"Welcome to the Think of an Animal Game!
Choose one of the following animals: dolphin, dog, snake, elephant, sea lion,
penguin, bee, camel, pig, zebra, bear, monkey, snail, armadillo, shrimp, parrot, water buffalo,
bat, and tortoise. I will ask you a series of questions about your animal, not exceeding 20 questions.
I will use the information from these questions to guess which animal you are thinking of once I have 
enough information. Good luck!"
   )
   (return)
)
               
/*
* Asks the user whether the animal they are thinking of is land-based. Triggers when the system
* needs to determine whether the animal is land-based to narrow down the possibilities of the given animal.
*/
(defrule askLandBased "Ask if the animal the user is thinking of lives on land."
   (need-landBased ?)
   =>
   (bind ?userResponse (askForFact "Is the given animal land-based"))
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
   (bind ?legOptions (create$ 4 2 0))
   (bind ?didSucceed FALSE) ; whether or not the user has answered YES or UNSURE to any of the leg questions

   (foreach ?option ?legOptions
      (if (not ?didSucceed) then
         (bind ?userResponse (askForFact (str-cat "Does the given animal have " ?option " legs")))

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
* Defines the characteristics representative of a dolphin. If all these are met, 
* will print that the animal is a dolphin.
*/
(defrule dolphinRule "Defines the unique characteristics of a standard dolphin."
   (landBased no)
   (warmBlooded yes)
   (canSurviveOnLand no)
   =>
   (printout t "The animal is a dolphin." crlf)
)

/*
* Defines the characteristics representative of a dog. If all these are met, 
* will print that the animal is a dog.
*/
(defrule dogRule "Defines the unique characteristics of a standard dog."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   =>
   (printout t "The animal is a dog." crlf)
)

/*
* Defines the characteristics representative of a tortoise. If all these are met, 
* will print that the animal is a tortoise.
*/
(defrule tortoiseRule "Defines the unique characteristics of a standard tortoise."
   (landBased no)
   (warmBlooded no)
   (canSurviveOnLand yes)
   =>
   (printout t "The animal is a tortoise." crlf)
)

/*
* Defines the characteristics representative of a shrimp. If all these are met, 
* will print that the animal is a snake.
*/
(defrule shrimpRule "Defines the unique characteristics of a standard shrimp."
   (landBased no)
   (warmBlooded no)
   (canSurviveOnLand no)
   =>
   (printout t "The animal is a shrimp." crlf)
)

/*
* Defines the characteristics representative of a snake. If all these are met, 
* will print that the animal is a snake.
*/
(defrule snakeRule "Defines the unique characteristics of a standard snake."
   (landBased yes)
   (warmBlooded no)
   (legs 0)
   =>
   (printout t "The animal is a snake." crlf)
)

/*
* Defines the characteristics representative of a elephant. If all these are met, 
* will print that the animal is a elephant.
*/
(defrule elephantRule "Defines the unique characteristics of a standard elephant."
   (landBased yes)
   (warmBlooded yes)
   (legs 4)
   =>
   (printout t "The animal is a elephant." crlf)
)

/*
* Defines the characteristics representative of a elephant. If all these are met, 
* will print that the animal is a elephant.
*/
(defrule seaLionRule "Defines the unique characteristics of a standard sea lion."
   (landBased no)
   (warmBlooded yes)
   (canSurviveOnLand yes)
   =>
   (printout t "The animal is a sea lion." crlf)
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

(setup)
(reset)
(run)
(return)