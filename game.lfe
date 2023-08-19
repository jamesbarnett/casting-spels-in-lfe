(set objects '(whiskey-bottle bucket frog chain))
(set living-room "Super-cool data.")
(set garden "Even cooler data.")
(set attic "The best data evar.")
(set state `(#(living-room ,living-room)
               #(garden ,garden)
               #(attic ,attic)))

(defrecord state
  objects
  places
  player-location
  goals)

(defrecord object
  name
	location)

(set objects
	(list (make-object name 'whiskey-bottle location 'living-room)
				(make-object name 'bucket location 'living-room)
				(make-object name 'frog location 'garden)
				(make-object name 'chain location 'garden)))

(defrecord place
  name
  description
  exits)

(defrecord exit
  direction
  object
  destination)

(set living-room
  (make-place
    name 'living-room
    description (++ "You are in the living-room of a wizard's house. "
                     "There is a wizard snoring on the couch.")
    exits (list
            (make-exit
              direction "west"
              object "door"
              destination 'garden)
            (make-exit
              direction "upstairs"
              object "stairway"
              destination 'attic))))

(set garden
  (make-place
    name 'garden
    description (++ "You are in a beautiful garden. "
                    "There is a well in front of you.")
    exits (list
            (make-exit
              direction "east"
              object "door"
              destination 'living-room))))

(set attic
  (make-place
    name 'attic
    description (++ "You are in the attic of the wizard's house. "
                    "There is a giant welding torch in the corner.")
    exits (list
            (make-exit
              direction "downstairs"
              object "stairway"
              destination 'living-room))))
          
(set netherworld
  (make-place
    name 'netherworld
    description (++ "Everything is misty and vague. "
                    "You seem to be in the netherworld.\n"
                    "There are no exits.\n"
                    "You could be here a long, long time ...")
    exits '()))

(defrecord goal
  name
  achieved?)

(set goals
  (list (make-goal name 'weld-chain achieved? 'false)
        (make-goal name 'dunk-bucket achieved? 'false)
        (make-goal name 'splash-wizard achieved? 'false)))

(set stats (make-state
            objects objects
            places (list living-room garden attic netherworld)
            player-location 'living-room
            goals goals))

