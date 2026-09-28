-- OOP basics, create a constructor; cup has a property of flOz; which can be assigned via the constructor
-- instead of cup.flOz, Haskell uses a different notation "flOz cup" (message being sent, instance of obj receiving message)

-- constructor to create instance of "cup" object
-- cup flOz = \message -> message flOz

-- Using Record allows for much easier building of the object
-- This automatically creates the 'cup' constructor and the 'flOz' getter.
data Cup = Cup { flOz :: Int } deriving (Show)

coffeeCup = Cup 12 -- so 12 gets "assigned" to flOz?

-- no longer needed because now using Record type
--getFlOz cup_inst = cup_inst (\flOz -> flOz)

-- performing a modifying action on the object
checkDiff :: (Num a, Ord a) => a -> a -> a -- all inputs & outputs must be both Num and Ord
checkDiff a b = if (a - b) >= 0 then a - b else 0

drink cup_inst ozDrank = Cup newOz
  where
    current_flOz  = flOz cup_inst
    newOz = checkDiff current_flOz ozDrank

-- this works fine, but remember we cannot overwrite the Record with a new value of flOz since variables (and Records) are immutable in Haskell!
