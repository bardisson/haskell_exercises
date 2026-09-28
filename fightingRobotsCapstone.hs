import Distribution.Types.LocalBuildInfo (withNeededTargetsInBuildOrder)
-- create a robot
robot (name, attack, hp) = \message -> message (name, attack, hp) -- all objects can be viewed as a collection of attributes that you can send messages to

-- create instance
killerRobot = robot ("Kill3r", 25, 200)

-- create some helper functions to allow you to easily access elements of the tuple
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

-- now easy to make accessors; no longer need to remember the ordering of the elements in the object tuple
getName robotInst = robotInst name
getAttack robotInst = robotInst attack
getHp robotInst = robotInst hp

-- because the object is more complex, we are going to want to make udpate setters
setName robotInst newName = robotInst (\(n, a, h) -> robot (newName, a, h))
setAttack robotInst newAttack = robotInst (\(n, a, h) -> robot (n, newAttack, h))
setHp robotInst newHp = robotInst (\(n, a, h) -> robot (n, a, newHp))

-- the setters above allow us to perform prototype based OOP
printRobot robotInst = robotInst (\(n, a, h) -> n ++ " attack: " ++ (show a) ++ " health: " ++ (show h) )

-- now, make them fight!
damage robotInst attackDamage = robotInst (\(n, a, h) -> robot (n, a, h - attackDamage))

-- see how "afterHit" is now a new robot instance
{-  
  λ> afterHit = damage killerRobot 90
  λ> getHp afterHit
 -}

-- now to make multiple robots fight -- applies damage from the attacker to the defender
fight robotInst defender = damage defender attack -- recall that defender here is itself a robotInst
  where attack =  if getHp robotInst > 10
                  then getAttack robotInst
                  else 0

-- now construct a contender
gentleRobot = robot ("GentleBot", 10, 300)

-- multiple fight rounds
gentleRound1 = fight killerRobot gentleRobot
killerRound1 = fight gentleRobot killerRobot
gentleRound2 = fight killerRound1 gentleRound1
killerRound2 = fight gentleRound1 killerRound1
gentleRound3 = fight killerRound2 gentleRound2
killerRound3 = fight gentleRound2 killerRound2

{-
 - Haskell is stateless -- and because of this, it doesn't matter in which order the above sequences happen when determining a winner
 - of the fight. Because of this, sequencing/ordering also does not matter in Haskell becuase we control exactly when and how a state
 - is modeled. Rearranging the ordering of the above functions has no impact in which robot wins the fight!
 - -}
