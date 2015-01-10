module EverCraft where

type Roll = Int
type Damage = Int

data Alignment =  Good | Evil | Neutral
                  deriving Show

data Abilities = Abilities { strength
                           , dexterity
                           , constitution
                           , wisdom
                           , intelligence
                           , charisma::Int
                           }
                  deriving Show

data AttackResult = AttackResult { player
                                 , opponent::Character
                                 }
                    deriving Show

data Character = Character { name::String
                           , alignment::Alignment
                           , experience::Int
                           , damage::Damage
                           , abilities::Abilities
                           }
                  deriving Show

defaultAbilities :: Abilities
defaultAbilities = Abilities { strength=10
                             , dexterity=10
                             , constitution=10
                             , wisdom=10
                             , intelligence=10
                             , charisma=10 }

defaultCharacter :: Character
defaultCharacter = Character { name=""
                             , alignment=Neutral
                             , experience=0
                             , damage=0
                             , abilities=defaultAbilities
                             }
newAbilities :: Abilities
newAbilities = defaultAbilities

baseHitpoints :: Int
baseHitpoints = 5

baseArmorClass :: Int
baseArmorClass = 10

baseExperienceForAttack :: Int
baseExperienceForAttack = 10

newCharacter :: Character
newCharacter = defaultCharacter

maxHitpoints :: Character -> Int
maxHitpoints character = if hp < 1 then 1 else hp
  where hp = baseHitpoints + abilityModifier (constitution $ abilities character) + levelModifier
        levelModifier = 5 * (currentLevel character - 1)

currentHitpoints :: Character -> Int
currentHitpoints character = maxHitpoints character - damage character

currentExperience :: Character -> Int
currentExperience = experience

addExperience :: Int -> Character -> Character
addExperience amount character = character{experience= currentExperience character + amount}

levelLedge :: Int
levelLedge = 1000

currentLevel :: Character -> Int
currentLevel character = 1 + currentExperience character `div` levelLedge

abilityModifier :: Int -> Int
abilityModifier abilityScore = (abilityScore - 10) `div` 2

modifiedAttackRoll :: Character -> Roll -> Roll
modifiedAttackRoll character originalRoll = originalRoll + abilityModifier (strength $ abilities character) + levelModifier
  where levelModifier = currentLevel character `div` 2

armorClass :: Character -> Int
armorClass character = baseArmorClass + abilityModifier (dexterity $ abilities character)

addDamage :: Damage -> Character -> Character
addDamage amount character = character {damage=damage character + amount}

isAlive :: Character -> Bool
isAlive character = currentHitpoints character > 0


criticalRoll :: Int
criticalRoll = 20

baseNoncriticalDamage :: Int
baseNoncriticalDamage = 1

baseCriticalDamage :: Int
baseCriticalDamage = 2

isCriticalHit :: Roll -> Bool
isCriticalHit = (==) criticalRoll

attackIsSuccessful :: Character -> Character -> Roll -> Bool
attackIsSuccessful attacker defender roll = modifiedAttackRoll attacker roll >= armorClass defender

rawDamageForAttack :: Character -> Roll -> Damage
rawDamageForAttack character roll = amount + abilityModifier (strength $ abilities character) * if isCriticalHit roll then 2 else 1
    where
  amount
    | isCriticalHit roll = baseCriticalDamage
    | otherwise = baseNoncriticalDamage

damageForAttack :: Character -> Roll -> Damage
damageForAttack character roll = if totalDamage >= 1
                                 then totalDamage
                                 else 1
  where totalDamage = rawDamageForAttack character roll

runAttack :: Character -> Character -> Roll -> AttackResult
runAttack attacker defender roll
  | attackIsSuccessful attacker defender roll = AttackResult { player=new_attacker
                                                             , opponent=new_opponent }
  | otherwise = AttackResult { player=attacker
                             , opponent=defender
                             }
    where new_attacker = addExperience baseExperienceForAttack attacker
          new_opponent = addDamage (damageForAttack attacker roll) defender
