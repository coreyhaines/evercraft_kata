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

abilityModifier :: Int -> Int
abilityModifier = halfOf . minusTen
                  where halfOf = flip div 2
                        minusTen = flip (-) 10

scoreForAbility :: (Abilities -> Int) -> Character -> Int
scoreForAbility f c = f $ abilities c

modifierForAbility :: (Abilities -> Int) -> Character -> Int
modifierForAbility f c = abilityModifier $ scoreForAbility f c

conModifier :: Character -> Int
conModifier = modifierForAbility constitution

strModifier :: Character -> Int
strModifier = modifierForAbility strength

dexModifier :: Character -> Int
dexModifier = modifierForAbility dexterity

maxHitpoints :: Character -> Int
maxHitpoints character = max 1 hp
  where hp = baseHitpoints + conModifier character + levelModifier
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
currentLevel = (1 +) . levelsOfExperience
  where levelsOfExperience = expPerLevel . currentExperience
        expPerLevel = flip div levelLedge

modifiedAttackRoll :: Character -> Roll -> Roll
modifiedAttackRoll character originalRoll = originalRoll + strModifier character + levelModifier
  where levelModifier = currentLevel character `div` 2

armorClass :: Character -> Int
armorClass character = baseArmorClass + dexModifier character

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
rawDamageForAttack character roll = amount + strModifier character * maybeCritDmgAmount
    where
  amount
    | isCriticalHit roll = baseCriticalDamage
    | otherwise = baseNoncriticalDamage
  maybeCritDmgAmount = if isCriticalHit roll
                       then 2
                       else 1

damageForAttack :: Character -> Roll -> Damage
damageForAttack = (max 1 .) . rawDamageForAttack

runAttack :: Character -> Character -> Roll -> AttackResult
runAttack attacker defender roll
  | attackIsSuccessful attacker defender roll = AttackResult { player=new_attacker
                                                             , opponent=new_opponent }
  | otherwise = AttackResult { player=attacker
                             , opponent=defender
                             }
    where new_attacker = addExperience baseExperienceForAttack attacker
          new_opponent = addDamage (damageForAttack attacker roll) defender
