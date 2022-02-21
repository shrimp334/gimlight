{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Actor.Status.Experience
    ( Experience
    , experience
    , getCurrentExperiencePoint
    , getLevel
    , gainExperience
    , pointForNextLevel
    ) where

import           GHC.Generics (Generic)

data Experience =
    Experience
        { point :: Int
        , level :: Int
        }
    deriving (Show, Ord, Eq, Generic)

experience :: Experience
experience = Experience 0 1

getCurrentExperiencePoint :: Experience -> Int
getCurrentExperiencePoint Experience {point = p} = p

getLevel :: Experience -> Int
getLevel Experience {level = l} = l

gainExperience :: Int -> Experience -> (Int, Experience)
gainExperience n Experience {point = p, level = l} =
    levelUp Experience {point = p + n, level = l}

levelUp :: Experience -> (Int, Experience)
levelUp e@Experience {point = p, level = l} =
    if p >= pointForNextLevel e
        then (\(a, b) -> (a + 1, b)) $
             levelUp Experience {point = p - pointForNextLevel e, level = l + 1}
        else (0, e)

pointForNextLevel :: Experience -> Int
pointForNextLevel Experience {level = l} = l
