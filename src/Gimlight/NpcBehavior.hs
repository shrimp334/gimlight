module Gimlight.NpcBehavior
    ( handleNpcTurns
    ) where

import           Control.Arrow               ((&&&))
import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.State         (StateT (runStateT), evalStateT,
                                              execStateT)
import           Control.Monad.Writer        (MonadWriter (writer), Writer)
import           Data.Array                  ((!))
import           Data.Foldable               (find)
import           Data.Maybe                  (fromMaybe)
import           Gimlight.Action             (Action, ActionResult (killed),
                                              newCellMap)
import           Gimlight.Action.Melee       (meleeAction)
import           Gimlight.Action.Move        (moveAction)
import           Gimlight.Action.Wait        (waitAction)
import           Gimlight.Actor              (Actor, getIndex, isFriendlyNpc,
                                              isPlayer, pathToDestination,
                                              target)
import           Gimlight.Coord              (Coord)
import           Gimlight.Data.Either        (expectRight)
import           Gimlight.Dungeon.Map.Cell   (CellMap, locateActorAt,
                                              positionsAndActors, removeActorAt,
                                              removeActorIf, transparentMap)
import           Gimlight.Dungeon.Map.Tile   (TileCollection)
import           Gimlight.Dungeon.PathFinder (getPathTo)
import           Gimlight.Fov                (calculateFov)
import           Gimlight.Log                (MessageLog)
import           Linear.V2                   (V2 (V2))

handleNpcTurns ::
       TileCollection -> CellMap -> Writer MessageLog (CellMap, [Actor])
handleNpcTurns ts cm =
    foldl foldStep (writer ((cm, []), [])) . filter (not . isPlayer . snd) $
    positionsAndActors cm
  where
    foldStep acc (position, _) = do
        (d', l) <- acc
        (newD, newLog) <- npcAction position ts d'
        return (newD, newLog ++ l)

npcAction ::
       Coord
    -> TileCollection
    -> CellMap
    -> Writer MessageLog (CellMap, [Actor])
npcAction position ts cm =
    case flip runStateT cm $ removeActorAt position of
        Right (actor, cellMapWithoutActor) ->
            npcActionFor actor cellMapWithoutActor
        _ -> return (cm, [])
  where
    npcActionFor a cellMapWithoutActor
        | isFriendlyNpc a = return (cm, [])
        | otherwise =
            (newCellMap &&& killed) <$>
            action a position ts (cellMapAfterUpdatingMap a cellMapWithoutActor)
    action a = selectAction position (entityAfterUpdatingMap a) cm
    cellMapAfterUpdatingMap a cellMapBeforeUpdating =
        case flip execStateT cellMapBeforeUpdating $
             locateActorAt ts (entityAfterUpdatingMap a) position of
            Right x -> x
            Left e  -> error $ "Failed to locate an actor: " <> show e
    entityAfterUpdatingMap a =
        fromMaybe
            (entityAfterUpdatingTarget a)
            (updatePath position (entityAfterUpdatingTarget a) ts cm)
    entityAfterUpdatingTarget = updateTarget position ts cm

updateTarget :: Coord -> TileCollection -> CellMap -> Actor -> Actor
updateTarget srcPosition ts cm a
    | currentTargetIsInFov = a
    | otherwise = a & target .~ (getIndex <$> nextTarget)
  where
    currentTargetIsInFov =
        case currentTarget of
            Right x -> isInFov x
            _       -> False
    indexToPosition x =
        fst <$> find ((getIndex x ==) . getIndex . snd) (positionsAndActors cm)
    currentTarget =
        flip evalStateT cm $
        removeActorIf (\x -> a ^. target == Just (getIndex x))
    nextTarget
        | null otherActorsInFov = Nothing
        | otherwise = Just $ head otherActorsInFov
    otherActorsInFov = filter isInFov otherActors
    fov = calculateFov srcPosition (transparentMap ts cm)
    isInFov x = maybe False (fov !) (indexToPosition x)
    otherActors =
        map snd $ filter (\(x, _) -> x /= srcPosition) $ positionsAndActors cm

updatePath :: Coord -> Actor -> TileCollection -> CellMap -> Maybe Actor
updatePath src e ts cm
    | isTargetInFov src ts cm e = Just $ e & pathToDestination .~ newPath
    | otherwise = Nothing
  where
    newPath = fromMaybe [] $ dst >>= getPathTo ts cm src
    dst = getTargetPosition e cm

selectAction :: Coord -> Actor -> CellMap -> Action
selectAction position e cm
    | targetIsNextTo position e cm =
        case offsetToTarget position e cm of
            Just offset -> meleeAction offset
            Nothing     -> moveOrWait e
    | otherwise = moveOrWait e

moveOrWait :: Actor -> Action
moveOrWait e
    | null $ e ^. pathToDestination = waitAction
    | otherwise = popPathToDestinationAndMove

popPathToDestinationAndMove :: Action
popPathToDestinationAndMove position tc cm =
    moveAction moveTo position tc newMap
  where
    (moveTo, newMap) =
        expectRight "No such actor." $
        flip runStateT cm $ do
            a <- removeActorAt position
            let path = a ^. pathToDestination
                (next, remaining) = (head path, tail path)
                offset = next - position
                updatedActor = a & pathToDestination .~ remaining
            locateActorAt tc updatedActor position
            return offset

targetIsNextTo :: Coord -> Actor -> CellMap -> Bool
targetIsNextTo position e cm =
    case distance of
        Just d' -> d' <= 1
        Nothing -> False
  where
    distance =
        (\(V2 x y) -> max (abs x) (abs y)) <$> offsetToTarget position e cm

offsetToTarget :: Coord -> Actor -> CellMap -> Maybe (V2 Int)
offsetToTarget position e cm = subtract position <$> getTargetPosition e cm

isTargetInFov :: Coord -> TileCollection -> CellMap -> Actor -> Bool
isTargetInFov position ts cm actor =
    ((calculateFov position (transparentMap ts cm) !) <$>
     getTargetPosition actor cm) ==
    Just True

getTargetPosition :: Actor -> CellMap -> Maybe Coord
getTargetPosition actor =
    fmap fst .
    find (\(_, other) -> actor ^. target == Just (getIndex other)) .
    positionsAndActors
