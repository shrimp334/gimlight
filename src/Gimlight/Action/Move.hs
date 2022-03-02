module Gimlight.Action.Move
    ( moveAction
    ) where

import           Control.Monad.State         (execStateT)
import           Control.Monad.Writer        (tell)
import           Gimlight.Action             (Action,
                                              ActionResult (ActionResult),
                                              ActionStatus (Failed, Ok))
import           Gimlight.Dungeon.Map.Cell   (Error (ActorAlreadyExists, OutOfRange, TileIsNotWalkable),
                                              locateActorAt, removeActorAt)
import qualified Gimlight.Localization.Texts as T
import           Linear.V2                   (V2)

moveAction :: V2 Int -> Action
moveAction offset position tiles cm =
    case result of
        Right x                     -> return $ ActionResult Ok x []
        Left (ActorAlreadyExists _) -> cannotMove
        Left TileIsNotWalkable      -> cannotMove
        Left OutOfRange             -> cannotMove
        _                           -> error "Unreachable."
  where
    result =
        flip execStateT cm $ do
            a <- removeActorAt position
            locateActorAt tiles a (position + offset)
    cannotMove = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed cm []
