module Gimlight.Action.PickUp
    ( pickUpAction
    ) where

import           Control.Lens                ((%%~), (&))
import           Control.Monad.State         (StateT (runStateT))
import           Control.Monad.Writer        (tell)
import           Data.Either.Combinators     (maybeToRight)
import           Gimlight.Action             (Action,
                                              ActionResult (ActionResult),
                                              ActionStatus (Failed, Ok))
import           Gimlight.Actor              (inventoryItems)
import           Gimlight.Dungeon.Map.Cell   (Error (ItemNotFound),
                                              locateActorAt, removeActorAt,
                                              removeItemAt)
import           Gimlight.Inventory          (addItem)
import           Gimlight.Item.SomeItem      (getName)
import qualified Gimlight.Localization.Texts as T

pickUpAction :: Action
pickUpAction position tc cm =
    case result of
        Right (Right name, newMap) -> do
            tell [T.youGotItem name]
            return $ ActionResult Ok newMap []
        Right (Left l, _) -> do
            tell [l]
            return $ ActionResult Failed cm []
        Left ItemNotFound -> do
            tell [T.youGotNothing]
            return $ ActionResult Failed cm []
        _ -> error "Unreachable."
  where
    result =
        flip runStateT cm $ do
            a <- removeActorAt position
            i <- removeItemAt position
            let newActor =
                    maybeToRight T.bagIsFull $ a & inventoryItems %%~ addItem i
            case newActor of
                Right x -> do
                    locateActorAt tc x position
                    return $ Right $ getName i
                Left x -> return $ Left x
