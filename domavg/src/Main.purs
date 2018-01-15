module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import DOM (DOM)
import DOM.Node.Types (ElementId(..), Element, elementToEventTarget)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types(HTMLInputElement)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.Node.Node (textContent, setTextContent)
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Classy.ParentNode (querySelector)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event(..), EventType(..), EventTarget(..))
import DOM.Classy.Element (getAttribute, setAttribute, replaceChild, firstChild, setTextContent)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce as U

doStuff ::  (Maybe Element) -> Number
doStuff (Just el) = 1.0
doStuff _ = 0.0


elementToHTMLInputElement :: Element -> HTMLInputElement
elementToHTMLInputElement = U.unsafeCoerce

getValue :: forall e. (Maybe Element) -> Eff (dom ::DOM | e) (Maybe String)
getValue (Just el) = getAttribute "value" el
getValue Nothing = pure Nothing


getValueNum :: forall e. (Maybe Element) -> Eff (dom :: DOM | e) Number
getValueNum (Just el) =
  do
    valStr <- value (elementToHTMLInputElement el)
    pure (fromMaybe 0.0 (fromString valStr))
getValueNum _ = pure 0.0


gv :: forall e. (Maybe Element) -> Eff (dom :: DOM | e) Number
gv (Just el) =
  value (elementToHTMLInputElement el) >>=
  \valStr -> 
      pure (fromMaybe 0.0 (fromString valStr))
gv _ = pure 0.0


respondToClick :: forall e. Event -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
respondToClick evt = do
  w <- window
  d <- document w
  b <- body d

  input1 <- case b of
    Just el -> querySelector (QuerySelector "#input1") el
    Nothing -> pure Nothing
  x <- gv input1
  logShow x
  pure unit

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  w <- window
  d <- document w
  b <- body d

  button <- case b of
    Just el -> querySelector (QuerySelector "#calculate") el
    Nothing -> pure Nothing
  case  button of
    Just b -> addEventListener (EventType "click")
                (eventListener respondToClick)
                false (elementToEventTarget b)
    Nothing -> pure unit
