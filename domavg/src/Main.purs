module Main where

import Prelude (Unit, bind, pure, show, unit, (+), (/))
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import DOM (DOM)
import DOM.Node.Types (Element, elementToEventTarget)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (HTMLInputElement)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.Classy.Node (setTextContent)
import DOM.Classy.ParentNode (querySelector)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event, EventType(EventType))
import Unsafe.Coerce as U

-- Needed in order to use the `value` function  
elementToHTMLInputElement :: Element -> HTMLInputElement
elementToHTMLInputElement = U.unsafeCoerce

getElementBySelector :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElementBySelector selector =
  do
    w <- window
    d <- document w
    bodyElement <- body d
    result <- case bodyElement of
      Just b ->
        querySelector (QuerySelector selector) b
      Nothing -> pure Nothing
    pure result
      
{-
  Given a selector that identifies an HTML input field, return its
  value as a `Maybe Number`
-}
getNumericValue :: forall e. String -> Eff (dom :: DOM, console :: CONSOLE | e) (Maybe Number)
getNumericValue id =
  do
    selected <- getElementBySelector id
    
    case selected of
      Just inputElement -> 
        do
          str <- (value (elementToHTMLInputElement inputElement))
          pure (fromString str)
      Nothing -> pure Nothing
  
{-
  Handle click on the Calculate button by:
  * getting values from the two input fields
  * calculating the average
  * showing the result in the `<span id="#output">`
-}
respondToClick :: forall e. Event -> Eff (console :: CONSOLE, dom :: DOM | e) Unit
respondToClick evt = do
  value1 <- getNumericValue "#input1"
  value2 <- getNumericValue "#input2"
  let avg = lift2 (/) (lift2 (+) value1 value2) (Just 2.0)
  let avgText =
        case avg of
          Just result -> (show result)
          Nothing -> ""
  textSpan <- getElementBySelector "#output"
  case textSpan of
    Just tspan ->
      setTextContent avgText tspan
    Nothing ->
      pure unit

{-
  Add event listener to the Calculate button
-}
main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  button <- getElementBySelector "#calculate"
  case button of
    Just btn -> addEventListener (EventType "click")
                (eventListener respondToClick)
                false (elementToEventTarget btn)
    Nothing -> pure unit
