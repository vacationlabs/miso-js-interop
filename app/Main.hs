{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Miso
import Miso.String as MS
import Data.Monoid((<>))
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal
import Miso.FFI
import Miso.Html.Internal ( Sub )
import Debug.Trace

foreign import javascript unsafe "$($1).datepicker({'onSelect': $(this).trigger('datepicker:onSelect')});"
  initDatePicker :: JSString -> IO ()

customSub :: ((() -> action)) -> Sub action model
customSub f _ = \sink -> do
  windowAddEventListener "datepicker:onSelect" =<< do
    asyncCallback1 $ \customEvent -> do
      sink $ f ()

data Action = NoOp
            | InitDatePicker
              deriving (Eq, Show)

data Model = Model{modelDate :: MisoString} deriving (Eq, Show)

main :: IO ()
main = startApp App
  {
    initialAction = NoOp
  , model=Model{modelDate=""}
  , update = updateModel
  , view = viewModel
  , events = defaultEvents
  , subs = [customSub (\_ -> trace "date selected!" NoOp)]
  }

updateModel :: Action -> Model -> Effect Model Action
updateModel NoOp m = noEff m
updateModel InitDatePicker m = m <# do
  initDatePicker "#mydatepicker"
  pure NoOp

viewModel :: Model -> View Action
viewModel m = div_ []
  [
    div_ []
    [
      text (MS.pack "date will come here" <> (modelDate m))
    , button_ [onClick (InitDatePicker)] [text "click to initialise datepicker"]
    ]
  , div_ [id_ "mydatepicker"] []
  ]

