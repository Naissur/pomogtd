module Common.EventUtils where

import Html exposing (Attribute)
import Html.Events exposing (on, onSubmit, onWithOptions, targetValue)
import Json.Decode exposing (succeed)
import Signal exposing(Address)

onSubmitPreventDefault : Address a -> a -> Attribute
onSubmitPreventDefault address value =
    onWithOptions
      "submit"
      { preventDefault = True, stopPropagation = False }
      (succeed 0)
      (\_ -> Signal.message address value)


onClickWithPreventDefault : Address a -> a -> Attribute
onClickWithPreventDefault address value =
    onWithOptions
      "click"
      { preventDefault = True, stopPropagation = False }
      (succeed 0)
      (\_ -> Signal.message address value)


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))
