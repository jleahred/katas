module Calculator2 where


import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
--import Graphics.Collage exposing (..)
--import Html
import Text
--import Markdown
import Signal exposing (..)




-- MODEL --------------------------------------------------------

model : Signal Model
model =
    foldp update initModel keysMailBox.signal


type alias Model =
    { display:          Display
    --, lastOperation:    LastOperation
    , result:           Float
    }

initModel: Model
initModel =
    { display       = DispInput initInput
    --, lastOperation = Full (0.0, Sum)
    , result        = 0.0
    }

type Display
    = DispInput     Input
    | DispResult    LastOperation

type alias Input =
    { text:     String
    , value:    Float
    , dot:      Bool
    , digits:   Int
    , pow10Dec: Float
    , op:       Operator
    }


type alias LastOperation = (Operator, Float)


type Operator
    = Sum
    | Subs
    | Mult
    | Div



initInput: Input
initInput =
    { text      = "0"
    , value     = 0.0
    , dot       = False
    , digits    = 0
    , pow10Dec  = 1.0
    }


resetInput: Model -> Model
resetInput model =
    { model
    | display  = DispInput initInput
    }





-- UPDATE --------------------------------------------------------

type Action
    = PressDigit    Digit
    | Operation     Operator
    | Equal
    | Dot
    | Clear
    | Reset

type Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9


update : Action -> Model -> Model
update keyEvent model =
    case keyEvent of
        PressDigit d    -> addDigit     model d
        Dot             -> addDot       model
        Operation  op   -> addOperator  model op
        Clear           -> clear        model
        Reset           -> initModel
        Equal           -> resolve      model


keysMailBox : Signal.Mailbox Action
keysMailBox =
    Signal.mailbox Clear



-- VIEW --------------------------------------------------------
calculatorView : a -> Model -> Element
calculatorView address model =
    -- collage 600 600 <| [move (10, 10) <| toForm <|
    flow down
    [ getDisplay model
    , flow right [ btAct "Clear" Clear, btAct "Reset" Reset]
    , flow right [ btDigit D7, btDigit D8, btDigit D9, btAct "+" <| Operation Sum]
    , flow right [ btDigit D4, btDigit D5, btDigit D6, btAct "-" <| Operation Subs]
    , flow right [ btDigit D1, btDigit D2, btDigit D3, btAct "x" <| Operation Mult]
    , flow right [ btDigit D0
                 , btAct "." Dot
                 , btAct "=" Equal
                 , btAct "/" <| Operation Div ]
    , show model
    ]

btDigit: Digit -> Element
btDigit d =
    btAct (digitToString d) (PressDigit d)

btAct: String -> Action -> Element
btAct txt action =
    button (Signal.message keysMailBox.address action) txt




main : Signal Element
main =
    map (calculatorView  keysMailBox.signal) model


-- SUPPORT ----------------------------------------------------
digitToString: Digit -> String
digitToString d
    = case d of
        D0 -> "0"
        D1 -> "1"
        D2 -> "2"
        D3 -> "3"
        D4 -> "4"
        D5 -> "5"
        D6 -> "6"
        D7 -> "7"
        D8 -> "8"
        D9 -> "9"

toInt: Digit -> Int
toInt d
    = case d of
        D0 -> 0
        D1 -> 1
        D2 -> 2
        D3 -> 3
        D4 -> 4
        D5 -> 5
        D6 -> 6
        D7 -> 7
        D8 -> 8
        D9 -> 9


addDot: Model -> Model
addDot model =
    case model.display of
        DispInput  input    ->
            { model
            | display =
                DispInput{ input
                | dot   = True
                , text  = input.text ++ "."
                }
            }
        DispResult              ->
            { model
            | display =
                DispInput { initInput
                | dot   = True
                , text  = "0."
                }
            }


addOperator: Model -> Operator -> Model
addOperator model op =
    { model
    | display       = DispInput initInput
    , lastOperation = Partial op
    }

clear: Model -> Model
clear model =
    resetInput model

addDigit: Model -> Digit -> Model
addDigit model digit =
    case model.display of
        DispResult          ->
            { model
            | display =
                DispInput { initInput
                | dot   = False
                , text  = digitToString digit
                }
            }
        DispInput   input   ->
            if input.digits < 10 then
                { model
                | display = DispInput (addDigitInput digit input)
                }
            else
                model
addDigitInput: Digit -> Input -> Input
addDigitInput digit input =
    if input.digits < 10 then
      let
        newValueDeinputmals digit input =
            if input.dot == False then
                ( input.value * 10 + (toFloat <| toInt digit)
                , input.pow10Dec
                )
            else
                ( input.value + (toFloat <| toInt digit)*input.pow10Dec/10
                , input.pow10Dec / 10
                )
      in
          if input.text == "0" then
            { input
            | text   = digitToString digit
            , digits = 1
            , value  = toFloat <| toInt digit
            }
          else
              let (val, pow10Dec) = newValueDeinputmals digit input
              in
                { input
                | text      = input.text ++ digitToString digit
                , digits    = input.digits + 1
                , value     = val
                , pow10Dec  = pow10Dec
                }
    else
        input

resolve: Model -> Model
resolve model =
    let
        dispValue model =
            case model.display of
                DispResult          -> 0.0
                DispInput   input   -> input.value
    in
        case model.lastOperation of
            Full (val, op)  ->
                { model
                | result = model.result + val
                }
            Partial op      ->
                { model
                | result = model.result + (dispValue model)
                }

getDisplay: Model -> Element
getDisplay model =
    let
        dispTxt: Model -> String
        dispTxt model =
            case model.display of
                DispResult      ->  toString model.result
                DispInput input ->  input.text
    in
        Text.fromString (dispTxt model)
            |> rightAligned
            |> width  ((widthOf <| btDigit D9) * 3)
