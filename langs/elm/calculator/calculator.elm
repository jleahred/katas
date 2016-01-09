module Calculator where


import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Html
import Markdown
import Text
import Signal exposing (..)




-- MODEL --------------------------------------------------------

model : Signal Model
model =
    foldp update (ModelInput initInput) keysMailBox.signal

type Model
    = ModelInput                Input
    | ModelResultInOperation    CurrentOperation
    | ModelResult               Result


type alias Input =
    { text:             String
    , value:            Float
    , dot:              Bool
    , digits:           Int
    , pow10Dec:         Float
    , currentOperation: CurrentOperation
    }

type alias Result =
    { value:         Float
    , lastOperation: LastOperation
    }



type Operator
    = Sum
    | Subs
    | Mult
    | Div


type alias CurrentOperation  =
    (Float, Operator) -- result or first operand and Operator

type alias LastOperation     =
    (Operator, Float)



initInput: Input
initInput =
    { text              = "0"
    , value             = 0.0
    , dot               = False
    , digits            = 0
    , pow10Dec          = 1.0
    , currentOperation  = (0.0, Sum)
    }


clearInput: Model -> Model
clearInput model =
    case model of
        ModelInput input ->
            ModelInput  { initInput
                        | currentOperation = input.currentOperation
                        }
        ModelResult _ ->
            model
        ModelResultInOperation _ ->
            model



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
        Clear           -> clear        model
        Reset           -> ModelInput   initInput
        Operation  op   -> addOperator  model op
        Equal           -> calculate    model

keysMailBox : Signal.Mailbox Action
keysMailBox =
    Signal.mailbox Clear



-- VIEW --------------------------------------------------------
calculatorView : a -> Model -> Html.Html
calculatorView address model =
    let
    calculator: Model -> Element
    calculator model =
        flow down
        [ getDisplay model
        , flow right [ btAct "Reset" Reset, btAct "Clear" Clear]
        , flow right [ btDigit D7, btDigit D8, btDigit D9, btAct "+" <| Operation Sum]
        , flow right [ btDigit D4, btDigit D5, btDigit D6, btAct "-" <| Operation Subs]
        , flow right [ btDigit D1, btDigit D2, btDigit D3, btAct "x" <| Operation Mult]
        , flow right [ btDigit D0
                     , btAct "." Dot
                     , btAct "=" Equal
                     , btAct "/" <| Operation Div ]
        --, show model
        ]
    in
    Html.div[][ Markdown.toHtml """
# Simple calculator

This is my very first application in [elm](http://elm-lang.org/) language


    """
    , Html.br[][]     , Html.br[][]
    , Html.fromElement  <|  calculator model
    , Html.br[][]
    , Markdown.toHtml """

It has been an amazing experience.

More information, source and comments [here](https://github.com/jleahred/katas/tree/master/langs/elm/calculator)
"""    ]


btDigit: Digit -> Element
btDigit d =
    btAct (digitToString d) (PressDigit d)

btAct: String -> Action -> Element
btAct txt action =
    button (Signal.message keysMailBox.address action) txt




-- Let the show starts

main : Signal Html.Html
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

digitToInt: Digit -> Int
digitToInt d
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
    let
        modelZeroDot =
                        { text              = "0."
                        , value             = 0.0
                        , dot               = True
                        , digits            = 1
                        , pow10Dec          = 10.0
                        , currentOperation  = (0.0, Sum)
                        }
        setCurrentOperation currOper input =
            { input
            | currentOperation = currOper
            }
    in
    case model of
        ModelResult result       ->
            ModelInput modelZeroDot
        ModelResultInOperation result       ->
            ModelInput <| (modelZeroDot |> setCurrentOperation result)
        ModelInput  input    ->
            if input.dot == False then
                if input.digits == 0 then
                    ModelInput { input
                               | dot   = True
                               , text  = "0."
                               , digits = 1
                               }
                else
                    ModelInput { input
                               | dot   = True
                               , text  = input.text ++ "."
                               }
            else
                model


addOperator: Model -> Operator -> Model
addOperator model op =
    let
        addCurrentOperation: Input -> Operator -> Float -> Input
        addCurrentOperation input operator operand =
            { input
            | currentOperation = (operand, operator)
            }
    in
    case model of
        ModelResult result       ->
            ModelResultInOperation  (result.value, op)
        ModelResultInOperation  result   ->
            ModelResultInOperation  (fst result, op)
        ModelInput  input    ->
            ModelInput <| addCurrentOperation initInput op <| operate
                                                    (fst input.currentOperation)
                                                    (snd input.currentOperation)
                                                    input.value

operate: Float -> Operator -> Float -> Float
operate op1 operator op2 =
    case operator of
        Sum   -> op1 + op2
        Subs  -> op1 - op2
        Mult  -> op1 * op2
        Div   -> op1 / op2


clear: Model -> Model
clear model =
    clearInput model

addDigit: Model -> Digit -> Model
addDigit model digit =
     case model of
        ModelInput   input   ->
            if input.digits < 10 then
                addDigitInput digit input
            else
                model
        ModelResult  result    ->
            ModelInput  { text  = digitToString digit
                        , value = digitToInt digit |> toFloat
                        , dot               = False
                        , digits            = 1
                        , pow10Dec          = 1.0
                        , currentOperation  = (0.0, Sum)
                        }
        ModelResultInOperation  result    ->
            ModelInput  { text  = digitToString digit
                        , value = digitToInt digit |> toFloat
                        , dot               = False
                        , digits            = 1
                        , pow10Dec          = 1.0
                        , currentOperation  = (fst result, snd result)
                        }



addDigitInput: Digit -> Input -> Model
addDigitInput digit input =
    if input.digits < 10 then
      let
        newValueDeinputmals digit input =
            if input.dot == False then
                ( input.value * 10 + (toFloat <| digitToInt digit)
                , input.pow10Dec
                )
            else
                ( input.value + (toFloat <| digitToInt digit)*input.pow10Dec/10
                , input.pow10Dec / 10
                )
      in
          if input.digits == 0 then
            ModelInput  { input
                        | text   = digitToString digit
                        , digits = 1
                        , value  = toFloat <| digitToInt digit
                        }
          else
              let (val, pow10Dec) = newValueDeinputmals digit input
              in
                ModelInput  { input
                            | text      = input.text ++ digitToString digit
                            , digits    = input.digits + 1
                            , value     = val
                            , pow10Dec  = pow10Dec
                            }
    else
        ModelInput input

calculate: Model -> Model
calculate model =
    case model of
        ModelResult  result   ->
            ModelResult { result
                        | value = operate   result.value
                                            (fst result.lastOperation)
                                            (snd result.lastOperation)
                        }
        ModelResultInOperation  result   ->
            ModelResult { value = (fst result)
                        , lastOperation = (Sum, 0)
                        }
        ModelInput   input    ->
            ModelResult { value= operate    (fst input.currentOperation)
                                (snd input.currentOperation)
                                input.value
                        , lastOperation= (snd input.currentOperation, input.value)
                        }





getDisplay: Model -> Element
getDisplay model =
    let
        dispTxt: Model -> String
        dispTxt model =
            case model of
                ModelResult             result  ->  toString result.value
                ModelResultInOperation  result  ->  fst result |> toString
                ModelInput              input   ->  input.text
    in
        Text.fromString (dispTxt model)
            |> rightAligned
            |> width  ((widthOf <| btDigit D9) * 3)
