import Html exposing (ol, li, text, ul, Html, button)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Http
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)
import StartApp as StartApp
--import Graphics.Element exposing (show)   --debug
import Time

import Json.Decode as Json exposing ((:=))






-- MODEL

type Model
  = ModelLoaded     Status
  | ModelMessage    String


type alias  Status =
  { statusTree    : List NodeInfo
  , expandedItems : List String
  }

type NodeInfo =
  NodeInfo
  { text    : String
  , status  : NodeStatus
  , id      : String
  , childs  : List NodeInfo
  }


type NodeStatus = OK | WARNING | ERROR


initModel: Model
initModel =
  ModelMessage  "Initializing..."




--  MAIN


main : Signal Html
main =
  app.html

app: StartApp.App Model
app =
  StartApp.start
  { init = (init, Effects.none)
  , update = update
  , view = view
  , inputs = [Signal.map JsonLoaded readingsMailbox2.signal]
  }



init: Model
init = initModel



-- UPDATE

type Action
  = ToggleSection String
  | JsonLoaded    Model



update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    JsonLoaded    nwModel -> (copyExpaned2Model nwModel model,  Effects.none)
    ToggleSection id      -> (toggleId model id,                Effects.none)





-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.body
    [ style [fontStyle] ]
    [ case model of
      ModelLoaded  status  ->
        Html.div []
        [ listNodesToHtml address status.expandedItems status.statusTree
        --, Html.fromElement <| show model    --  debug
        ]
      ModelMessage error   ->
        Html.text error
    ]


listNodesToHtml: Signal.Address Action -> List String -> List NodeInfo  -> Html
listNodesToHtml address toggledIds treeList =
  Html.ul[]
    (List.map (nodeToHtml address toggledIds) treeList)


nodeToHtml: Signal.Address Action -> List String -> NodeInfo ->  Html
nodeToHtml address toggledIds (NodeInfo nodeInfo) =
  if List.length nodeInfo.childs==0 || not (List.member nodeInfo.id toggledIds)
  then
    nodeInfoToHtml address nodeInfo
  else
    Html.span[]
      <| [nodeInfoToHtml address nodeInfo]
      ++ [listNodesToHtml address toggledIds nodeInfo.childs]




-- EFFECTS

port tasks: Signal (Task Never ())
port tasks =
  app.tasks


lazy: (() -> Json.Decoder a) -> Json.Decoder a
lazy thunk =
  Json.customDecoder Json.value
    (\js -> Json.decodeValue (thunk ()) js)


decodeTree: Json.Decoder NodeInfo
decodeTree =
  Json.object4 --JsonNodeInfo
    (\t s i c -> NodeInfo {text=t, status=s, id=i, childs=c})
    ("text" := Json.string)
    ("status" := Json.string |> stringToNodeStatus)
    ("id" := Json.string)
    ("childs" := Json.list (lazy (\_-> decodeTree)))


stringToNodeStatus: Json.Decoder String -> Json.Decoder NodeStatus
stringToNodeStatus d =
  Json.customDecoder d  (\s-> Ok <|
                          case s of
                              "OK"      -> OK
                              "WARNING"  -> WARNING
                              _         -> ERROR
                        )


getStatusFromServer: Task a ()
getStatusFromServer =
  let
    modelFromLTree lt =
      ModelLoaded
      { statusTree    = lt
      , expandedItems = []
      }
    resquest =
      Http.post (Json.list decodeTree) "status.json" Http.empty
      --Http.get (Json.list decodeTree) "http://127.0.0.1:8000/status.json"
      --Http.get (Json.list decodeTree) "http://100.100.16.64:8000/status.json"
      |> Task.map modelFromLTree
  in
    resquest
      `Task.onError`
          (\err -> Task.succeed (ModelMessage <| toString err))
      `Task.andThen`
          (\m -> Signal.send readingsMailbox2.address m)

port periodicTasks : Signal (Task () ())
port periodicTasks = Signal.map (\_ -> getStatusFromServer)
                                <|  Time.every (2*Time.second)

readingsMailbox2 : Signal.Mailbox Model
readingsMailbox2 = Signal.mailbox initModel








--  SUPPORT

copyExpaned2Model: Model -> Model -> Model
copyExpaned2Model nwModel prevModel =
  case nwModel of
    ModelMessage _      ->
                  nwModel
    ModelLoaded status  ->
                  ModelLoaded
                  { status
                  | expandedItems = getExpList prevModel}

getExpList: Model -> List String
getExpList model =
  case model of
    ModelMessage _      -> []
    ModelLoaded status  -> status.expandedItems


nodeInfoToHtml: Signal.Address Action
                -> { a | id : String
                   , text : String
                   , childs: List NodeInfo
                   , status: NodeStatus
                   }
                -> Html
nodeInfoToHtml address nodeInfo=
  let
    listStyle =
      style [("list-style", "none")]
    buttonStyle =
      style [ ("border", "none")
            , ("background", "none")
            , fontStyle]
    styleLI nChilds=
      if nChilds == 0 then
          [("color", "transparent")]
      else
          []
    listIndicator ni =
      Html.span [ style <| (styleLI <| List.length nodeInfo.childs)
                        ++ [("margin-left", "7px")]
                ]
                [ text " · "]
    statusColor status =
      case status of
        OK       -> "green"
        WARNING  -> "orange"
        _        -> "red"
    circleStatus status =
      Html.div [ style [ ("border-radius", "30px")
                       , ("margin-top", "3px")
                       , ("width", "15px")
                       , ("height", "15px")
                       , ("background", statusColor status)
                       , ("color", "transparent")
                       , ("float", "left")
                       ]
                ]
                [ text "_"]
  in
    li [listStyle]
     [ button [ onClick address (ToggleSection nodeInfo.id)
             , buttonStyle
             ]
             [ circleStatus  nodeInfo.status
             , listIndicator nodeInfo
             , text <| nodeInfo.text
             ]
     ]


toggleId: Model -> String -> Model
toggleId model id =
  case model of
    ModelLoaded status ->
      ModelLoaded
      { status
      | expandedItems
          = if not (List.member id status.expandedItems) then
              status.expandedItems ++ [id]
            else
              List.filter (\i -> i/=id)  status.expandedItems
      }
    _ ->
      ModelMessage "Error, received toggle with invalid status tree"



fontStyle : (String, String)
fontStyle = ("font-size", "1.1em")






--  TESTING


  {--  the view will generate something like...
   ul [class "market1"]
    [ li [] [text "market1"]
    , ul [class "submarket1"]
      [ li [] [text "prices"]
      , li [] [text "trading"]
      , li [] [text "loading"]
      , li [] [text "misc"]
      ]
    , li [] [text "market2"]
    , ul [class "submarket1"]
      [ li [] [text "prices"]
      , li [] [text "trading"]
      , li [] [text "loading"]
      , li [] [text "misc"]
      ]
    ]
  --}

testModel : Model
testModel
    =
    let
        node text id childs = NodeInfo { text=text, id=id, status=OK, childs=childs }
    in
    ModelLoaded
    { statusTree =
        [ node "testing1" "1"
          [ node "testing11" "11" []
          , node "testing12" "12"
            [ node "testing121" "121" []
            , node "testing122" "122" []
            ]
          ]
        , node "testing2" "2"
          [ node "testing21" "21" []
          , node "testing22" "22" []
          , node "testing23" "23" []
          ]
        ]
    , expandedItems = []
    }
