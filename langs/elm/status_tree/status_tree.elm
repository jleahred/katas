import Html exposing (ol, li, text, ul, Html, button)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)
import StartApp as StartApp

import Json.Decode as Json exposing ((:=))





-- MODEL

type Model
  = ModelLoaded  Status
  | ModelError   String



type alias  Status =
    { statusTree    : List Tree
    , expandedItems : List String
    }


type Tree
  = Node NodeInfo (List Tree)


--type Status = OK | WARNING | ERROR

type alias NodeInfo =
  { text : String
  , status : String
  , id : String
  }


emptyModel: Model
emptyModel =
    ModelLoaded
        { statusTree      = []
          , expandedItems   = []
        }





--  MAIN


main : Signal Html
main =
  app.html

app: StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }



init: (Model, Effects Action)
init =
    (emptyModel, fetchStatus)






-- UPDATE

type Action
    = ToggleSection String
    | Loaded        (List NodeInfo)
    | ErrorJson     String

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Loaded nodeInfoList ->  (testModel, Effects.none)
        ToggleSection id    ->  (testModel, Effects.none)--(StatusTree (toggleTree id model), Effects.none)
        --Testing nodeInfo    ->  (testModel, Effects.none)
        ErrorJson  error    ->
            (ModelError <| "Error loading json: " ++ error, Effects.none)
                --(testModel, Effects.none)





-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    case model of
        ModelLoaded status  -> treeToHtml address status.statusTree
        ModelError  error   -> Html.text error

treeToHtml: Signal.Address Action -> List Tree -> Html
treeToHtml address tree =
    Html.ul[] (List.map (nodeToHtml address) tree)

nodeToHtml: Signal.Address Action -> Tree -> Html
nodeToHtml address (Node nodeInfo listSubtree) =
--statusToHtml
--statusToHtml address (Node nodeInfo listSubtree) =
    let
        nodeInfoToHtml address =
            li [] [button [{--onClick address (ToggleSection nodeInfo.id)--}] [text nodeInfo.text] ]
    in
        case listSubtree of
            []      ->  nodeInfoToHtml address
            _       ->  Html.ul[] <| [nodeInfoToHtml address]
                                        ++ [treeToHtml address listSubtree]
            --ul [class "node"] <| List.map (statusTreeToHtml address) listStatusTree
    --ul [class "node"] <| List.map (statusTreeToHtml address) listStatusTree

{--statusLstToHtml: Signal.Address Action -> List Status -> Html
statusLstToHtml address listStatus =
    let
        genNode address (Node nodeInfo list =
            li [] [button [onClick address (ToggleSection nodeInfo.id)] [text nodeInfo.text] ]
    in
        List.map (genNode address)  listStatus
--}
{--
view : Signal.Address Action -> Model -> Html
view address model =
    case model of
        (Node nodeInfo listStatusTree) ->
            ul [class "node"]  <| List.map (statusTreeToHtml address) listStatusTree
        Error  error  -> Html.text error
--}

{--
statusTreeToHtml: Signal.Address Action -> StatusTree -> Html
statusTreeToHtml address listStatusTree =
  let
    genNode address nodeInfo =
      li [] [button [onClick address (ToggleSection nodeInfo.id)] [text nodeInfo.text] ]
  in
    case (Node NodeInfo listStatusTree, nodeInfo.expanded) of
      ([], _)     -> genNode address nodeInfo
      (lst, True) -> Html.span[]
        [ genNode address nodeInfo
        , ul [class "node"] <| List.map (statusTreeToHtml address) listStatusTree
        ]
      (_, _)     -> genNode address nodeInfo
--}


-- EFFECTS

port tasks: Signal (Task Never ())
port tasks =
    app.tasks

decodeNode: Json.Decoder NodeInfo
decodeNode =
    Json.object3 NodeInfo
      ("text" := Json.string)
      ("status" := Json.string)
      ("id" := Json.string)

{--
addNode: Maybe NodeInfo -> Model
addNode maybeNode =
    case maybeNode of
        Just node -> testModel
        Nothing -> []
--}

fetchStatus: Effects Action
fetchStatus =
    let
        resquest =
            Http.get (Json.list decodeNode) "http://127.0.0.1:8000/status.json"
            |> Task.map Loaded
            --|> Testing
            --|> Effects.task
    in
        resquest
            `Task.onError` (\err -> Task.succeed (ErrorJson <| toString err))
            --`Task.andThen` (\action -> Effects.task action)
            |> Effects.task



-- SUPPORT
{--
toggleTree : String -> List Tree -> List Tree
toggleTree id tree =
  let
    toggleNodeInfo li =
      { li
      | expanded = not li.expanded
      }
    toogleTree id (Node li st) =
      if li.id == id then
        Node (toggleNodeInfo li) st
      else
        Node li (toggleTree id st)
  in
    List.map (toogleTree id) tree
--}







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
        --node text status id = Node { text=text, id=id, status=status }
        node text id = Node { text=text, id=id, status="OK" }
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
