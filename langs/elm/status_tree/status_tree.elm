import Html exposing (ol, li, text, ul, Html, button)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)
import StartApp as StartApp
import Graphics.Element exposing (show)

import Json.Decode as Json exposing ((:=))





-- MODEL

type Model
  = ModelLoaded     Status
  | ModelMessage    String



type alias  Status =
    { statusTree    : List Tree
    , expandedItems : List String
    }


type Tree
  = Node NodeInfo (List Tree)



type alias NodeInfo =
  { text : String
  , status : String
  , id : String
  }


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
    { init = init
    , update = update
    , view = view
    , inputs = []
    }



init: (Model, Effects Action)
init =
    (initModel, fetchStatus)






-- UPDATE

type Action
    = ToggleSection String
    | Loaded        (List NodeInfo)--(List Tree)
    | ErrorJson     String

update: Action -> Model -> (Model, Effects Action)
update action model =
    let
        modelFromLTree lt =
            ModelLoaded
            { statusTree    = lt
            , expandedItems = []
            }
    in
        case action of
            --Loaded listTree     -> (modelFromLTree listTree, Effects.none)
            Loaded nodeInfo -> (testModel, Effects.none)
            ToggleSection id    -> (toggleId model id, Effects.none)
            ErrorJson  error    ->
                (ModelMessage <| "Error loading json: " ++ error, Effects.none)





-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    case model of
        ModelLoaded  status  ->
            Html.div []
            [ treeToHtml address status.expandedItems status.statusTree
            , Html.fromElement <| show model
            ]
        ModelMessage error   ->
            Html.text error

treeToHtml: Signal.Address Action -> List String -> List Tree  -> Html
treeToHtml address toggledIds tree =
    Html.ul[] (List.map (nodeToHtml address toggledIds) tree)

nodeToHtml: Signal.Address Action -> List String -> Tree ->  Html
nodeToHtml address toggledIds (Node nodeInfo listSubtree) =
    let
        nodeInfoToHtml address =
            li [] [button [onClick address (ToggleSection nodeInfo.id)] [text nodeInfo.text] ]
    in
        if List.length listSubtree==0
            ||  not (List.member nodeInfo.id toggledIds)
            then
            nodeInfoToHtml address
        else
            Html.span[] <| [nodeInfoToHtml address]
                ++ [treeToHtml address toggledIds listSubtree]




-- EFFECTS

port tasks: Signal (Task Never ())
port tasks =
    app.tasks

decodeTree: Json.Decoder NodeInfo
decodeTree =
    Json.object3 NodeInfo
      ("text" := Json.string)
      ("status" := Json.string)
      ("id" := Json.string)
      --("childs" := Json.list)



fetchStatus: Effects Action
fetchStatus =
    let
        resquest =
            Http.get (Json.list decodeTree) "http://127.0.0.1:8000/status.json"
            |> Task.map Loaded
    in
        resquest
            `Task.onError` (\err -> Task.succeed (ErrorJson <| toString err))
            |> Effects.task


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
