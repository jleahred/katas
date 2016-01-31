import Html exposing (ol, li, text, ul, Html, button)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Effects exposing (Effects, Never)
import Task exposing (Task, andThen)
import StartApp as StartApp

import Json.Decode as Json exposing ((:=))




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


port tasks: Signal (Task Never ())
port tasks =
    app.tasks

init: (Model, Effects Action)
init =
    (emptyModel, fetchStatus)


decodeLeaf: Json.Decoder LeafInfo
decodeLeaf =
    Json.object4 LeafInfo
      ("text" := Json.string)
      ("status" := Json.string)
      ("expanded" := Json.bool)
      ("id" := Json.string)

{--
addLeaf: Maybe LeafInfo -> Model
addLeaf maybeLeaf =
    case maybeLeaf of
        Just leaf -> testModel
        Nothing -> []
--}

fetchStatus: Effects Action
fetchStatus =
    let
        resquest =
            Http.get decodeLeaf "http://127.0.0.1:8000/status.json"
            |> Task.map Testing
            --|> Testing
            --|> Effects.task
    in
        resquest
            `Task.onError` (\err -> Task.succeed (Err <| toString err))
            --`Task.andThen` (\action -> Effects.task action)
            |> Effects.task

{--
fetchStatus: Effects Action
fetchStatus =
  --Http.get (Json.list decodeLeaf) "http://localhost:8000/status.json"
  (Http.get (decodeLeaf) "http://127.0.0.1:8000/status.json")
    {--|> Task.toMaybe
    |> Task.map (\l -> ModelLoaded (addLeaf l))
    |> Effects.task
    --}
    `Task.onError` (\msg -> Err "kljlkj")
    --|> Task.toResult
    --`Task.onError` \msg -> Err msg
    |> Task.map (\l -> ModelLoaded l)
    |> Effects.task
    --|> Task.map Ok task `Task.onError` \msg -> Task.succeed (Err msg)


    {--
    `Task.onError` (\msg -> Task.succeed ({ text="text", expanded=True, id="id", status="OK" }))
    --`Task.andThen` (\msg -> Task.succeed ({ text="text", expanded=True, id="id", status="OK" }))
    |> Task.map (\l -> ModelLoaded [])
    |> Effects.task
    --}

    {--`Task.onError` (\msg -> Task.succeed ({ text="text", expanded=True, id="id", status="OK" }))
    `Task.andThen` (\msg -> Task.succeed ({ text="text", expanded=True, id="id", status="OK" }))
    |> Task.map (\l -> ModelLoaded [])
    --`Task.andThen` \resp -> testModel
    |> Effects.task
    --}
--}


--port runner : Task Http.Error  (List LeafInfo)
--port runner : Task Http.Error (Signal Action)
--port runner =
--  getJsonStatus `andThen` (testModel Signal.send )
  --(LoadedLeafInfos >> Signal.send NoOp)

{--
init =
    (testModel, Effects.none)
--}


-- MODEL

type Model
  = StatusTree (List SubTree)
  | Error String



type alias StatusTree
  = List SubTree


type SubTree
  = Node LeafInfo StatusTree


type Status = OK | WARNING | ERROR

type alias LeafInfo =
  { text : String
  , status : String
  , expanded : Bool
  , id : String
  }


emptyModel: Model
emptyModel = StatusTree []


-- UPDATE

type Action
    = ToggleSection String
    | Testing LeafInfo
    | Err String

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        ToggleSection id    ->  (StatusTree (toggleTree id model), Effects.none)
        --Testing leafInfo    ->  (testModel, Effects.none)
        Err     error       ->  (testModel, Effects.none)
        _ -> (testModel, Effects.none)





-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    case model of
        StatusTree  st  -> Html.text "pending to parse tree"
        Error  error  -> Html.text error

{--
view : Signal.Address Action -> Model -> Html
view address model =
    case model of
        (Node leafInfo listStatusTree) ->
            ul [class "node"]  <| List.map (statusTreeToHtml address) listStatusTree
        Error  error  -> Html.text error
--}

{--
statusTreeToHtml: Signal.Address Action -> StatusTree -> Html
statusTreeToHtml address listStatusTree =
  let
    genLeaf address leafInfo =
      li [] [button [onClick address (ToggleSection leafInfo.id)] [text leafInfo.text] ]
  in
    case (Node LeafInfo listStatusTree, leafInfo.expanded) of
      ([], _)     -> genLeaf address leafInfo
      (lst, True) -> Html.span[]
        [ genLeaf address leafInfo
        , ul [class "node"] <| List.map (statusTreeToHtml address) listStatusTree
        ]
      (_, _)     -> genLeaf address leafInfo
--}



-- SUPPORT

toggleTree : String -> StatusTree -> StatusTree
toggleTree id tree =
  let
    toggleLeafInfo li =
      { li
      | expanded = not li.expanded
      }
    toogleSubTree id (Node li st) =
      if li.id == id then
        Node (toggleLeafInfo li) st
      else
        Node li (toggleTree id st)
  in
    List.map (toogleSubTree id) tree













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
    node text status expanded id = Node { text=text, expanded=expanded, id=id, status=status }
    nodeExp text id = Node { text=text, expanded=True, id=id, status="OK" }
    nodeColap text id = Node { text=text, expanded=False, id=id, status="OK" }
  in
    StatusTree
    [ nodeExp "testing1" "1"
      [ nodeExp "testing11" "11" []
      , nodeExp "testing12" "12"
        [ nodeExp "testing121" "121" []
        , nodeExp "testing122" "122" []
        ]
      ]
    , nodeExp "testing2" "2"
      [ nodeExp "testing21" "21" []
      , nodeExp "testing22" "22" []
      , nodeExp "testing23" "23" []
      ]
    ]
