module ActionBar exposing (..)

import CSS exposing (actionPanel, floatLeft, floatRight)
import Html exposing (Html, button, div, textarea, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { message : String
    , action : String
    , visible : String
    }


type Msg
    = Send
    | Content String
    | Visible String


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "hidden", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send ->
            ( model, Cmd.none )

        Content input ->
            ( { model | message = input }, Cmd.none )

        Visible value ->
            ( { model | visible = value }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ actionPanel, style [ ( "visibility", model.visible ) ] ]
        [ button [ floatRight, onClick Send ] [ text "send" ]
        , textarea [ floatLeft, style [ ( "width", "90%" ) ], onInput Content ] []
        ]
