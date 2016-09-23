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



--send : String -> Task Error Int
--send c =
--    let
--        payload =
--            En.object
--                [ ( "cookie", En.string c )
--                ]
--    in
--        Http.fromJson (at [ "status" ] int) <|
--            Http.send Http.defaultSettings
--                { verb = "POST"
--                , headers = [ ( "Content-Type", "application/json" ) ]
--                , url = "http://localhost:3000/test_reply"
--                , body = Http.string <| En.encode 0 payload
--                }
--send_message : String -> Cmd Msg
--send_message cookie =
--    perform Fail SendSucceed <| send cookie


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
