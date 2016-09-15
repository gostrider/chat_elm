module ChatItem exposing (..)

import Html exposing (..)
import Html.App as App


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias ChatInfo =
    { sender : String
    , avatar : String
    , message_body : String
    }


type Msg
    = Show


init : ( ChatInfo, Cmd Msg )
init =
    ( ChatInfo "sender" "url" "string", Cmd.none )


subscriptions : ChatInfo -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> ChatInfo -> ( ChatInfo, Cmd Msg )
update msg model =
    case msg of
        Show ->
            ( model, Cmd.none )


view : ChatInfo -> Html Msg
view model =
    div []
        [ text model.sender
        , br [] []
        , text model.message_body
        , br [] []
        , text model.avatar
        ]
