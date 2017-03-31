module Main exposing (..)

import Html exposing (Html, div, program)
import Login exposing (Login, LoginStatus)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { login : Login
    }


type Msg
    = UpdateLogin LoginStatus


init : ( Model, Cmd Msg )
init =
    ( Model Login.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin updateLogin ->
            let
                ( login_, effLogin ) =
                    Login.update updateLogin model.login
            in
                ( { model | login = login_ }, Cmd.map UpdateLogin effLogin )


view : Model -> Html Msg
view model =
    div [] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
