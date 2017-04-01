module Controllers.Login exposing (..)

-- Core

import Html exposing (..)
import Html.Attributes exposing (align, type_, style)
import Html.Events exposing (onClick, onInput)


-- Data model

import Models.Login exposing (AuthStatus)
import Services.LoginService as LoginService


type alias Login =
    { username : String
    , password : String
    , user : LoginService.Authentication
    }


type LoginAction
    = Auth
    | Username String
    | Password String
    | UpdateLoginService AuthStatus


init : Login
init =
    Login "" "" LoginService.init


update : LoginAction -> Login -> ( Login, Cmd LoginAction )
update action login =
    case action of
        Username username_ ->
            ( { login | username = username_ }, Cmd.none )

        Password password_ ->
            ( { login | password = password_ }, Cmd.none )

        Auth ->
            let
                effLoginService =
                    LoginService.verify (LoginService.formPayload login.username login.password)
            in
                ( login, Cmd.map UpdateLoginService effLoginService )

        UpdateLoginService loginService_ ->
            let
                ( loginServiceUpdate, effLoginService ) =
                    LoginService.update loginService_ login.user
            in
                ( { login | user = loginServiceUpdate }, Cmd.map UpdateLoginService effLoginService )


view : Login -> Html LoginAction
view login =
    div [ align "center" ]
        [ h1 [] [ text "Title" ]
        , br [] []
        , text "Username: "
        , input [ onInput Username, type_ "text" ] []
        , br [] []
        , text "Password: "
        , input [ onInput Password, type_ "password" ] []
        , br [] []
        , div []
            [ text (toString login.user.status)
            , button [ onClick Auth ] [ text "Login" ]
            ]
        ]



--makeLogin : String -> LoginService.Authentication -> Login
--makeLogin status login =
--    Login "" "" status login
