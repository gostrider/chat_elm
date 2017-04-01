module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (align, type_, style)
import Html.Events exposing (onClick, onInput)
import Services.LoginService as LoginService


type alias Login =
    { username : String
    , password : String
    , user : LoginService.Authentication
    }


type LoginStatus
    = Auth
    | Username String
    | Password String
    | UpdateLoginService LoginService.AuthStatus


init : Login
init =
    Login "" "" LoginService.init


update : LoginStatus -> Login -> ( Login, Cmd LoginStatus )
update msg model =
    case msg of
        Username username_ ->
            ( { model | username = username_ }, Cmd.none )

        Password password_ ->
            ( { model | password = password_ }, Cmd.none )

        Auth ->
            let
                effLoginService =
                    LoginService.verify (LoginService.formPayload model.username model.password)
            in
                ( model, Cmd.map UpdateLoginService effLoginService )

        UpdateLoginService loginService_ ->
            let
                ( loginServiceUpdate, effLoginService ) =
                    LoginService.update loginService_ model.user
            in
                ( { model | user = loginServiceUpdate }, Cmd.map UpdateLoginService effLoginService )


view : Login -> Html LoginStatus
view model =
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
            [ text (toString model.user.status)
            , button [ onClick Auth ] [ text "Login" ]
            ]
        ]



--makeLogin : String -> LoginService.Authentication -> Login
--makeLogin status login =
--    Login "" "" status login
