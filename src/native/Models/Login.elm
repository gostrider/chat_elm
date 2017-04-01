module Models.Login exposing (..)

import Models.User exposing (User)
import Http exposing (Error)


type Status
    = LoginSucceed
    | LoginFailed


type AuthStatus
    = Succeed User
    | Fail Error
