module Router exposing (..)

import Navigation exposing (Location)
import String exposing (dropLeft)
import UrlParser exposing (Parser, map, oneOf, parseHash, s)


type Route
    = RouteMain
    | RouteNothing


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map RouteMain (s "main")
        , map RouteNothing (s "login")
        , map RouteNothing (s "")
        ]


hashParser : Location -> Result String Route
hashParser location =
    location.hash
        |> dropLeft 1
        |> parseHash identity matchRoute


routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route

        Err err ->
            RouteNothing
