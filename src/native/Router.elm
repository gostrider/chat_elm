module Router exposing (..)

import Navigation exposing (Location, makeParser)
import String exposing (dropLeft)
import UrlParser exposing (Parser, format, oneOf, parse, s)


type Route
    = RouteMain
    | RouteNothing


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ format RouteMain (s "main")
        , format RouteNothing (s "login")
        , format RouteNothing (s "")
        ]


hashParser : Location -> Result String Route
hashParser location =
    location.hash
        |> dropLeft 1
        |> parse identity matchRoute


parser : Navigation.Parser (Result String Route)
parser =
    makeParser hashParser


routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route

        Err err ->
            RouteNothing
