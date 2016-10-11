module UrlParse exposing (..)

import Navigation
import String


toUrl : String -> String
toUrl route =
    "#/" ++ route


fromUrl : String -> String
fromUrl route =
    String.dropLeft 2 route


urlParser : Navigation.Parser String
urlParser =
    Navigation.makeParser (fromUrl << .hash)
