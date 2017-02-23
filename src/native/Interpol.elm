port module Interpol exposing (..)

import Json.Decode as De
    exposing
        ( Decoder
        , at
        , andThen
        , decodeString
        , int
        , list
        , string
        )
import Navigation exposing (newUrl)


type alias Model =
    { interpol : Delivery
    }


type alias RespStr =
    String


type alias RespAuth =
    { id : String
    , uuid : String
    , session : String
    }


type Msg
    = Return String


type Delivery
    = ResponseString RespStr
    | ResponseAuthenticate RespAuth


port store : String -> Cmd msg


port reply : (String -> msg) -> Sub msg


port get : String -> Cmd msg


port remove : String -> Cmd msg


init : Model
init =
    Model << ResponseString <| ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Return interpolated ->
            let
                interpolDecode =
                    interpolated
                        |> De.decodeString responseDecoder
                        |> Result.withDefault (ResponseString "err")
            in
                ( { model | interpol = interpolDecode }
                , Cmd.none
                )


stringDecoder : Decoder Delivery
stringDecoder =
    De.map ResponseString
        (at [ "response" ] string)


authDecoder : Decoder Delivery
authDecoder =
    De.map ResponseAuthenticate
        (De.map3 RespAuth
            (at [ "response", "id" ] string)
            (at [ "response", "uuid" ] string)
            (at [ "response", "session" ] string)
        )


packageDecoder : String -> Decoder Delivery
packageDecoder event =
    case event of
        "get" ->
            authDecoder

        "post" ->
            stringDecoder

        "error" ->
            stringDecoder

        _ ->
            De.fail "unknown_event"


responseDecoder : Decoder Delivery
responseDecoder =
    at [ "event" ] string |> andThen packageDecoder


interpolLogin : Delivery -> ( String, RespAuth )
interpolLogin delivery =
    case delivery of
        ResponseAuthenticate val ->
            ( "succeed", val )

        ResponseString _ ->
            ( "", RespAuth "" "" "" )
