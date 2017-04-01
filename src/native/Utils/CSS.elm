module Utils.CSS exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


leftPanel : Attribute msg
leftPanel =
    style
        [ ( "float", "left" )
        , ( "width", "30%" )
        , ( "height", "500px" )
        , ( "overflow", "auto" )
        , ( "background-color", "gray" )
        ]


rightPanel : Attribute msg
rightPanel =
    style
        [ ( "float", "right" )
        , ( "width", "70%" )
        , ( "top", "0px" )
        ]


messagePanel : Attribute msg
messagePanel =
    style
        [ ( "height", "500px" )
        , ( "overflow", "auto" )
        , ( "background-color", "gainsboro" )
        ]


actionPanel : Attribute msg
actionPanel =
    style
        [ ( "buttom", "0px" )
        , ( "height", "20px" )
        ]


floatLeft : Attribute msg
floatLeft =
    style
        [ ( "float", "left" ) ]


floatRight : Attribute msg
floatRight =
    style
        [ ( "float", "right" ) ]
