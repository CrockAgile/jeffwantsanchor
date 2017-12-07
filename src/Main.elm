module Main exposing (main)

import Html
import Color exposing (..)
import Style exposing (style)
import Element exposing (textLayout, layout, row, column, text, link, el, paragraph)
import Element.Attributes exposing (spacing, padding, center, verticalCenter, height, width, maxWidth, px, fill)
import Style.Scale
import Style.Color as Color
import Style.Font as Font


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Int


init : ( Model, Cmd msg )
init =
    ( 0, Cmd.none )


type Msg
    = Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Styles
    = NoStyle
    | Placeholder
    | Page
    | Header
    | Title
    | ContestBanner
    | Qualifications
    | Qualifier
    | Question
    | Answer


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style NoStyle []
        , style Placeholder
            [ Font.size (fontScale 2)
            ]
        , style Page
            []
        , style Header
            [ Color.background (Color.rgb 78 64 169)
            , Color.text white
            ]
        , style ContestBanner
            [ Color.background (Color.rgb 46 255 217)
            , Color.text (Color.rgb 41 47 54)
            , Font.weight 700
            , Font.uppercase
            , Font.size 18
            ]
        , style Title
            [ Font.size (fontScale 3)
            , Font.typeface (fontStack SansSerif)
            ]
        , style Qualifications
            []
        , style Qualifier []
        , style Question
            [ Font.bold ]
        , style Answer []
        ]


fontScale : Int -> Float
fontScale =
    Style.Scale.modular 16 1.618


type FontStack
    = SansSerif
    | Serif
    | Mono


fontStack : FontStack -> List Style.Font
fontStack stack =
    case stack of
        SansSerif ->
            List.map Font.font [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ]

        Serif ->
            List.map Font.font [ "Garamond", "serif" ]

        Mono ->
            List.map Font.font [ "Inconsolata", "monospace" ]


view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        column Page
            [ center, width fill ]
            [ header
            , contestBanner
            , Element.textLayout Qualifications
                [ width fill ]
                [ (q_and_a "Qualified?" "Does it reload?")
                ]
            ]


header : Element.Element Styles variation Msg
header =
    Element.header Header
        []
        (row NoStyle
            [ center, verticalCenter, width fill, height (px 85) ]
            [ Element.h1 Title [] (text "Jeff Wants Anchor")
            ]
        )


contestBanner : Element.Element Styles variation Msg
contestBanner =
    el ContestBanner
        [ width fill, center ]
        (link "mailto:crockagile@gmail.com" <|
            (el NoStyle [ padding 6, center ] (text "Hire Jeff Crocker.  Find out how💰"))
        )


q_and_a : String -> String -> Element.Element Styles variation Msg
q_and_a question answer =
    paragraph Qualifier
        []
        [ el Question [ padding 15 ] (text question)
        , el Answer [] (text answer)
        ]
