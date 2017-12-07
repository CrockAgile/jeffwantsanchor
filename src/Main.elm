module Main exposing (main)

import Html
import Dict
import Color exposing (..)
import Style exposing (style)
import Element exposing (textLayout, layout, row, column, text, underline, link, el, paragraph, image)
import Element.Attributes exposing (spacing, padding, paddingBottom, paddingTop, center, verticalCenter, height, width, maxWidth, px, fill, percent)
import Style.Scale
import Style.Color as Color
import Style.Font as Font
import Style.Background as Background
import Style.Shadow as Shadow


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
    | ActionCall
    | ShadowedWhite
    | ShadowedDark
    | ShadowedPurple
    | ActionList
    | AsSeenIn
    | About
    | Qualifier
    | Question
    | Answer


purple : Color.Color
purple =
    Color.rgb 78 64 169


teal : Color.Color
teal =
    Color.rgb 46 255 217


dark : Color.Color
dark =
    Color.rgb 41 47 54


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style NoStyle []
        , style Placeholder
            [ Font.size (fontScale 2)
            ]
        , style Page
            [ Font.typeface (fontStack SansSerif) ]
        , style Header
            [ Color.background purple
            , Color.text white
            ]
        , style ContestBanner
            [ Color.background teal
            , Color.text dark
            , Font.uppercase
            , Font.size (fontScale 1)
            , Font.weight 700
            , Shadow.box { offset = ( 0, 2 ), size = 2, blur = 0, color = Color.rgba 41 47 54 0.2 }
            ]
        , style Title
            [ Font.size (fontScale 3)
            ]
        , style ActionCall
            [ Background.gradient (pi * 3 / 4) [ Background.step (Color.rgb 71 161 255), Background.step (Color.rgb 99 120 253) ] ]
        , style ShadowedWhite
            [ Color.text white
            , Font.size (fontScale 3)
            , Font.weight 700
            , Shadow.text { offset = ( 0, 2 ), blur = 0, color = Color.rgba 0 0 0 0.2 }
            ]
        , style ShadowedDark
            [ Color.text dark
            , Font.size (fontScale 2)
            , Font.weight 700
            , Shadow.text { offset = ( 1, 3 ), blur = 0, color = white }
            ]
        , style ShadowedPurple
            [ Color.text purple
            , Font.size (fontScale 2)
            , Font.weight 700
            , Shadow.text { offset = ( 1, 3 ), blur = 0, color = white }
            ]
        , style ActionList
            [ Color.text teal
            , Font.size (fontScale 2)
            , Font.weight 700
            ]
        , style AsSeenIn
            [ Color.background white
            , Font.size (fontScale 2)
            , Color.text (Color.rgb 78 72 72)
            ]
        , style About
            [ Color.background teal
            ]
        , style Qualifier []
        , style Question
            [ Font.bold ]
        , style Answer []
        ]


fontScale : Int -> Float
fontScale =
    Style.Scale.roundedModular 18 1.618


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


email : String
email =
    "crockagile@gmail.com"


mailto : String
mailto =
    "mailto:" ++ email


view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        column Page
            [ center, width fill, height fill ]
            [ header
            , actionCall
            , asSeenIn
            , about
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
        (link mailto <|
            (el NoStyle [ padding 6, center ] (text "Hire Jeff Crocker.  Find out how💰"))
        )


actionCall : Element.Element Styles variation Msg
actionCall =
    column ActionCall
        [ width fill, center, spacing 40, paddingBottom 60 ]
        [ contestBanner
        , Element.h2 ShadowedWhite [] (text "Writes code, right from your office.")
        , Element.h3 ActionList [] (text "Get products, cooperation, and passion, all for one salary.")
        , Element.paragraph ActionList
            []
            [ el NoStyle [ padding 6 ] (text "Already have engineers?")
            , link mailto <| (underline "Great, I work better in teams.")
            ]
        ]


asSeenIn : Element.Element Styles variation Msg
asSeenIn =
    column AsSeenIn
        [ spacing 20, center, paddingTop 25, paddingBottom 25 ]
        [ Element.h4 NoStyle
            []
            (text "As seen in...")
        , Element.wrappedRow
            NoStyle
            [ width fill, spacing 40 ]
            (List.map
                asSeenInLogo
                [ ( "polysync.png", "PolySync" )
                , ( "souperseconds.png", "Souper Seconds" )
                , ( "clearwater.png", "Clearwater Analytics" )
                , ( "camporkila.jpg", "Camp Orkila" )
                ]
            )
        ]


asSeenInLogo : ( String, String ) -> Element.Element Styles variation Msg
asSeenInLogo ( src, company ) =
    image NoStyle [ height (px 100) ] { src = src, caption = company ++ " Logo" }


about : Element.Element Styles variation Msg
about =
    row About
        [ width fill, center, padding 30 ]
        [ column NoStyle
            [ maxWidth (px 780), width fill ]
            [ Element.h2 ShadowedWhite [ padding 30 ] (text "About me")
            , textLayout ShadowedDark
                [ padding 20 ]
                [ paragraph NoStyle
                    []
                    [ text "Raised and taught in the Pacific Northwest, I am on a mission to make your product into reality."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "I am an enthusiastic engineer seeking to pin my curiosity and creativity against challenging problems."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "I know that any obstacle can be overcome with the ingenuity of an open-minded team."
                    ]
                , paragraph NoStyle
                    [ paddingTop 20 ]
                    [ text "Want to work together? Email"
                    ]
                , paragraph ShadowedPurple
                    []
                    [ link mailto <| (text email) ]
                ]
            ]
        ]


q_and_a : String -> String -> Element.Element Styles variation Msg
q_and_a question answer =
    paragraph Qualifier
        []
        [ el Question [ padding 15 ] (text question)
        , el Answer [] (text answer)
        ]
